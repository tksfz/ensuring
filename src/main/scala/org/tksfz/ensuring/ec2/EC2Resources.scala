package org.tksfz.ensuring.ec2

import java.util.concurrent.CompletionException

import cats.effect.{ContextShift, IO, Timer}
import org.tksfz.ensuring.{Ensure, Except, State}
import software.amazon.awssdk.services.ec2.Ec2AsyncClient
import software.amazon.awssdk.services.ec2.model.{AttributeValue, DescribeInstancesRequest, DescribeInstancesResponse, Ec2Exception, Filter, Instance, InstanceState, InstanceType, ModifyInstanceAttributeRequest, ResourceType, RunInstancesRequest, StartInstancesRequest, StopInstancesRequest, Tag, TagSpecification}

import scala.jdk.CollectionConverters._
import scala.jdk.FutureConverters._

class EC2Resources(ec2Client: Ec2AsyncClient) {
  def ec2(imageId: String, tag: (String, String))(implicit cs: ContextShift[IO]): EC2Ensure = {
    val request = DescribeInstancesRequest.builder()
      .filters(
        Filter.builder().name(s"tag:${tag._1}").values(tag._2).build(),
        // exclude terminated instances only
        Filter.builder().name("instance-state-name").values(Seq("pending", "running", "shutting-down", "stopping", "stopped").asJava).build(),
      )
      .build()
    EC2Ensure(ec2Client,
      RunInstancesRequest.builder()
        .imageId(imageId)
        .minCount(1)
        .maxCount(1)
        .tagSpecifications(TagSpecification.builder()
          .resourceType(ResourceType.INSTANCE)
          .tags(Tag.builder().key(tag._1).value(tag._2).build()).build()),
      Ensure.find {
        IO.fromFuture(IO(ec2Client.describeInstances(request).asScala))
          .map(_.reservations().asScala.flatMap(_.instances.asScala).headOption)
      }
        .subcondition { i =>
          Ensure.equal(i.imageId(), imageId)
        }
    )
  }
}

/**
  * @param ec2Client TODO this may end up getting pushed down into Kleisli
  * @param rir the RunInstancesRequest to use on recovery: Accumulates instance options (instance type, etc.)
  */
case class EC2Ensure(ec2Client: Ec2AsyncClient, rir: RunInstancesRequest.Builder, ens: Ensure[Instance])(implicit cs: ContextShift[IO]) extends Ensure[Instance] {
  def ensure: IO[State[Instance]] = {
    ens
      .recover {
        // TODO: instanceId can't be our param because the creation below won't produce the "desired" instanceId
        IO.fromFuture(IO(ec2Client.runInstances(rir.build()).asScala))
          .map(_.instances().asScala.head)
      }
      .ensure
  }

  def instanceType(instanceType: InstanceType)(implicit timer: Timer[IO]): Ensure[Instance] = {
    EC2Ensure(
      ec2Client,
      rir.instanceType(instanceType),
      ens.subcondition { t =>
        Ensure.equal(t.instanceType(), instanceType)
          .recover {
            for {
              _ <- IO.fromFuture(IO {
                val req = StopInstancesRequest.builder()
                    .instanceIds(t.instanceId)
                    .build()
                ec2Client.stopInstances(req).asScala
              })
              _ <- IO(println("waiting"))
              _ <- waitForState(t.instanceId, "stopped")
              _ <- IO(println("done waiting"))
              _ <- IO.fromFuture(IO {
                val miar: ModifyInstanceAttributeRequest = ModifyInstanceAttributeRequest.builder()
                  .instanceId(t.instanceId())
                  .instanceType(AttributeValue.builder().value(instanceType.toString).build())
                  .build()
                ec2Client.modifyInstanceAttribute(miar).asScala
              })
              _ <- IO.fromFuture(IO(ec2Client.startInstances(
                  StartInstancesRequest.builder()
                    .instanceIds(t.instanceId)
                    .build()
                ).asScala
              ))
                  .handleErrorWith { x =>
                    IO(println("ih")).flatMap{ _ =>
                    x match {
                    case e: CompletionException => e.getCause match {
                      case e: Ec2Exception if e.statusCode() == 400 =>
                        IO(println("got error")).flatMap{ _ =>
                        IO.delay(Except("instance can't be started, destroying and reprovisioning"))
                        }
                    }}
                  }
                  }
            } yield {
              // TODO fetch the newly provisioned instance type?
              instanceType
            }
          }
      },
    )
  }

  private def waitForState(instanceId: String, state: String)(implicit timer: Timer[IO]): IO[_] = {
    IO.fromFuture(IO {
      val dir = DescribeInstancesRequest.builder()
          .instanceIds(instanceId)
          .build()
      ec2Client.describeInstances(dir).asScala
    })
      .flatMap { resp =>
        val i = resp.reservations().asScala.flatMap(_.instances.asScala).head
        if (i.state().nameAsString() == state) {
          IO.unit
        } else {
          IO(println(s"got ${i.state}")).flatMap { _ =>
          import scala.concurrent.duration._
          IO.sleep(1.second).flatMap(_ => waitForState(instanceId, state))
          }
        }
      }
  }
}
