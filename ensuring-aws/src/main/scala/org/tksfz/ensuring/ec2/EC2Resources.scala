package org.tksfz.ensuring.ec2

import java.util.concurrent.CompletionException

import cats.effect.{ContextShift, IO, Timer}
import org.tksfz.ensuring.{Already, Ensure, Except, State}
import software.amazon.awssdk.services.ec2.Ec2AsyncClient
import software.amazon.awssdk.services.ec2.model.{AttributeValue, DescribeInstancesRequest, DescribeInstancesResponse, Ec2Exception, Filter, Instance, InstanceState, InstanceType, ModifyInstanceAttributeRequest, ResourceType, RunInstancesRequest, StartInstancesRequest, StopInstancesRequest, Tag, TagSpecification, TerminateInstancesRequest}

import scala.jdk.CollectionConverters._
import scala.jdk.FutureConverters._

class EC2Resources(ec2Client: Ec2AsyncClient) {
  def ec2(imageId: String, subnetId: String, tag: (String, String))(implicit cs: ContextShift[IO], timer: Timer[IO]): EC2Ensure = {
    val request = DescribeInstancesRequest.builder()
      .filters(
        Filter.builder().name(s"tag:${tag._1}").values(tag._2).build(),
        // exclude terminated instances only
        Filter.builder().name("instance-state-name").values(Seq("pending", "running", "shutting-down", "stopping", "stopped").asJava).build(),
      )
      .build()
    EC2Ensure(ec2Client,
      request,
      RunInstancesRequest.builder()
        .imageId(imageId)
        .minCount(1)
        .maxCount(1)
        .subnetId(subnetId)
        .tagSpecifications(TagSpecification.builder()
          .resourceType(ResourceType.INSTANCE)
          .tags(Tag.builder().key(tag._1).value(tag._2).build()).build()),
      Ensure.already[IO, Instance](_)
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
case class EC2Ensure(ec2Client: Ec2AsyncClient, request: DescribeInstancesRequest, rir: RunInstancesRequest.Builder,
                     setup: Instance => Ensure[IO, Instance])
                    (implicit cs: ContextShift[IO], timer: Timer[IO]) extends Ensure[IO, Instance] {
  def ensure: IO[State[IO, Instance]] = {
    val initial: Ensure[IO, Instance] =
      Ensure.find {
        IO.fromFuture(IO(ec2Client.describeInstances(request).asScala))
          .map(_.reservations().asScala.flatMap(_.instances.asScala).headOption)
      }
        .recoverWithDefault {
          IO.fromFuture(IO(ec2Client.runInstances(rir.build()).asScala))
            .map(_.instances().asScala.head)
          // TODO: consider waiting for started
          // This should probably restart thhe Ensure too
        }

    /**
      * This recursive structure is only needed because
      * we want recoverWith to have access to the Instance i
      * to terminate the instance. So recoverWith is nested
      * inside the flatMap.
      *
      * TODO: I think this might be bracket:
      * initial.bracket(destroy) { setup }
      */
    def beginningToEnd: Ensure[IO, Instance] = {
      initial.flatMap { i =>
        setup(i)
          .log()
          .recoverWith {
            // If setup fails then terminate and go back to the beginning
            Ensure.lift(
              for {
                _ <- IO.fromFuture(IO {
                  println("terminating")
                  ec2Client.terminateInstances(TerminateInstancesRequest.builder()
                    .instanceIds(i.instanceId)
                    .build()).asScala
                })
                _ <- waitForState(i.instanceId, "terminated")
              } yield {
                ()
              }
            )
              .flatMap { _ => beginningToEnd }
          }
      }
    }
    beginningToEnd.ensure
  }

  def instanceType(instanceType: InstanceType)(implicit timer: Timer[IO]): EC2Ensure = {
    this.copy(
      rir = rir.instanceType(instanceType),
      setup = setup(_).subcondition { t =>
        Ensure.equal[IO, InstanceType](t.instanceType(), instanceType)
          .recoverWithF {
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
              x <- IO.fromFuture(IO(ec2Client.startInstances(
                  StartInstancesRequest.builder()
                    .instanceIds(t.instanceId)
                    .build()
                ).asScala
              ))
                  .map[State[IO, InstanceType]](_ => Already(instanceType))
                  .handleErrorWith[State[IO, InstanceType]] {
                    // This no longer repros but can be simulated by forcing an Except
                    case e: CompletionException => e.getCause match {
                      case e: Ec2Exception if e.statusCode() == 400 =>
                        IO { println(s"got error"); e.printStackTrace() }.flatMap{ _ =>
                        IO.delay(Except("instance can't be started, destroying and reprovisioning"))
                        }
                    }
                  }
                  //.map(_ => Except("fake"))
            } yield {
              // TODO fetch the newly provisioned instance type?
              x
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
        val current = i.state().nameAsString()
        if (current == state) {
          IO.unit
        } else if (current == "terminated") {
          IO.raiseError(new IllegalStateException("terminated"))
        } else {
          IO(println(s"got ${i.state}")).flatMap { _ =>
          import scala.concurrent.duration._
          IO.sleep(1.second).flatMap(_ => waitForState(instanceId, state))
          }
        }
      }
  }
}
