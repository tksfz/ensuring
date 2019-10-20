package org.tksf.ensuring.ec2

import cats.effect.{ContextShift, IO}
import org.tksf.ensuring.{Already, Ensure, State}
import software.amazon.awssdk.services.ec2.Ec2AsyncClient
import software.amazon.awssdk.services.ec2.model.{AttributeValue, DescribeInstancesRequest, DescribeInstancesResponse, Filter, Instance, InstanceType, ModifyInstanceAttributeRequest, RunInstancesRequest}

import scala.jdk.CollectionConverters._
import scala.jdk.FutureConverters._

class EC2Resources(ec2Client: Ec2AsyncClient) {
  def ec2(instanceId: String)(implicit cs: ContextShift[IO]): EC2Ensure = {
    val request = DescribeInstancesRequest.builder()
      .instanceIds(instanceId)
      .build()
    EC2Ensure(ec2Client,
      RunInstancesRequest.builder(),
      Ensure.find {
        // TODO: find by tag or whatnot
        IO.fromFuture(IO(ec2Client.describeInstances(request).asScala))
          .map(_.reservations().asScala.flatMap(_.instances.asScala).headOption)
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

  def instanceType(instanceType: InstanceType): Ensure[Instance] = {
    EC2Ensure(
      ec2Client,
      rir.instanceType(instanceType),
      ens.subcondition { t =>
        Ensure.equal(t.instanceType(), instanceType)
          .recover {
            IO.fromFuture(IO {
              val miar: ModifyInstanceAttributeRequest = ModifyInstanceAttributeRequest.builder()
                .instanceType(AttributeValue.builder().value(instanceType.toString).build())
                .build()
              ec2Client.modifyInstanceAttribute(miar).asScala
            }).map(_ => instanceType) // TODO fetch the newly provisioned instance type?
          }
      },
    )
  }
}
