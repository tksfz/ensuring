package org.tksf.ensuring.ec2

import cats.effect.{ContextShift, IO}
import org.tksf.ensuring.{Already, Ensure}
import software.amazon.awssdk.services.ec2.Ec2AsyncClient
import software.amazon.awssdk.services.ec2.model.{DescribeInstancesRequest, DescribeInstancesResponse, Instance, RunInstancesRequest}

import scala.jdk.CollectionConverters._
import scala.jdk.FutureConverters._

class EC2Resources(ec2Client: Ec2AsyncClient) {
  def ec2(instanceId: String)(implicit cs: ContextShift[IO]): Ensure[Instance] = {
    val request = DescribeInstancesRequest.builder()
      .instanceIds(instanceId)
      .build()
    Ensure.find {
      IO.fromFuture(IO(ec2Client.describeInstances(request).asScala))
        .map(_.reservations().asScala.flatMap(_.instances.asScala).headOption)
    }
      .recover {
        // TODO: instance type and other subcondition options
        // TODO: instanceId can't be our param because the creation below won't produce the "desired" instanceId
        val rir = RunInstancesRequest.builder()
            .build()
        IO.fromFuture(IO(ec2Client.runInstances(rir).asScala))
          .map(_.instances().asScala.head)
      }
  }
}
