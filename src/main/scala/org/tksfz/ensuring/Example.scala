package org.tksf.ensuring

import cats.effect.{ExitCode, IO, IOApp}
import org.tksf.ensuring.ec2.EC2Resources
import software.amazon.awssdk.services.ec2.Ec2AsyncClient
import software.amazon.awssdk.services.ec2.model.InstanceType

object Example extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val ec2Client = Ec2AsyncClient.builder().build()
    val ec2 = new EC2Resources(ec2Client)
    (for {
      myInstance <- ec2.ec2("i-12345")
        .instanceType(InstanceType.M2_XLARGE)
    } yield {
      ()
    }).ensure.map { _ =>
      ExitCode.Success
    }
  }
}
