package org.tksfz.ensuring

import cats.effect.{ExitCode, IO, IOApp}
import org.tksfz.ensuring.ec2.EC2Resources
import software.amazon.awssdk.services.ec2.Ec2AsyncClient
import software.amazon.awssdk.services.ec2.model.InstanceType

object Example extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val ec2Client = Ec2AsyncClient.builder().build()
    val ec2 = new EC2Resources(ec2Client)
    (for {
      myInstance <- ec2.ec2("ami-0c6af4325106f3667", "Name" -> "test")
        .instanceType(InstanceType.T3_MICRO)
    } yield {
      myInstance
    }).ensure.flatMap {
      case a@Already(t) => IO(println(a))
      case Except(err) => IO(println(err))
      case TBD(io) => IO(println("provisioning:")).flatMap(_ => io).map(println(_))
    }.map { _ =>
      ExitCode.Success
    }
  }
}
