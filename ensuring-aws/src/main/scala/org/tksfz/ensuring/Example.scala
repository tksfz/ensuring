package org.tksfz.ensuring

import cats.effect.{ExitCode, IO, IOApp}
import org.tksfz.ensuring.ec2.EC2Resources
import software.amazon.awssdk.services.ec2.Ec2AsyncClient
import software.amazon.awssdk.services.ec2.model.InstanceType

object Example extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val ec2Client = Ec2AsyncClient.builder().build()
    val ec2 = new EC2Resources(ec2Client)
    val datacenter =
      for {
        myInstance <- ec2.ec2("ami-0c6af4325106f3667", args(0), "Name" -> "test")
          .instanceType(InstanceType.T1_MICRO)
      } yield {
        myInstance
      }
    val plan = datacenter.ensure.map { state =>
      println("plan: " + state)
    }
    //deepRun(datacenter.ensure)
    plan.map { x =>
      println(x)
      ExitCode.Success
    }
  }

  private def deepRun[A](f: IO[State[IO, A]]): IO[_] = {
    f.flatMap {
      case a@Already(t) => IO(println(a))
      case Except(err) => IO(println(err))
      case TBD(t, io) => IO(println("provisioning:")).flatMap(_ => deepRun(io))
    }
  }
}
