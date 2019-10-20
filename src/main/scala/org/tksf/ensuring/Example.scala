package org.tksf.ensuring

import cats.effect.{ExitCode, IO, IOApp}
import org.tksf.ensuring.ec2.EC2Resources
import software.amazon.awssdk.services.ec2.Ec2AsyncClient
import software.amazon.awssdk.services.ec2.model.{AttributeValue, InstanceType, ModifyInstanceAttributeRequest}

import scala.jdk.FutureConverters._
import scala.concurrent.ExecutionContext.Implicits.global

object Example extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val ec2Client = Ec2AsyncClient.builder().build()
    val ec2 = new EC2Resources(ec2Client)
    (for {
      myInstance <- ec2.ec2("i-12345")
        .subcondition { i =>
          Ensure.equal(i.instanceType(), InstanceType.M2_XLARGE)
            .recover { IO.fromFuture { IO {
              val miar: ModifyInstanceAttributeRequest = ModifyInstanceAttributeRequest.builder()
                  .instanceType(AttributeValue.builder().value("m2.xlarge").build())
                  .build()
              ec2Client.modifyInstanceAttribute(miar).asScala
                .map(_ => i.instanceType()) // TODO
            } }
            }
        }
    } yield {
      ()
    }).ensure.map { _ =>
      ExitCode.Success
    }
  }
}
