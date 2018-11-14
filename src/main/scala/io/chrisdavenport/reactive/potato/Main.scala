package io.chrisdavenport.reactive.potato

import cats.implicits._
import cats.effect._
import scala.concurrent.duration._

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] = for {
    (add, react) <- AddHandler.newAddHandler[Int]
    _ <- add.register(PrintIt)
    removed <- add.register(PrintIt)
    _ <- add.register(PrintLater)
    _ <- removed
    _ <- react(1)

  } yield ExitCode.Success

  val PrintIt: Handler[Int] = {i: Int => IO(println(i))}
  val PrintLater: Handler[Int] = {
    i: Int => timer.sleep(1.second) >> IO(println(i + 1))
  }

}