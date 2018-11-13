package io.chrisdavenport.reactive.potato

import cats.implicits._
import cats.effect._
import cats.effect.concurrent._

case class Handler[A](run: A => IO[Unit])

case class AddHandler[A](register: Handler[A] => IO[IO[Unit]])

object AddHandler {

  def newAddHandler[A] : IO[(AddHandler[A], Handler[A])] = {
    def registerUnregister(
      handlers: Ref[IO, Map[Unique, Handler[A]]]
    ): AddHandler[A] = AddHandler{handler => for {
      key <- Unique.newUnique
      _ <- handlers.update(_ + (key -> handler))
    } yield handlers.update(_ - key)
    }
    def runHandlers(handlers: Ref[IO, Map[Unique, Handler[A]]]): Handler[A] = 
      Handler[A]{a => 
        handlers.get.map(_.values).map(_.toList)
        .flatMap(l => l.traverse_(_.run(a)))
      }
    for {
    handlers <- Ref.of[IO, Map[Unique, Handler[A]]](Map.empty[Unique, Handler[A]])
    } yield (registerUnregister(handlers), runHandlers(handlers))
  }
}

final class Unique private {}
object Unique {
  def newUnique: IO[Unique] = IO.delay(new Unique)
}