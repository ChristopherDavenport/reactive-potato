package io.chrisdavenport.reactive.potato

import io.chrisdavenport.unique._
import cats.implicits._
import cats.effect._
import cats.effect.concurrent._

final case class AddHandler[A](register: Handler[A] => IO[IO[Unit]]){

  def map[B](f: A => B): AddHandler[B] = mapIO(f(_).pure[IO])
  /**
    * Map the event value with an 'IO' action.
    */
  def mapIO[B](f: A => IO[B]): AddHandler[B] = AddHandler{h: Function1[B, IO[Unit]] => 
      register(f(_) >>= h)
  }

  /**
    *  Filter event values that don't return 'True'.
    */
  def filterIO(f: A => IO[Boolean]): AddHandler[A] = AddHandler{h: Function1[A, IO[Unit]] => 
    register{a: A => f(a).ifM(h(a), IO.unit)}
  }
}


object AddHandler {

  def newAddHandler[A] : IO[(AddHandler[A], Handler[A])] = {
    def registerUnregister(
      handlers: Ref[IO, Map[Unique, Handler[A]]]
    ): AddHandler[A] = AddHandler{handler => for {
      key <- Unique.newUnique[IO]
      _ <- handlers.update(_ + (key -> handler))
    } yield handlers.update(_ - key)
    }
    def runHandlers(handlers: Ref[IO, Map[Unique, Handler[A]]]): Handler[A] = 
      {a => 
        handlers.get.map(_.values).map(_.toList)
        .flatMap(l => l.traverse_(_(a)))
      }
    for {
    handlers <- Ref.of[IO, Map[Unique, Handler[A]]](Map.empty[Unique, Handler[A]])
    } yield (registerUnregister(handlers), runHandlers(handlers))
  }

}