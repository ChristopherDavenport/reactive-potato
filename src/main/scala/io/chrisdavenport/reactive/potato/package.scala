package io.chrisdavenport.reactive

import cats.effect.IO

package object potato {
  type Handler[A] = A => IO[Unit]
}