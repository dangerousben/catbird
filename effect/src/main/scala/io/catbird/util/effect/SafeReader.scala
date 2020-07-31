package io.catbird.util.effect

import cats.effect.{ Resource, Sync }
import com.twitter.concurrent.AsyncStream
import com.twitter.io.Reader

class SafeReader[F[_]: Sync] {
  def fromAsyncStream[A](as: AsyncStream[A]): Resource[F, Reader[A]] =
    resource(Reader.fromAsyncStream(as))

  def resource[A](reader: => Reader[A]): Resource[F, Reader[A]] =
    Resource.make(Sync[F].delay(reader))(r => Sync[F].delay(r.discard()))

  implicit class ReaderOps[A](r: Reader[A]) {
  }
}

object SafeReader {
  def apply[F[_]: Sync]: SafeReader[F] = new SafeReader[F]
}
