package io.catbird.util

import com.twitter.concurrent.AsyncStream
import fs2.Stream

object Fs2 {
  def asyncStreamToFs2Stream[A](as: => AsyncStream[A]): Stream[Rerunnable, A] =
    Stream.unfoldEval(Rerunnable(as))(_.flatMapF(_.uncons.map(_.map { case (head, tail) => (head, Rerunnable(tail())) })))
}
