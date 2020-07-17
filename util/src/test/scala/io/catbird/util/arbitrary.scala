package io.catbird.util

import com.twitter.concurrent.AsyncStream
import com.twitter.conversions.DurationOps._
import com.twitter.io.Reader
import com.twitter.util.{ Future, Return, Try, Var }
import org.scalacheck.{ Arbitrary, Gen, Cogen }

trait ArbitraryInstances {
  implicit def futureArbitrary[A](implicit A: Arbitrary[A]): Arbitrary[Future[A]] =
    Arbitrary(A.arbitrary.map(Future.value))

  implicit def tryArbitrary[A](implicit A: Arbitrary[A]): Arbitrary[Try[A]] =
    Arbitrary(A.arbitrary.map(Return(_)))

  implicit def varArbitrary[A](implicit A: Arbitrary[A]): Arbitrary[Var[A]] =
    Arbitrary(A.arbitrary.map(Var.value))

  implicit def asyncStreamArbitrary[A](implicit A: Arbitrary[A]): Arbitrary[AsyncStream[A]] =
    Arbitrary(A.arbitrary.map(AsyncStream.of))

  // Note that this doesn't cover BufReader or InputStreamReader currently
  private def readerGen[A: Arbitrary]: Gen[Reader[A]] =
    Gen.frequency(
      1 -> Reader.empty[A],  // empty reader defined inline
      3 -> Arbitrary.arbitrary[A].flatMap(Reader.value(_)),  // FutureReader (used for both fromFuture and value)
      1 -> Reader.exception(new Exception),  // also FutureReader but making sure we exercise the failed case
      3 -> Gen.listOf(Arbitrary.arbitrary[A]).flatMap(Reader.fromSeq(_)),  // IteratorReader
      2 -> Arbitrary.arbitrary[AsyncStream[A]].flatMap(Reader.fromAsyncStream(_)),  // uses Pipe
      2 -> readerGen[A].map(Reader.value(_).flatten)  // flatten any of the other types
    )

  implicit def readerArbitrary[A: Arbitrary]: Arbitrary[Reader[A]] =
    Arbitrary(readerGen[A])

  implicit def rerunnableArbitrary[A](implicit A: Arbitrary[A]): Arbitrary[Rerunnable[A]] =
    Arbitrary(futureArbitrary[A].arbitrary.map(Rerunnable.fromFuture[A](_)))

  implicit def cogenFuture[A](implicit A: Cogen[A]): Cogen[Future[A]] =
    A.contramap(futureComonad(1.second).extract)

  implicit def cogenVar[A](implicit A: Cogen[A]): Cogen[Var[A]] =
    A.contramap(varComonad.extract)

  implicit def cogenRerunnable[A](implicit A: Cogen[A]): Cogen[Rerunnable[A]] =
    A.contramap(Rerunnable.rerunnableComonad(1.second).extract)
}
