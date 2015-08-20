package fp

import java.util.concurrent._

object Parallelism {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
  def unit[A](a: A): Par[A] =  (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone = true

    override def isCancelled: Boolean = false

    override def get(timeout: Long, timeUnit: TimeUnit): A = get

    override def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def combine[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call(): A = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }
}

object ParTests extends App {
  import fp.Parallelism._
  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.length <= 1) {
      unit(ints.headOption.getOrElse(0))
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      combine(fork(sum(l)), fork(sum(r)))(_ + _)
    }
  }

  val es = Executors.newFixedThreadPool(
    java.lang.Runtime.getRuntime.availableProcessors())
  val p = sum(IndexedSeq(1, 2, 3, 4, 5))
  println(p(es).get())
}
