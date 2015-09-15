package fp.matrix

import java.util.concurrent.{Callable, Executors}

trait ThreadStrategy {
  def execute[A](func: Function0[A]): Function0[A]
}

object SameThreadStrategy extends ThreadStrategy {
  def execute[A](func: Function0[A]) = func
}

object ThreadPoolStrategy extends ThreadStrategy {
  val pool = Executors.newFixedThreadPool(
    java.lang.Runtime.getRuntime.availableProcessors())
  def execute[A](func: () => A) = {
    val future = pool.submit(new Callable[A] {
      def call(): A = {
        Console.println("Executing function on thread: " +
          Thread.currentThread().getName)
        func()
      }
    })
    () => future.get()
  }
}

