package scalamacro

import scala.reflect.macros.Context
import scala.language.experimental.macros


object DebugMacroDefs {
  def hello(): Unit = macro helloImpl

  def helloImpl(c: Context)(): c.Expr[Unit] = {
    import c.universe._
    reify {println("Hello world!")}
  }
}
