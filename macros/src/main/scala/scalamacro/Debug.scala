package scalamacro

import scala.reflect.macros.Context
import scala.language.experimental.macros


object DebugMacroDefs {
  def debug(param: Any): Unit = macro debugImpl

  def debugImpl(c: Context)(param: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._
    val paramRep = show(param.tree)
    val paramRepTree = Literal(Constant(paramRep))
    val paramRepExpr = c.Expr[String](paramRepTree)
    reify {println(paramRepExpr.splice + " = " + param.splice)}
  }
}