package macroexamples

import scalamacro.DebugMacroDefs._

object DebugExample extends App {
  val y = 16

  def testFunc(): Unit = {
    val p = 12
    debug(p)
    debug(p + y)
  }

  testFunc()
}
