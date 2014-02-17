package fp.matrix

import scala.collection.mutable.ArrayBuffer

/**
 *
 */
class Matrix(private val repr: Array[Array[Double]]) {
  lazy val rowRank = repr.size
  lazy val colRank = if(rowRank > 0) repr(0).size else 0

  def row(idx: Int): Seq[Double] = repr(idx)
  def col(idx: Int): Seq[Double] =
    repr.foldLeft(ArrayBuffer[Double]()) {
      (buffer, currentRow) =>
        buffer.append(currentRow(idx))
        buffer
    }
  override def toString = "Matrix" + repr.foldLeft("") {
    (msg, row) => msg + row.mkString("\n|", " | ", "|")
  }
}

object Matrix {
  // implicit view here, usually used
  implicit def arrayToMatrix(array: Array[Array[Double]]): Matrix = {
    new Matrix(array)
  }

  // This implicit
  implicit val strategy = SameThreadStrategy

  def multiply(a: Matrix, b: Matrix)
              (implicit threading: ThreadStrategy): Matrix = {
    assert(a.colRank == b.rowRank)
    val buffer = new Array[Array[Double]](a.rowRank)
    for (i <- 0 until a.rowRank) {
      buffer(i) = new Array[Double](b.colRank)
    }

    def computeValue(row: Int, col: Int): Unit = {
      val pairwiseElems = a.row(row).zip(b.col(col))
      val products = for ((x, y) <- pairwiseElems) yield x * y
      val result = products.sum
      buffer(row)(col) = result
    }

    val computations = for {
      i <- 0 until a.rowRank
      j <- 0 until b.colRank
    } yield threading.execute {() => computeValue(i, j)}

    computations.foreach(_())
    new Matrix(buffer)
  }

  def main(args: Array[String]) = {
    val x = Array(Array(1d,2d,3d), Array(4d,5d,6d))
    val y = Array(Array(1d), Array(1d), Array(1d))

    println(Matrix.multiply(x, y))
    println(Matrix.multiply(x, y)(ThreadPoolStrategy))
  }
}