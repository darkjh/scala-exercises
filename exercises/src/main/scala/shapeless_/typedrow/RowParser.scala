package shapeless_.typedrow

import org.apache.spark.sql.Row
import shapeless._

trait RowParser[T] {
  def apply(row: Row, fromIdx: Int): T
  def apply(row: Row): T = apply(row, 0)
}

object RowParser {
  def apply[T](implicit parser: RowParser[T]): RowParser[T] = parser

  def instance[T](f: (Row, Int) => T): RowParser[T] =
    new RowParser[T] {
      def apply(row: Row, fromIdx: Int) = f(row, fromIdx)
    }

  implicit val hnil: RowParser[HNil] =
    instance { (row, fromIdx) =>
      // check that there are no more elements remaining
      assert(row.length == fromIdx, s"Expected $fromIdx element(s), got ${row.length}")
      HNil
    }
  implicit def hcons[H, T <: HList]
   (implicit
     cell: RowCell[H],
     tail: RowParser[T]
   ): RowParser[H :: T] =
    instance { (row, fromIdx) =>
      assert(row.length > fromIdx, s"Expected at least $fromIdx element(s), got ${row.length}")
      val v = row.get(fromIdx).asInstanceOf[AnyRef]
      cell(v) :: tail(row, fromIdx + 1)
    }
  implicit def generic[F, G]
   (implicit
     gen: Generic.Aux[F, G],
     parser: RowParser[G]
   ): RowParser[F] =
    instance { (row, fromIdx) =>
      gen.from(parser(row, fromIdx))
    }
}
