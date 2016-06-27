package shapeless_.typedrow

import org.apache.spark.sql.Row

import scala.reflect.ClassTag

object syntax {

  implicit class RowSeqOps(val rows: Seq[Row]) extends AnyVal {
    def as[T: RowParser]: Seq[T] =
      rows.map(RowParser[T].apply(_))
  }

  implicit class RowArrayOps(val rows: Array[Row]) extends AnyVal {
    def as[T: ClassTag : RowParser]: Array[T] =
      rows.map(RowParser[T].apply(_))
  }

  implicit class RowOps(val row: Row) extends AnyVal {
    def as[T: RowParser]: T =
      RowParser[T].apply(row)
  }

}
