package shapeless_.typedrow

import org.apache.spark.sql.Row

trait RowCell[T] {
  def apply(value: AnyRef): T
}

object RowCell {
  def apply[T](implicit cell: RowCell[T]): RowCell[T] = cell

  def instance[T](f: AnyRef => T): RowCell[T] =
    new RowCell[T] {
      def apply(value: AnyRef) = f(value)
    }

  implicit val double: RowCell[Double] =
    instance { v =>
      assert(v != null)
      v.asInstanceOf[Double]
    }
  implicit val float: RowCell[Float] =
    instance { v =>
      assert(v != null)
      v.asInstanceOf[Float]
    }
  implicit val long: RowCell[Long] =
    instance { v =>
      assert(v != null)
      v.asInstanceOf[Long]
    }
  implicit val int: RowCell[Int] =
    instance { v =>
      assert(v != null)
      v.asInstanceOf[Int]
    }
  implicit val boolean: RowCell[Boolean] =
    instance { v =>
      assert(v != null)
      v.asInstanceOf[Boolean]
    }
  implicit val string: RowCell[String] =
    instance { v =>
      assert(v != null)
      v.asInstanceOf[String]
    }

  implicit val row: RowCell[Row] =
    instance { v =>
      assert(v != null)
      v.asInstanceOf[Row]
    }

  implicit def option[T: RowCell]: RowCell[Option[T]] =
    instance { v =>
      if (v == null)
        None
      else
        Some(RowCell[T].apply(v))
    }
}
