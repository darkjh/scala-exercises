package shapeless_.json

import spray.json._

import shapeless._, labelled._
import syntax.singleton._


object JsonFormat {
  def apply[T](implicit f: Lazy[JsonFormat[T]]): JsonFormat[T] = f.value
}

// TODO low priority
object Formats extends DefaultJsonProtocol {
  implicit object HNilFormat extends JsonFormat[HNil] {
    override def read(json: JsValue): HNil = HNil
    override def write(obj: HNil): JsValue = JsObject()
  }

  implicit def hListFormat[Key <: Symbol, Value, Remaining <: HList](
    implicit
    key: Witness.Aux[Key],
    lazyJfh: Lazy[JsonFormat[Value]],
    lazyJft: Lazy[JsonFormat[Remaining]]
  ) = new JsonFormat[FieldType[Key, Value] :: Remaining] {
    val jfh = lazyJfh.value
    val jft = lazyJft.value

    override def read(json: JsValue): FieldType[Key, Value] :: Remaining = {
      val fields = json.asJsObject.fields
      val head = jfh.read(fields(key.value.name))
      val tail = jft.read(json)
      field[Key](head) :: tail
    }

    override def write(hlist: FieldType[Key, Value] :: Remaining): JsValue = {
      val fields =
        jft.write(hlist.tail).asJsObject().fields + (key.value.name -> jfh.write(hlist.head))
      JsObject(fields)
    }
  }

  implicit def familyFormat[T, Repr](
    implicit
    gen: LabelledGeneric.Aux[T, Repr],
    lazySg: Lazy[JsonFormat[Repr]]
  ): JsonFormat[T] = new JsonFormat[T] {
    val sg = lazySg.value

    def read(j: JsValue): T = gen.from(sg.read(j))
    def write(t: T): JsValue = sg.write(gen.to(t))
  }
}

case class Teapot(a: String, b: Long, c: Boolean)

object JsonDemo extends App {
  import Formats._

  // why error ???
//  implicit val teapotJsonFormat = cachedImplicit[JsonFormat[Teapot]]

  def toJson[T: JsonFormat](value: T): String = {
    val json = value.toJson
    json.compactPrint
  }

  def fromJson[T: JsonFormat](s: String): T = {
    s.parseJson.convertTo[T]
  }

  val expected = "{\"a\": \"hello\", \"b\": 13, \"c\": true}"

  println(toJson(Teapot("h", 15l, true)))
  println(fromJson[Teapot](expected))
}