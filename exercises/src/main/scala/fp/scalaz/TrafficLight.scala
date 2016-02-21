package fp.scalaz

import scalaz._

object TrafficLight extends App {
  sealed trait TrafficLight
  case object Red extends TrafficLight
  case object Yellow extends TrafficLight
  case object Green extends TrafficLight

  implicit val TrafficLightEqual: Equal[TrafficLight] = Equal.equal(_ == _)


  // Red === Yellow
  //  ==> compile error
  // The problem is that scalaz's `Equal` is not covariant
  // To make this work we need `Red` and `Yellow` to be essentially the same type
}
