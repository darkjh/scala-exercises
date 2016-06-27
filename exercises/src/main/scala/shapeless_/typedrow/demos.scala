package shapeless_.typedrow

import org.apache.spark.sql.SQLContext
import org.apache.spark.{SparkContext, SparkConf}

import syntax._


case class C(a: Int, b: Int)


object Demo extends App {
  val conf = new SparkConf()
    .setAppName("demo")
    .setMaster("local")
  val sc = new SparkContext(conf)
  val sql = new SQLContext(sc)
  import sql.implicits._

  val df = sql.createDataFrame(List((1, 2), (2, 2), (3, 2), (4, 2)))

  val results = df.collect().as[C].foreach(println)
}
