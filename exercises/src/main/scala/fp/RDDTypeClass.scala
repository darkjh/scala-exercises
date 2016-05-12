package fp

import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

import scala.language.higherKinds
import scala.reflect.ClassTag


trait RDDLike[F[_]] {
  def map[A, B: ClassTag](fa: F[A], f: A => B): F[B]

  def filter[A](fa: F[A], p: A => Boolean): F[A]

  def isEmpty[A](fa: F[A]): Boolean

  def reduce[A](fa: F[A], f: (A, A) => A): A
}


object RDDLike {
  def apply[F[_]](implicit ev: RDDLike[F]): RDDLike[F] = ev

  implicit class RDDLikeOps[F[_]: RDDLike, A](fA: F[A]) {
    def map[B: ClassTag](f: A => B): F[B] = RDDLike[F].map(fA, f)
    def filter(p: A => Boolean): F[A] = RDDLike[F].filter(fA, p)
    def isEmpty: Boolean = RDDLike[F].isEmpty(fA)
    def reduce(f: (A, A) => A): A = RDDLike[F].reduce(fA, f)
  }
}


case class RawRow(id: Long, data1: String, data2: Int)


case class ResultRow(id: Long, data: String)


object Implicits {
  implicit def rdd: RDDLike[RDD] =
    new RDDLike[RDD] {
      def map[A, B: ClassTag](rddA: RDD[A], f: A => B): RDD[B] = rddA.map(f)
      def filter[A](rddA: RDD[A], p: A => Boolean): RDD[A] = rddA.filter(p)
      def isEmpty[A](rddA: RDD[A]): Boolean = rddA.isEmpty
      def reduce[A](rddA: RDD[A], f: (A, A) => A): A = rddA.reduce(f)
    }

  implicit def seq: RDDLike[Seq] =
    new RDDLike[Seq] {
      def map[A, B: ClassTag](seq: Seq[A], f: (A) => B): Seq[B] = seq.map(f)
      def reduce[A](seq: Seq[A], f: (A, A) => A): A = seq.reduce(f)
      def filter[A](seq: Seq[A], p: (A) => Boolean): Seq[A] = seq.filter(p)
      def isEmpty[A](seq: Seq[A]): Boolean = seq.isEmpty
    }
}


object RDDTypeClass extends App {
  import Implicits._
  import RDDLike._

  val rawData = List(
    RawRow(1, "data", 1),
    RawRow(1, "data", 2),
    RawRow(1, "data", 3),
    RawRow(2, "data", 4)
  )

  def processing[F[_] : RDDLike](data: F[RawRow]): F[ResultRow] =
    data
      .filter(_.id == 1)
      .map { rr =>
        ResultRow(rr.id, rr.data1 + rr.data2.toString)
      }

  // on RDD
  val conf = new SparkConf().setAppName("test").setMaster("local")
  val sc = new SparkContext(conf)

  val dataRdd = sc.parallelize(rawData)
  processing(dataRdd).foreach(println)

  // on Seq
  processing(rawData.asInstanceOf[scala.collection.Seq[RawRow]]).foreach(println)
}