package teachercode

import org.apache.spark.SparkContext.rddToPairRDDFunctions

import java.io._
import org.apache.spark.{SparkConf, SparkContext}

import scala.util.Random

object KMeans {

  val fileName =
    "/Users/zavattar/IdeaProjects/AA20-21/kmeans.txt"

  val epsilon = 0.0001
  val numK = 4

  val randomX = new Random
  val randomY = new Random
  val maxCoordinate = 100.0

  def genFile() = {

    val initPoints = Vector((50.0,50.0),(50.0,-50.0),(-50.0,50.0),(-50.0,-50.0))

    val distance = 80.0
    val numPoints = 10000000

    // only using reduce by key
    //    val numPoints = 100000000

    val randomPoint = new Random

    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    for (i <- 0 until numPoints) {
      val x = (randomX.nextDouble-0.5) * distance
      val y = (randomX.nextDouble-0.5) * distance
      val centroid = initPoints(randomPoint.nextInt(initPoints.length))
      bw.write((centroid._1+x)+"\t"+(centroid._2 + y)+"\n")
    }
    bw.close
  }

  def distance(p1:(Double, Double), p2:(Double, Double)) =
    math.sqrt(
      math.pow(p2._1-p1._1, 2) +
        math.pow(p2._2-p1._2, 2) )

  def findClosest(p: (Double, Double),
                  centroids: Array[(Int,(Double, Double))]): Int =
    centroids.map(c => (c._1,distance(c._2,p))).
      minBy(_._2)._1

  def weightedMean(x: Double, n: Double, y: Double, m: Double) =
    1.0/(1.0+m/n)*x + 1.0/(1.0+n/m)*y

  def weightedMeanPoint(x: ((Double,Double),Double),
                        y: ((Double,Double),Double)) =
    ((weightedMean(x._1._1,x._2,y._1._1,y._2),
      weightedMean(x._1._2,x._2,y._1._2,y._2)),
      x._2+y._2)

  def meanDistance(c: Array[(Int,(Double, Double))],
                   newc: Array[(Int,(Double, Double))]) =
    ((c zip newc).
      map (c => distance(c._1._2,c._2._2)).
      sum) / c.length

  def main(args: Array[String]): Unit = {

    genFile()

    val conf = new SparkConf().setAppName("KMeans").setMaster("local[*]")
    val sc = new SparkContext(conf)
    val input = sc.textFile(fileName)
    val sparkPoints = input.map(s =>
      ( (s.takeWhile(_ != '\t')).toDouble,
        (s.dropWhile(_ != '\t')).toDouble )
    ).cache
    var centroids =
      ((0 until numK) zip
        sparkPoints.takeSample(false,numK)).
        toArray

    var finished=false

    var numIterations=0

    do {
      val newCentroids = sparkPoints.
        map(p => (findClosest(p,centroids), (p,1.0))).
        //        groupBy(p => (findClosest(p,centroids))).
        reduceByKey(weightedMeanPoint).
        //        map((x:(Int,Iterable[(Double,Double)])) =>
        //            (x._1, (x._2.map((_, 1.0))).reduce(weightedMeanPoint))).
        map(c => (c._1,c._2._1)).
        collect()

      if (meanDistance(centroids,newCentroids) < epsilon)
        finished=true
      else centroids=newCentroids
      numIterations+=1
    } while(!finished)
    centroids.map(println)
    println("Iterations = "+numIterations)
  }

}