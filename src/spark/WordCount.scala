package spark

import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
import org.apache.commons.io.FileUtils
import org.apache.spark.SparkContext.rddToPairRDDFunctions

import java.io._

object WordCount {
    def main(args: Array[String]): Unit = {
        val conf = new SparkConf().setAppName("wordCount").setMaster("local[*]");
        val sc = new SparkContext(conf);
        val inputFile = "";
        val outputFile = "";
        val input = sc.textFile(inputFile)
        val counts = input.flatMap(line => line.split(" ")).
            map(word => (word.filter(_.isLetter).toUpperCase(), 1)).
            reduceByKey((x,y) => x + y);
        FileUtils.deleteDirectory(new File(outputFile))
        counts.saveAsTextFile(outputFile)
        sc.stop()
    }
}
