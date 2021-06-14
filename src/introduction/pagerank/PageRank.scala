package introduction.pagerank

/*
* This algorithm will be an example of Spark's big data computation capabilities
* PageRank is a Google algorithm that ranks web pages based on graphs.
* The rank of each node is computed iteratively through parameters that
* weigh the rank of the nodes that are linked to it.
*/
import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
import org.apache.commons.io.FileUtils
import org.apache.spark.SparkContext.rddToPairRDDFunctions

import java.io
import java.io.File

object PageRank {
    def main(args: Array[String]): Unit = {
        val conf = new SparkConf().setAppName("pageRank").setMaster("local[*]")
        val sc = new SparkContext(conf)
        val inputFile = ""
        val outputFile = ""
        val input = sc.textFile(inputFile) // The input is a RDD of Strings: a special Spark collection
        // After the initial definition, we can use Scala's high order functions
        // almost without apparent changes because there is a 1:1 mapping
        // of the standard library and structure.
        val edges = input.map(s => s.split("\t")).map(
            a => (a(0).toInt, a(1).toInt)
        )
        /* All of these are RDD transformations. The pairs of the map will be distributed
        * between computing nodes automatically thanks to Spark. */
        val links = edges.groupByKey().persist()
        var ranks = links.mapValues(v => 1.0)

        /* This for will work on the distributed RDD transparently */
        for(i <- 0 until 10) {
            val contributions = links.join(ranks).flatMap {
                case (u, (uLinks, rank)) =>
                    uLinks.map(t => (t, rank / uLinks.size))
            }
            ranks = contributions.reduceByKey((x,y) => x+y).
                mapValues(v => 0.15+0.85*v)
        }

        /* Final gathering of results */
        FileUtils.deleteDirectory(new File(outputFile))
        /* This is an RDD action (different from transformations)
        * Transformations are not executed instantly. The transformations
        * are triggered by actions. This delayed execution allows Spark
        * to only compute what is needed, when it's needed. This is an important
        * optimization. */
        ranks.saveAsTextFile(outputFile)
        sc.stop()
    }
}
