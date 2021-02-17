package introduction.mandelbrot

import utils.Complex

import java.io.FileOutputStream
import java.nio.file.Paths

import scala.collection.parallel.CollectionConverters._

object Mandelbrot {
    /*
    * This program will compute the introduction.mandelbrot set and reproduce it in a plot.
    * The plot will show a fractal image.
    * Elements in the introduction.mandelbrot set are imaginary numbers:
    * - An imaginary number c is defined as (a,b) where a is the real part and b
    *   is the imaginary part. c = a + b*i (where i is the solution to x^2 = -1)
    */

    /*
    * First of all, we define a class for complex numbers in the utils package.
    */

    /*
    * Then, we define a filename where the image will be saved.
    * Change this when running the code on other systems.
    */
    val fileName: String = Paths.get("imgs", "scalaimage.pgm").toString
    val fileNameParallel: String = fileName.replace(".pgm", "_par.pgm").toString

    def run(n: Int, level: Int): Unit = {
        val out = new FileOutputStream(fileName)
        out.write(("P5\n"+n+" "+n+"\n255\n").getBytes())
        for (j <- 0 until n*n) {
            // We consider n*n numbers: are they in the introduction.mandelbrot set?
            // The coordinates of the point are computed
            // (the image is divided vertically in three parts)
            val x = -2.0 + j%n * 3.0 / n
            val y = -1.5 + j/n * 3.0 / n
            var z = new Complex(0, 0)
            // c is the number we are interested in
            val c = new Complex(x, y)
            var i = 0
            // We want to see if the sequence of numbers (pruned at a certain level)
            // converges or diverges.
            // Start from (0,0) and iteratively expand the sequence.
            // If at a certain level the absolute value of z is greater than 2
            // the number is guaranteed to diverge (mathematical formula)
            // We can also stop when we reach our desired level of expansion.
            while (z.abs < 2 && i < level) {
                z=z*z +c; i=i+1
            }
            out.write((255 * (i - 1) / level).toInt)
        }
        out.close()
    }

    /*
    * Note that with this algorithm we do a computation on each pixel.
    * Each pixel's computation is completely independent from the others.
    * Therefore, we could easily parallelize this program.
    * Scala makes this parallelization super easy and makes it scalable as well
    * for machines with a great number of nodes.
    * We just need to modify the algorithm a little.
    * - The range indicated in the for construct should be parallelized.
    *   This is because a single number from that range is used to performed
    *   the whole body of the for.
    */

    def runParallel(n: Int, level: Int): Unit = {
        val out = new FileOutputStream(fileNameParallel)
        out.write(("P5\n"+n+" "+n+"\n255\n").getBytes())
        // We need an intermediate array because we can't write directly on a file
        // in a parallel computation
        var a = new Array[Int](n*n)
        for (j <- (0 until n*n).par) {
            val x = -2.0 + (j%n) * 3.0/n
            val y = -1.5 + (j/n) * 3.0/n
            var z = new Complex(0, 0)
            val c = new Complex(x,y)
            var i = 0
            while (z.abs < 2 && i < level) {z = z*z + c; i=i+1}
            a(j) = (255 * (i - 1) / level).toInt
        }
        // Final write on file
        for (k <- 0 until n*n) out.write(a(k))
        out.close()
    }

    def main(args: Array[String]): Unit = {
        var start = System.nanoTime()
        run(1000, 50)
        var end = System.nanoTime()
        println("Classic execution: Elapsed time: " + (end - start)/1000000 + "ms")
        start = System.nanoTime()
        runParallel(1000, 50)
        end = System.nanoTime()
        println("Parallel execution: Elapsed time: " + (end - start)/1000000 + "ms")
    }

}
