import scala.io.Source
import scala.util.Random

object KMeansUtils {

  // 1. Calculate the Euclidean distance between two points `p` and `q`.
  def distance(p: Vector[Double], q: Vector[Double]): Double = {
    Math.sqrt((p zip q).map { case (pi, qi) => Math.pow(pi - qi, 2) }.sum)
  }

  // 2. Given a query point `q`, find the nearest point among `candidates`.
  def closestPoint(q: Vector[Double], candidates: Array[Vector[Double]]): Vector[Double] = {
    candidates.minBy(candidate => distance(q, candidate))
  }

  // 3. Perform the addition of two points `v1` and `v2`.
  def addVec(v1: Vector[Double], v2: Vector[Double]): Vector[Double] = {
    (v1 zip v2).map { case (v1i, v2i) => v1i + v2i }
  }

  // 4. Find the centroid of `cluster`.
  def average(cluster: Iterable[Vector[Double]]): Vector[Double] = {
    val n = cluster.size
    val sum = cluster.reduce(addVec)
    sum.map(_ / n)
  }

  // Function to read points from a file with tab-separated values
  def readPointsFromFile(filename: String): Seq[Vector[Double]] = {
    val source = Source.fromFile(filename)
    val points = source.getLines().map { line =>
      line.split("\t").map(_.toDouble).toVector
    }.toSeq
    source.close()
    points
  }
}

object KMeans {

  import KMeansUtils._

  def kmeans(points: Seq[Vector[Double]], k: Int, maxIterations: Int = 100): Seq[Vector[Double]] = {
    // Randomly initialize centroids
    var centroids = Random.shuffle(points).take(k)

    for (_ <- 1 to maxIterations) {
      // Assign points to the nearest centroid
      val clusters = points.groupBy(p => closestPoint(p, centroids.toArray))

      // Recalculate centroids
      val newCentroids = clusters.values.map(average).toSeq

      // Check for convergence
      if (centroids == newCentroids) {
        return centroids
      }

      centroids = newCentroids
    }

    centroids
  }
}

// Example usage
object Main extends App {
  import KMeansUtils._

  // Read points from file
  val points = readPointsFromFile("clustering_dataset.txt")

  // Number of clusters
  val k = 3

  // Perform K-Means clustering
  val centroids = KMeans.kmeans(points, k)

  // Print the resulting centroids
  println(s"Centroids: ${centroids.mkString(", ")}")
}
