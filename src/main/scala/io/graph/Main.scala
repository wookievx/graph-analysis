package io.graph

import java.io.{BufferedWriter, FileWriter}

import io.graph.alg.BellmanFordAlg
import io.graph.parser.StringParser
import cats.instances.double._

import scala.util.Try

object Main {

  def main(args: Array[String]): Unit = {
    val word = args(0)
    val graph = StringParser.readGraph(word)
    val distances = BellmanFordAlg.bellmanFordImpl(graph, word)
    val transformed = distances.mapValues(_.toFinite.map(_.toString).getOrElse("INF"))
    val writer = new BufferedWriter(new FileWriter("results.out"))
    transformed.toSeq.sortBy(p => Try(p._2.toDouble).getOrElse(Double.MaxValue)).foreach {
      case (w, distance) =>
        println(s"$w, $distance")
        writer.write(s"$w, $distance")
        writer.newLine()
    }
    writer.close()
  }

}
