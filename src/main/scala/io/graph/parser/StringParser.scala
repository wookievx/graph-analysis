package io.graph.parser

import java.nio.file.{Files, Path, Paths}
import java.util.stream.Collectors

import io.graph.model.Graph

import cats.syntax.all._
import cats.instances.map._
import cats.instances.set._

import scala.collection.JavaConverters._
import scala.io.Source

object StringParser {

  def readGraph(directory: String): Graph[String, Double] = {
    def raw = for {
      file <- Files.walk(Paths.get(directory)).collect(Collectors.toList[Path]).asScala if !file.toFile.isDirectory
      p = Source.fromFile(file.toFile)
        .getLines()
        .map { s =>
          s.split(",") match {
            case Array(ns, w, _*) =>
              ns.toInt -> w
            case d =>
              throw new IllegalArgumentException(s"Unexpected string after splitting: ${d.toList}")
          }
        }
    } yield file.getFileName.toString.split('.').head -> p

    val maxes =
      raw.map({
        case (p, it) =>
          val set = it.toSet
          p -> {
            if (set.isEmpty) Set.empty[(Double, String)]
            else {
              val max = set.map(_._1).max
              set.map {
                case (i, s) =>
                  ((max - i.toDouble + 1.0) / max) -> s
              }
            }
          }
      }).toMap

    maxes.foreach(println)

    val others = for {
      p <- maxes
      p2 <- p._2
    } yield p2._2 -> Set((p2._1, p._1))

    Graph(maxes |+| others)
  }


}
