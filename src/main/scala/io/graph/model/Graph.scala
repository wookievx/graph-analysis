package io.graph.model

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

case class Graph[V, E](neighbours: Map[V, Set[(E, V)]]) {
  def vertexNumber: Int = neighbours.size
  def vertices[Col[_]](implicit cbf: CanBuildFrom[Nothing, V, Col[V]]): Col[V] = neighbours.keysIterator.to[Col]
  def neighbourVertex(vertex: V): Option[Set[V]] = neighbours.get(vertex).map(_.map(_._2))
  def neighbourEdges(vertex: V): Option[Set[E]] = neighbours.get(vertex).map(_.map(_._1))
  def edgesNeighbours[Col[_]](implicit cbf: CanBuildFrom[Nothing, Edge[V, E], Col[Edge[V, E]]]): Col[Edge[V, E]] = {
    neighbours.iterator.flatMap({
      case (v, es) =>
        es.iterator.map {
          case (label, to) =>
            Edge(v, to, label)
        }
    }).to[Col]
  }
}


case class Edge[V, E](from: V, to: V, label: E)