package io.graph.alg
import cats.kernel.Semigroup
import cats.{Monoid, Order}
import cats.syntax.all._
import io.graph.model.Graph

import scala.collection.mutable.{Map => MMap}
import scala.language.implicitConversions

object BellmanFordAlg {

  def bellmanFordImpl[V, E: Monoid: Order](graph: Graph[V, E], from: V): Map[V, OrInf[E]] = {
    val distances = MMap(graph.vertices[Seq].map(_ -> inf[E]):_*)
    distances(from) = Monoid[E].empty.liftInf

    for {
      _ <- (0 until graph.vertexNumber - 1).toIterator
      edge <- graph.edgesNeighbours[Iterator]
    } if (distances(edge.to) > (distances(edge.from) |+| edge.label.liftInf)) {
      distances(edge.to) = distances(edge.from) |+| edge.label.liftInf
    }

    distances.toMap
  }

  private object InfMarker {
    override def toString: String = "INF"
  }

  def inf[A]: OrInf[A] = new OrInf(InfMarker)

  def lift[A](value: A): OrInf[A] = new OrInf(value)

  implicit class InfOps[A](private val a: A) extends AnyVal {
    def liftInf: OrInf[A] = lift(a)
  }

  final class OrInf[+A](private val rawValue: Any) extends AnyVal {

    def isInf: Boolean = rawValue == InfMarker

    def toFinite: Option[A] = rawValue match {
      case InfMarker => None
      case t => t.asInstanceOf[A].some
    }

    override def toString: String = rawValue.toString

  }

  implicit def order[A: Order]: Order[OrInf[A]] =
    Order from { (l1, l2) =>
      if (l1.isInf && l2.isInf) 0
      else if (l1.isInf) 1
      else if (l2.isInf) -1
      else l1.toFinite.get compare l2.toFinite.get
    }

  implicit def semiGroup[A: Semigroup]: Semigroup[OrInf[A]] = Semigroup instance { (l, r) =>
    if (l.isInf || r.isInf) inf
    else (l.toFinite.get |+| r.toFinite.get).liftInf
  }

}
