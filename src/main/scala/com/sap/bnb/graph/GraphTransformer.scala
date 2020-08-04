/*
 * SPDX-FileCopyrightText: SAP SE or an SAP affiliate company and bayesian-network-builder contributors
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.sap.bnb.graph

import com.sap.bnb.bn.{BE, CPT1, CPT2}
import com.sap.bnb.calc.{Collider, Splitter}
import com.sap.bnb.dsl.{From, To}
import com.sap.bnb.graph.GraphTransformer.NMap
import com.sap.bnb.graph.util._

/**
 * @author Giancarlo Frison <giancarlo.frison@sap.com>
 */
object GraphTransformer {
  type NMap = String => Option[BE[Any]]

  def apply(dict: Map[String, Set[Any]]) = dict.foldLeft((Map.empty[String, BNode], Map.empty[String, BE[Any]])) {
    case (acc, (node: String, rels: Set[Any])) => {
      val source = rels.count(_.isInstanceOf[From[_]]) match {
        case 0 => None
        case 1 => {
          val from = rels.filter(_.isInstanceOf[From[_]]).head.asInstanceOf[From[_]]
          Some(from.value match {
            case (from1: String, from2: String, cpt: CPT2[Any, Any, Any]) => new CPT2Source(cpt, from1, from2)
            case (name: String, cpt: CPT1[_, _]) => new CPT1Source(cpt, name)
          })
        }
        case _ => throw new IllegalArgumentException("not yet managed the merge")
      }
      val poster = rels.count(_.isInstanceOf[To[_]]) match {
        case 0 => None
        case 1 => {
          val to = rels.filter(_.isInstanceOf[To[_]]).head.asInstanceOf[To[_]]
          Some(to.value match {
            case (cpt: CPT1[Any, Any], target: String) => new SinglePost(cpt, node, target)
            case (cpt: CPT2[Any, Any, Any], a: String, b: String) => new Collider2Post(cpt, node, a, b)
            case unknown => throw new IllegalArgumentException(s"TO case not managed:$unknown")
          })
        }
        case 2 => {
          val tts = rels.flatMap {
            case To((cpt: CPT1[Any, Any], target: String)) => Some((cpt, target))
            case _ => None
          }.toList
          Some(new Split2Post(node, tts(0)._1, tts(1)._1, tts(0)._2, tts(1)._2))
        }
      }
      val prior = rels.find(_.isInstanceOf[BE[_]]).map(_.asInstanceOf[BE[Any]])
      (acc._1 + ((node, BNode(source, poster))), prior.map(p => acc._2 + ((node, p))).getOrElse(acc._2))
    }
  }
}

case class BNode(source: Option[Bayesier], posterior: Option[Bayesier])

trait Bayesier {
  def apply(m: NMap): Option[BE[Any]]

  def ends(): Set[String]
}

class CPT1Source[A, B](cpt: CPT1[A, B], n1: String) extends Bayesier {
  def apply(m: NMap): Option[BE[Any]] = m(n1).map(a => cpt.from(a.asInstanceOf[BE[A]]).asInstanceOf[BE[Any]])

  override def ends(): Set[String] = Set(n1)
}

class CPT2Source(cpt: CPT2[Any, Any, Any], n1: String, n2: String) extends Bayesier {
  def apply(m: NMap): Option[BE[Any]] = (m(n1), m(n2)).flat().map(t => cpt.from(t._1, t._2))

  override def ends(): Set[String] = Set(n1, n2)
}

class Split2Post(prior: String, cpt1: CPT1[Any, Any], cpt2: CPT1[Any, Any], n1: String, n2: String) extends Bayesier {
  override def apply(m: NMap): Option[BE[Any]] = (m(prior), m(n1), m(n2)).flat().map {
    case (a: BE[_], b: BE[_], c: BE[_]) => Splitter(a, cpt1, cpt2, b, c)
  }

  override def ends(): Set[String] = Set(n1, n2)
}

class SinglePost(cpt: CPT1[Any, Any], prior: String, evidence: String) extends Bayesier {
  override def apply(m: NMap): Option[BE[Any]] = (m(prior), m(evidence)).flat().map {
    case (a: BE[_], b: BE[_]) => cpt.from(a).posterior()(b)
  }

  override def ends(): Set[String] = Set(evidence)
}

class Collider2Post(cpt: CPT2[Any, Any, Any], n1: String, n2: String, evidence: String) extends Bayesier {
  override def apply(m: NMap): Option[BE[Any]] = (m(n1), m(n2), m(evidence)).flat().map {
    case (a: BE[_], b: BE[_], c: BE[_]) => Collider(a, b, cpt, c)
  }

  override def ends(): Set[String] = Set(evidence)
}
