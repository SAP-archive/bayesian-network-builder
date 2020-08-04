/*
 * SPDX-FileCopyrightText: SAP SE or an SAP affiliate company and bayesian-network-builder contributors
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.sap.bnb

import com.sap.bnb.calc._

import scala.math.{max, min}

/**
 * @author Giancarlo Frison <giancarlo.frison@sap.com>
 */
package object bn {

  type CPT1[A, B] = Map[A, BE[B]]
  type CPT2[A, B, C] = Map[(A, B), BE[C]]

  implicit class CPTOps[A, B](cpt: CPT1[A, B]) {
    def from(prior: BE[A]) = Bayes(prior, cpt)
  }

  implicit class CPT2Ops[A, B, C](cpt: CPT2[A, B, C]) {
    def from(priorA: BE[A], priorB: BE[B]): BE[C] = Collider(priorA, priorB, cpt)
  }

  type BEMap[A, B] = Seq[(A, BE[B])]


  class BE[T](val chances: Map[T, Double]) extends Equals {
    require(chances.values.forall(_ >= 0), "all probabilities must be > 0, " + chances)
    require(math.abs(1 - chances.values.sum) <= .06, s"probabilities must sum up to 1, $chances, sum:${chances.values.sum}")

    override def toString: String = chances.toSeq.sortBy(_._1)(Ordering.by(_.toString)).map(e => s"${e._1}-> .${BE.format(e._2)}").mkString(", ")


    def normWeight(v: T): Double = {
      val m = 1d / chances.size.toDouble
      if (chances(v) < m) {
        chances(v) * .5 / m
      } else {
        (1d - .5) / (1d - m) * (chances(v) - m) + .5
      }
    }

    override def canEqual(that: Any): Boolean = {
      if (that.getClass != this.getClass) {
        false
      } else {
        if (chances.canEqual(that.asInstanceOf[BE[Any]].chances)) {
          true
        } else {
          false
        }
      }
    }
  }


  implicit class Merge[T](a: BE[T]) {
    def merge(el: BE[T]): BE[T] = Conflationer(a, el)
  }


  implicit class Entropier(a: BE[_]) {
    def entropy(): Double = Entropy(a)
  }


  implicit class Euclid[T](a: BE[T]) {
    def euclid(b: BE[T]): Double = EuclideanDistance(a)(b)
  }


  implicit class BayesOps[A, B](a: BE[A]) {
    def chain(clauses: (A, BE[B])*): BE[B] = TotalProb(a, clauses)

    def chain(clauses: Map[A, BE[B]]): BE[B] = TotalProb(a, clauses.toList)

    def chain(clauses: Seq[(A, BE[B])])(implicit d: DummyImplicit): BE[B] = TotalProb(a, clauses)

  }


  object BE {

    def format(n: Double): String = "%02d".format(((math floor n * 100) / 100 * 100).toInt)
  }

  object Cards {
    def apply[T](clauses: (T, Double)*) = new BE((clauses.map(_._1) zip Norm(clauses.map(_._2))).toMap)
  }

  object Sure {

    def apply(v: Boolean): BE[Boolean] = apply(v, Set(true, false))

    def apply[T](k: T, all: Set[T]): BE[T] = new BE[T](all.map(s => {
      (s, {
        if (s == k) {
          .999
        } else {
          1e-3 / all.size
        }
      })
    }).toMap)
  }


  object Uniform {
    def apply[T](s: Seq[T]) = new BE(s.map(v => (v, 1d / s.length)).toMap)
  }

  object Flip {
    def apply(trueProbability: Double) = {
      require(trueProbability >= 0 && trueProbability <= 1)
      val p2 = max(trueProbability, 1e-05)
      val p = min(p2, 1 - 1e-05)
      new BE(Map(true -> p, false -> (1 - p)))
    }
  }

  object Convert {
    def apply[A1](ls: Seq[BE[A1]]): BE[A1] = chainRedux(ls)


    private def chainRedux[T](tuples: Seq[BE[T]]): BE[T] = tuples.reduce(Conflationer(_, _))


  }

  implicit def seqDoubleAvg(v: Iterable[Double]) = new AvgIterable(v)


}
