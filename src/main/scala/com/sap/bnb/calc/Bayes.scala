package com.sap.bnb.calc

import com.sap.bnb.bn.{BE, Sure}

/**
 * https://www.dbs.ifi.lmu.de/Lehre/MaschLernen/SS2014/Skript/BayesNets2014.pdf
 *
 * @author Giancarlo Frison <giancarlo.frison@sap.com>
 */
object Bayes {
  def apply[A, B](a: BE[A], cond: Map[A, BE[B]]): Bayes[A, B] = new Bayes[A, B](a, cond)
}

/**
 *
 * @param a   P(A)
 * @param a2b P(B | A)
 * @tparam A
 * @tparam B
 */
class Bayes[A, B](val a: BE[A], val a2b: Map[A, BE[B]]) extends BE[B](a.chain(a2b).chances) {
  val inv: Map[B, BE[A]] = chances.keySet.map(bcase => (bcase, new BE(a.chances.map(av => (av._1, a2b(av._1).chances(bcase) * a.chances(av._1) / chances(bcase)))))).toMap

  /**
   *
   * @return P(A | B) = cond * a / this
   */
  def posterior() = {
    new BayesPosterior[A, B](inv)
  }
}

class BayesPosterior[A, B](val cond: Map[B, BE[A]]) {
  def apply(b: BE[B]) = b.chain(cond)

  def apply(evidence: B) = Sure(evidence, cond.keySet).chain(cond)
}

