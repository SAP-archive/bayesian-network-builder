/*
 * SPDX-FileCopyrightText: SAP SE or an SAP affiliate company and bayesian-network-builder contributors
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.sap.bnb.calc

import com.sap.bnb.bn.{BE, BEMap}

trait TotalProb[A, B] {
  def apply(el: BE[A], clauses: (A, BE[B])*): BE[B]
}

/**
 * https://en.wikipedia.org/wiki/Law_of_total_probability
 *
 * @author Giancarlo Frison <giancarlo.frison@sap.com>
 */
object TotalProb {
  def apply[A, B](el: BE[A], clauses: BEMap[A, B]): BE[B] = {
    if (el.chances.size != clauses.map(_._1).toSet.size) {
      throw new IllegalArgumentException(s"not correct number of mapping elements. el:${el}, clauses:${clauses.mkString(", ")}")
    }
    val targetSyms: Set[B] = clauses.map(_._2).flatMap(_.chances.keySet).toSet
    val posteriors: Map[B, Double] = targetSyms.map(t => (t, clauses.map(f => f._2.chances(t) * el.chances(f._1)).sum)).toMap
    new BE[B]((posteriors.keys zip Norm(posteriors.values)).toMap)
  }

  implicit val chainer = new TotalProb[Boolean, Symbol] {
    override def apply(el: BE[Boolean], clauses: (Boolean, BE[Symbol])*): BE[Symbol] = TotalProb(el, clauses)
  }
}

