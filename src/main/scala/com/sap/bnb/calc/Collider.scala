/*
 * SPDX-FileCopyrightText: 2020 Giancarlo Frison <giancarlo.frison@sap.com>
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.sap.bnb.calc

import com.sap.bnb.bn.{BE, CPT2}

/**
 * @author Giancarlo Frison <giancarlo.frison@sap.com>
 */
object Collider {

  def apply[A, B, C](a: BE[A], b: BE[B], cpt: CPT2[A, B, C]): BE[C] = {
    val aa = a.chances
    val bb = b.chances
    require(cpt.map(_._1._1).toSet.size == aa.keySet.size, "A elements do not match")
    require(cpt.map(_._1._2).toSet.size == bb.keySet.size, "B elements do not match")
    require(cpt.map(_._2.chances.size).toSet.size == 1, "BE[C] not fully defined")
    val ch = cpt.values.head.chances
    new BE(ch.keySet.map(c => (c, cpt.foldLeft(0d)((acc, cp) => acc + (aa(cp._1._1) * bb(cp._1._2) * cp._2.chances(c))))).toMap)
  }

  def apply[A, B, C](a: BE[A], b: BE[B], cpt: Map[(A, B), BE[C]], evidence: BE[C]): BE[A] = {
    val cptSeq = cpt.map(k => (k._1._1, k._1._2, k._2)).toSeq
    val a2c: Map[A, BE[C]] = cptSeq.groupBy(_._1)
      .map(t => (t._1, b.chain(t._2.map(k => (k._2, k._3)).toMap)))
    Bayes(a, a2c).posterior()(evidence)
  }


}
