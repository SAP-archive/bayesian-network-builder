/*
 * SPDX-FileCopyrightText: SAP SE or an SAP affiliate company and bayesian-network-builder contributors
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.sap.bnb.calc

import com.sap.bnb.bn.{BE, CPT1, Sure}

/**
 * @author Giancarlo Frison <giancarlo.frison@sap.com>
 */
object Splitter {

  def apply[A, B, C](prior: BE[A], cpt1: CPT1[A, B], cpt2: CPT1[A, C], evidence1: BE[B], evidence2: BE[C]): BE[A] = {
    val b = cpt1.from(prior)
    val c = cpt2.from(prior)
    val cl: Map[A, Double] = prior.chances.map(aa => {
      (aa._1, cpt1(aa._1).chances.flatten(bb => cpt2(aa._1).chances.map(cc => {
        evidence2.chances(cc._1) * cc._2 * evidence1.chances(bb._1) * bb._2 * aa._2 / b.chances(bb._1) / c.chances(cc._1)
      })).sum)
    })
    new BE((cl.keySet zip Norm(cl.values)).toMap)
  }

  def apply[A, B, C](cpt1: CPT1[A, B], cpt2: CPT1[A, C]) = new {
    def prior(a: BE[A]) = new Splitter[A, B, C](a, cpt1, cpt2)
  }

  class Splitter[A, B, C](a: BE[A], cpt1: CPT1[A, B], cpt2: CPT1[A, C]) {
    def posterior(evidence1: B, evidence2: C): BE[A] = posterior(Sure(evidence1, cpt1.values.flatMap(_.chances.keySet).toSet), Sure(evidence2, cpt2.values.flatMap(_.chances.keySet).toSet))

    def posterior(evidence1: BE[B], evidence2: BE[C]) = Splitter(a, cpt1, cpt2, evidence1, evidence2)
  }

}
