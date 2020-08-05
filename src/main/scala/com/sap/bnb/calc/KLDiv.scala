/*
 * SPDX-FileCopyrightText: 2020 SAP SE or an SAP affiliate company and bayesian-network-builder contributors
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.sap.bnb.calc

import com.sap.bnb.bn.BE

/**
 * @author Giancarlo Frison <giancarlo.frison@sap.com>
 *
 * https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence
 */
object KLDiv {
  def apply[T](p: BE[T], q: BE[T]): Double = (p.chances.keySet ++ q.chances.keySet).toSeq.map(k => p.chances(k) * math.log(p.chances(k) / q.chances(k))).sum
}

/**
 * https://en.wikipedia.org/wiki/Jensen%E2%80%93Shannon_divergence
 */
object JSDiv {
  def apply[T](p: BE[T], q: BE[T]): Double = {
    val m = new BE((p.chances.keySet ++ q.chances.keySet).toSeq.map(k => (k, (p.chances(k) + q.chances(k)) / 2d)).toMap)
    (KLDiv(p, m) + KLDiv(q, m)) / 2d
  }
}
