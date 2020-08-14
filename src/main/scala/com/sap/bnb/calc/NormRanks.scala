/*
 * SPDX-FileCopyrightText: 2020 SAP SE or an SAP affiliate company and bayesian-network-builder contributors
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.sap.bnb.calc

/**
  * Normalized Ranks
  * norm = (log(x) - log(min))/ (log(max) - log(min))
  * range 0 / 1
  *
  * @author Giancarlo Frison <giancarlo.frison@sap.com>
  */
class NormRanks[T](it: Iterable[T])(implicit n: Numeric[T]) {

  import n._

  val s = it.map(c => math.log(c.toDouble))
  val min = s.min
  val max = s.max

  def apply(x: T): Double = (math.log(x.toDouble) - min) / (max - min)
}

object NormRanks {
  def apply[T](it: Iterable[T])(implicit n: Numeric[T]): NormRanks[T] =
    new NormRanks(it)
}
