/*
 * SPDX-FileCopyrightText: 2020 SAP SE or an SAP affiliate company and bayesian-network-builder contributors
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.sap.bnb.calc

/**
  * @author Giancarlo Frison <giancarlo.frison@sap.com>
  */
object EuclideanDistance {

  import com.sap.bnb.bn._

  def apply[A1](e1: BE[A1]): BE[A1] => Double =
    (e2: BE[A1]) => e1.chances.map(f => math.abs(f._2 - e2.chances(f._1))).avg

}
