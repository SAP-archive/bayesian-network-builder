/*
 * SPDX-FileCopyrightText: 2020 SAP SE or an SAP affiliate company and bayesian-network-builder contributors
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.sap.bnb.calc

import scala.math.{log, max}

/**
  * @author Giancarlo Frison <giancarlo.frison@sap.com>
  */
object Entropy {

  import com.sap.bnb.bn._

  def apply(e: BE[_]): Double =
    -e.chances.map(t => log(max(t._2, 1e-5)) * t._2).avg

}
