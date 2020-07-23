/*
 * SPDX-FileCopyrightText: 2020 Giancarlo Frison <giancarlo.frison@sap.com>
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.sap.bnb.calc

/**
 * @author Giancarlo Frison <giancarlo.frison@sap.com>
 */
class AvgIterable(val v: Iterable[Double]) {
  def avg = v.foldLeft((0.0, 1d))((acc, i) => (acc._1 + (i - acc._1) / acc._2, acc._2 + 1d))._1
}
