/*
 * SPDX-FileCopyrightText: SAP SE or an SAP affiliate company and bayesian-network-builder contributors
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.sap.bnb.calc

import com.sap.bnb.bn.Flip
import org.scalatest.{FunSuite, Matchers}

class SplitterTest extends FunSuite with Matchers {

  val alarm2john = Map(true -> Flip(.9), false -> Flip(.05))
  val alarm2mary = Map(true -> Flip(.7), false -> Flip(.01))
  test("both posteriors") {
    val withVals = Splitter(alarm2john, alarm2mary).prior(Flip(.1)).posterior(Flip(0), Flip(1))
    val withoutVals = Splitter(alarm2john, alarm2mary).prior(Flip(.1)).posterior(false, true)
    assert(withVals.chances(true) === (.45 +- .01))
    assert(withVals.chances(true) - withoutVals.chances(true) < .02)
  }
}
