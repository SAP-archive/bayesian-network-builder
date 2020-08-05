/*
 * SPDX-FileCopyrightText: 2020 SAP SE or an SAP affiliate company and bayesian-network-builder contributors
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.sap.bnb.calc

import com.sap.bnb.bn.Flip
import org.scalatest.FunSuite
import org.scalatest._

class BayesTest extends FunSuite with Matchers {
  test("burglar alarm") {
    val example = Bayes(Flip(.01), Map(true -> Flip(.98), false -> Flip(.01)))
    assert(example.posterior()(true).chances(true) === (.497 +- .01))
  }

  test("posterior 2 steps") {
    val b2a = Map(true -> Flip(.98), false -> Flip(.01))
    val a2w = Map(true -> Flip(.7), false -> Flip(.1))
    val a = b2a.from(Flip(.01))
    val w = a2w.from(a)
    val bposterior = a.posterior()(w.posterior()(true))
    assert(bposterior.chances(true) === (.06 +- .002))
  }
}
