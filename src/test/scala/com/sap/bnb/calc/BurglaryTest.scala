/*
 * SPDX-FileCopyrightText: 2020 SAP SE or an SAP affiliate company and bayesian-network-builder contributors
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.sap.bnb.calc

import com.sap.bnb.bn._
import com.sap.bnb.bn.Flip
import org.scalatest.{FunSuite, Matchers}

class BurglaryTest extends FunSuite with Matchers {
  val pburglar = Flip(.001)
  val pearth = Flip(.002)
  val inAlarm = Map((true, true) -> Flip(.95),
    (true, false) -> Flip(.94),
    (false, true) -> Flip(.29),
    (false, false) -> Flip(.001))
  val inJohn = Map(true -> Flip(.9), false -> Flip(.05))
  val inMary = Map(true -> Flip(.7), false -> Flip(.01))

  test("John calls but Mary not, what is P(burglar)?") {
    val ppBurglar = Collider(pburglar, pearth, inAlarm,
      Splitter(inJohn, inMary)
        .prior(inAlarm.from(pburglar, pearth))
        .posterior(true, false))

    assert(ppBurglar.chances(false) === (.99 +- .02))

  }

}
