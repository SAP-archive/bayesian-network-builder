/*
 * SPDX-FileCopyrightText: 2020 SAP SE or an SAP affiliate company and bayesian-network-builder contributors
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.sap.bnb.calc

import com.sap.bnb.bn.{Flip, Sure}
import org.scalatest.{FunSuite, Matchers}

class ColliderTest extends FunSuite with Matchers {
  val f = Map(
    (true, true) -> Flip(.95),
    (true, false) -> Flip(.85),
    (false, true) -> Flip(.9),
    (false, false) -> Flip(.02)
  )

  test("sickness") {
    println("prova:" + Flip(.1).chain(true -> Flip(.887), false -> Flip(.108)))
    assert(f.from(Flip(.1), Flip(.1)).chances(false) === (.8168 +- .03))
  }

  test("cpt inverse") {
    assert(
      Collider(Flip(.1), Flip(.1), f, Sure(false)).chances(
        false
      ) === (.98 +- .01)
    )
  }
}
