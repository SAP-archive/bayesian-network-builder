/*
 * SPDX-FileCopyrightText: 2020 Giancarlo Frison <giancarlo.frison@sap.com>
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.sap.bnb.graph

import com.sap.bnb.bn.{BE, Flip}
import com.sap.bnb.dsl._
import org.scalatest.{FunSuite, Matchers}

class dslTest extends FunSuite with Matchers {

  test("dsl") {
    val a: DSLGraph = druid {
      "ciao" <~ Flip(.2)
    }
    assert(a.priors.values.head.chances(true) === .2)
  }
  test("leaves") {
    val a = druid {
      "begin" ~ (true -> Flip(.2)) ~> "end"
    }
    assert(a.nodes.keySet === Set("begin", "end"))
  }
  test("cases") {
    val a = druid {
      "begin" ~ (true -> Flip(.2), false -> Flip(.9)) ~> "end"
    }
    println(a.cases)
    assert(a.cases("begin") === Set(true, false))
    assert(a.cases("end") === Set(true, false))
  }
  test("deduce single") {
    val a = druid {
      "ciao" <~ Flip(.9)
      "ciao" ~ (true -> Flip(.9), false -> Flip(.2)) ~> "riciao"
    }
    assert(a.nodes.keySet === Set("ciao", "riciao"))
    val res: Option[BE[Boolean]] = a.solve("riciao").value
    res.map(_.chances(true)).get should be > .8
  }
  test("deduce single no posterior") {
    val a = druid {
      "ciao" ~ (true -> Flip(.9), false -> Flip(.2)) ~> "riciao"
    }
    val ret: Option[BE[Boolean]] = a.evidences("ciao" -> true).solve("riciao").value
    ret.map(_.chances(true)).get should be > .8
  }
  test("posterior single") {
    val a = druid {
      "ciao" <~ Flip(.1)
      "ciao" ~ (true -> Flip(.9), false -> Flip(.2)) ~> "riciao"
    }
    val ret: Option[BE[Boolean]] = a.evidences("riciao" -> true).solve("ciao").value
    ret.map(_.chances(true)).get should be(.33 +- .01)
  }
  test("posterior single prior as evidence") {
    val a = druid {
      "ciao" ~ (true -> Flip(.9), false -> Flip(.2)) ~> "riciao"
    }
    val ret: Option[BE[Boolean]] = a.evidences("ciao" -> Flip(.1), "riciao" -> true).solve("ciao").value
    ret.map(_.chances(true)).get should be(.33 +- .01)
  }
  test("collider") {
    val a = druid {
      "burglar" <~ Flip(.001)
      "earthquake" <~ Flip(.002)
      "alarm" <~ ("burglar", "earthquake",
        (true, true) -> Flip(.95),
        (true, false) -> Flip(.94),
        (false, true) -> Flip(.29),
        (false, false) -> Flip(.001))
    }
    val alarm = a.solve[Boolean]("alarm").value.get
    alarm.chances(true) should be(.0025 +- .0001)
  }
  test("collider posterior") {
    val a = druid {
      "burglar" <~ Flip(.001)
      "earthquake" <~ Flip(.002)
      "alarm" <~ ("burglar", "earthquake",
        (true, true) -> Flip(.95),
        (true, false) -> Flip(.94),
        (false, true) -> Flip(.29),
        (false, false) -> Flip(.001))
    }
    val burglar = a.evidences("alarm" -> true, "earthquake" -> false)
      .solve[Boolean]("burglar").value.get
    burglar.chances(true) should be(.45 +- .01)
  }
  test("john prior") {
    val a = druid {
      "burglar" <~ Flip(.001)
      "earthquake" <~ Flip(.002)
      "alarm" <~ ("burglar", "earthquake",
        (true, true) -> Flip(.95),
        (true, false) -> Flip(.94),
        (false, true) -> Flip(.29),
        (false, false) -> Flip(.001))
      "alarm" ~ (true -> Flip(.9), false -> Flip(.05)) ~> "john"
      "alarm" ~ (true -> Flip(.7), false -> Flip(.01)) ~> "mary"
    }
    val john = a.solve[Boolean]("john").value.get
    john.chances(true) should be(.052 +- .001)
  }
  test("simple posterior") {
    val a = druid {
      "a" <~ Flip(.9)
      "a" ~ (true -> Flip(.1), false -> Flip(.9)) ~> "b"
    }
    val posterior: Option[BE[Boolean]] = a.evidences("b" -> Flip(.9)).solve("a").value
    posterior shouldBe defined
    println(posterior.get)
    posterior.get.chances(true) should be(.54 +- .01)
  }
  test("john posterior") {
    val graph = druid {
      "burglar" <~ Flip(.001)
      "earthquake" <~ Flip(.002)
      "alarm" <~ ("burglar", "earthquake",
        (true, true) -> Flip(.95),
        (true, false) -> Flip(.94),
        (false, true) -> Flip(.29),
        (false, false) -> Flip(.001))
      "alarm" ~ (true -> Flip(.9), false -> Flip(.05)) ~> "john"
      "alarm" ~ (true -> Flip(.7), false -> Flip(.01)) ~> "mary"
    }
    val burglar = graph.evidences("john" -> true, "mary" -> true)
      .solve[Boolean]("burglar").value.get
    burglar.chances(true) should be(.28 +- .01)
    val burglar2 = graph.evidences("john" -> true, "mary" -> false)
      .solve[Boolean]("burglar").value.get
    burglar2.chances(true) should be(.005 +- .001)

    println("chances burglary: " + f"${burglar2.chances(true) * 100}%2.1f%%")
  }
}
