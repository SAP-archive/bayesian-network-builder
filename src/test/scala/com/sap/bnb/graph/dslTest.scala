/*
 * SPDX-FileCopyrightText: 2020 SAP SE or an SAP affiliate company and bayesian-network-builder contributors
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.sap.bnb.graph

import com.sap.bnb.bn.{BE, Flip}
import com.sap.bnb.dsl._
import org.scalatest.{FunSuite, Matchers}

class dslTest extends FunSuite with Matchers {

  test("dsl") {
    val a: DSLGraph = graph {
      "ciao" <~ Flip(.2)
    }
    assert(a.priorsF().values.head.chances(true) === .2)
  }
  test("leaves") {
    val a = graph {
      "begin" ~ (true -> Flip(.2)) ~> "end"
    }
    assert(a.nodes.keySet === Set("begin", "end"))
  }
  test("cases") {
    val a = graph {
      "begin" ~ (true -> Flip(.2), false -> Flip(.9)) ~> "end"
    }
    println(a.cases)
    assert(a.cases("begin") === Set(true, false))
    assert(a.cases("end") === Set(true, false))
  }
  test("deduce single") {
    val a = graph {
      "ciao" <~ Flip(.9)
      "ciao" ~ (true -> Flip(.9), false -> Flip(.2)) ~> "riciao"
    }
    assert(a.nodes.keySet === Set("ciao", "riciao"))
    val res: Option[BE[Boolean]] = a.solve("riciao").value
    res.map(_.chances(true)).get should be > .8
  }
  test("deduce single no posterior") {
    val a = graph {
      "ciao" ~ (true -> Flip(.9), false -> Flip(.2)) ~> "riciao"
    }
    val ret: Option[BE[Boolean]] =
      a.evidences("ciao" -> true).solve("riciao").value
    ret.map(_.chances(true)).get should be > .8
  }
  test("posterior single") {
    val a = graph {
      "ciao" <~ Flip(.1)
      "ciao" ~ (true -> Flip(.9), false -> Flip(.2)) ~> "riciao"
    }
    val ret: Option[BE[Boolean]] =
      a.evidences("riciao" -> true).solve("ciao").value
    ret.map(_.chances(true)).get should be(.33 +- .01)
  }
  test("posterior single prior as evidence") {
    val a = graph {
      "ciao" ~ (true -> Flip(.9), false -> Flip(.2)) ~> "riciao"
    }
    val ret: Option[BE[Boolean]] =
      a.evidences("ciao" -> Flip(.1), "riciao" -> true).solve("ciao").value
    ret.map(_.chances(true)).get should be(.33 +- .01)
  }
  test("collider") {
    val a = graph {
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
    val a = graph {
      "burglar" <~ Flip(.001)
      "earthquake" <~ Flip(.002)
      "alarm" <~ ("burglar", "earthquake",
      (true, true) -> Flip(.95),
      (true, false) -> Flip(.94),
      (false, true) -> Flip(.29),
      (false, false) -> Flip(.001))
    }
    val burglar = a
      .evidences("alarm" -> true, "earthquake" -> false)
      .solve[Boolean]("burglar")
      .value
      .get
    burglar.chances(true) should be(.45 +- .01)
  }
  test("john prior") {
    val a = graph {
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
    val a = graph {
      "a" <~ Flip(.9)
      "a" ~ (true -> Flip(.1), false -> Flip(.9)) ~> "b"
    }
    val posterior: Option[BE[Boolean]] =
      a.evidences("b" -> Flip(.9)).solve("a").value
    posterior shouldBe defined
    println(posterior.get)
    posterior.get.chances(true) should be(.54 +- .01)
  }
  test("john posterior") {
    val g = graph {
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
    val burglar = g
      .evidences("john" -> true, "mary" -> true)
      .solve[Boolean]("burglar")
      .value
      .get
    burglar.chances(true) should be(.28 +- .01)
    val burglar2 = g
      .evidences("john" -> true, "mary" -> false)
      .solve[Boolean]("burglar")
      .value
      .get
    burglar2.chances(true) should be(.005 +- .001)

    println("chances burglary: " + f"${burglar2.chances(true) * 100}%2.1f%%")
  }
  test("dynamic posterior") {
    val dyng = graph {
      "var1" ~ (true -> Flip(.1), false -> Flip(.9)) ~> "var2"
      "var2" ~ (true -> Flip(.9), false -> Flip(.1)) ~~> "var1"
    }
    val ret = dyng.evidences("var1" -> true).solve[Boolean]("var2")
    ret.value.get.chances(true) should be(.1 +- .01)
    val r2 = ret.next.solve[Boolean]("var2")
    r2.value.get.chances(true) should be(.75 +- .01)
    val r3 = r2.next.solve[Boolean]("var2")
    r3.value.get.chances(true) should be(.33 +- .01)
  }
  test("dynamic 2") {
    val g = graph {
      "flu" ~ (true -> Flip(.95), false -> Flip(.02)) ~> "fever"
      "fever" ~ (true -> Flip(.9), false -> Flip(.05)) ~> "therm"
      "therm" ~ (true -> Flip(.95), false -> Flip(.01)) ~> "takeAspirin"
      "takeAspirin" ~ (true -> Flip(.05), false -> Flip(.9)) ~~> "fever"
    }
    val therm = g.evidences("flu" -> true).solve[Boolean]("therm")
    therm.value.get.chances(true) should be(.85 +- .01)
    val t2 = therm.next.solve[Boolean]("therm")
    t2.value.get.chances(true) should be(.22 +- .01)
  }
  test("weather") {
    val g = graph {
      "highPressure" ~ (true -> Flip(.9), false -> Flip(.2)) ~> "sunny"
      "sunny" ~ (true -> Flip(.05), false -> Flip(.8)) ~> "highHumidity"
      "highHumidity" ~ (true -> Flip(.2), false -> Flip(.9)) ~~> "highPressure"
    }
    val w1 = g.evidences("highPressure" -> true).solve[Boolean]("sunny")
    w1.value.get.chances(true) should be(.89 +- .01)
    val w2 = w1.next.solve[Boolean]("sunny")
    w2.value.get.chances(true) should be(.76 +- .01)
  }

  test("diapers"){
    val g = graph {
      "female" <~ Flip(.1)
      "female" ~ (true -> Flip(.8), false -> Flip(.2)) ~> "buyDiapers"
    }
    val f = g.evidences("buyDiapers"->true).solve[Boolean]("female")
    println(f)
  }
}
