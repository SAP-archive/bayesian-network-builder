/*
 * SPDX-FileCopyrightText: 2020 SAP SE or an SAP affiliate company and bayesian-network-builder contributors
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.sap.bnb.calc

import com.sap.bnb.bn.{BE, Cards}

import scala.math.{abs, log, sqrt}

/**
 * @author Giancarlo Frison <giancarlo.frison@sap.com>
 */
object zutil {
  def arcSinh(x: Double): Double = log(x + sqrt(x * x + 1.0))

  def softmax(ds: Array[Double]): Seq[Double] = softmax(ds.toSeq)

  def coth(x: Double): Double = ((x - 1) / x) / ((x + 1) / x)

  def arccsh(x: Double): Double = log(1 + sqrt(1 + x * x) / abs(x))

  def softmax(ds: Iterable[Double]): Seq[Double] = softmax(ds.toSeq)

  def softmax(y: Seq[Double]): Seq[Double] = {
    val exps = y.map(math.exp)
    val sum = exps.sum
    exps.map(i => i / sum)
  }

  def relax[T](p: Double, ls: Seq[T]): BE[T] = Cards(ls zip Norm((1 to ls.length).map(
    x => {
      val y = x.toDouble / ls.length - 1d / (ls.length * 2)
      println(s"$x -> $y")
      val z = sech((p - y) * 7)
      println(s"$y sech $z")
      z
    }
  )): _*)

  def sech(x: Double): Double = 1.0 / math.cosh(x)

}
