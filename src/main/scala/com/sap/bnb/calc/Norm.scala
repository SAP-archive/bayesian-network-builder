/*
 * SPDX-FileCopyrightText: SAP SE or an SAP affiliate company and bayesian-network-builder contributors
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.sap.bnb.calc

/**
 * @author Giancarlo Frison <giancarlo.frison@sap.com>
 */
object Norm {
  def apply(ds: Array[Double]): Seq[Double] = apply(ds.toSeq)

  def apply(y: Seq[Double]): Seq[Double] = {
    val sum = y.sum

    y.map(_ / sum)
  }

  def apply(ds: Iterable[Double]): Seq[Double] = apply(ds.toSeq)

}

object Rescale {
  def apply(ds: Array[Double]): Seq[Double] = apply(ds.toSeq)

  def apply(y: Seq[Double]): Seq[Double] = {
    val min: Double = y.min
    val max: Double = y.max

    y.map(d => (d - min) / (max - min))
  }

  def apply(ds: Iterable[Double]): Seq[Double] = apply(ds.toSeq)

}

object Softmax {
  def apply[F[Any] <: Iterable[Any]](ds: F[Double]): F[Double] = {
    val exps: F[Double] = ds.map(d => math.exp(d)).asInstanceOf[F[Double]]
    val se: Double = exps.fold[Double](0d) { case (a, b) => a + b }
    exps.map(d => d / se).asInstanceOf[F[Double]]
  }
}
