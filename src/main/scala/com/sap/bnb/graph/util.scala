/*
 * SPDX-FileCopyrightText: SAP SE or an SAP affiliate company and bayesian-network-builder contributors
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.sap.bnb.graph

/**
 * @author Giancarlo Frison <giancarlo.frison@sap.com>
 */
object util {

  implicit class OptT2[A, B](t: (Option[A], Option[B])) {
    def flat(): Option[(A, B)] =
      for (
        a <- t._1;
        b <- t._2
      ) yield (a, b)
  }

  implicit class OptT3[A, B, C](t: (Option[A], Option[B], Option[C])) {
    def flat(): Option[(A, B, C)] =
      for (
        a <- t._1;
        b <- t._2;
        c <- t._3
      ) yield (a, b, c)
  }

}
