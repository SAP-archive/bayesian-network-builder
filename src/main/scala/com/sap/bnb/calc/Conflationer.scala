package com.sap.bnb.calc

import com.sap.bnb.bn.BE

/**
 * https://stats.stackexchange.com/questions/155817/combining-probabilities-information-from-different-sources
 *
 * @author Giancarlo Frison <giancarlo.frison@sap.com>
 */
object Conflationer {
  def apply[A](left: BE[A], right: Option[BE[A]]): BE[A] = right.map(apply(left, _)).getOrElse(left)

  def apply[A](left: BE[A], right: BE[A]): BE[A] = {
    if (left.chances.size != right.chances.size) {
      throw new IllegalArgumentException(s"not equal number of elements. left:${left}, right:${right}")
    }
    new BE(left.chances.map(lt => (lt._1, (right.chances(lt._1) + lt._2) / 2)))
  }
}

