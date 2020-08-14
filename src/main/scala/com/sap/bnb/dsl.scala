/*
 * SPDX-FileCopyrightText: 2020 SAP SE or an SAP affiliate company and bayesian-network-builder contributors
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.sap.bnb

import com.sap.bnb.bn.BE
import com.sap.bnb.graph.DSLGraph

import scala.collection.MultiDict
import scala.util.DynamicVariable

/**
  * @author Giancarlo Frison <giancarlo.frison@sap.com>
  */
object dsl {

  private val ws: DynamicVariable[MultiDict[String, Any]] = new DynamicVariable(
    null
  )

  def graph(body: => Any) = {
    ws.withValue(MultiDict[String, Any]()) {
      body
      DSLGraph(ws.value.keySet.map(k => (k, ws.value.get(k).toSet)).toMap)
    }
  }

  private def pimpName(name: String, obj: Any) = {
    ws.value = ws.value.concat(MultiDict(name -> obj))
  }

  implicit def dslName(arg: String) = new DSL(arg)

  class DSL(node: String) {

    def <~(prior: BE[_]): Unit = {
      pimpName(node, prior)
    }

    def ~[A, B](clauses: (A, BE[B])*) = new Line[A, B](node, clauses)

    def <~[A, B, C](a1: String, a2: String, cpt: ((_, _), BE[_])*): Unit = {
      val cptm =
        cpt.map(t => (t._1._1, t._1._2) -> t._2.asInstanceOf[BE[Any]]).toMap
      pimpName(node, From((a1, a2, cptm)))
      pimpName(a1, To((cptm, a2, node)))
      pimpName(a2, To((cptm, a1, node)))
    }
  }

  class Line[A, B](node: String, clauses: Seq[(A, BE[B])]) {

    def ~>(target: String) =
      new {
        pimpName(node, To((clauses.toMap, target)))
        pimpName(target, From((node, clauses.toMap)))

        def ~[A, B](clauses2: (A, BE[B])*) = new Line[A, B](target, clauses2)
      }
  }

  class To[T](val value: T)

  class From[T](val value: T)

  object To {
    def apply[T](value: T): To[T] = new To(value)

    def unapply[T](arg: To[T]): Option[T] = Some(arg.value)
  }

  object From {
    def apply[T](value: T): From[T] = new From(value)

    def unapply[T](arg: From[T]): Option[T] = Some(arg.value)
  }

}
