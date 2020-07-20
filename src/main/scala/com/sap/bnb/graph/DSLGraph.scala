package com.sap.bnb.graph

import com.sap.bnb.bn.{BE, CPT1, CPT2, Sure}
import com.sap.bnb.dsl.{From, To}

/**
 * @author Giancarlo Frison <giancarlo.frison@sap.com>
 */
object DSLGraph {
  def apply(dict: Map[String, Set[Any]]) = {
    val (nodes, priors) = GraphTransformer(dict)
    //map all variables types
    val cases: Map[String, Set[Any]] = dict.foldLeft(Map.empty[String, Set[Any]]) {
      case (acc, (node: String, rels: Set[Any])) => acc ++ rels.map {
        case From((_, _, cpt: CPT2[Any, Any, Any])) => (node, cpt.flatMap(_._2.chances.keySet).toSet)
        case From((_, cpt: CPT1[Any, Any])) => (node, cpt.flatMap(_._2.chances.keySet).toSet)
        case To((cpt: CPT1[Any, Any], to: String)) => (node, cpt.keys.toSet)
        case To((cpt: CPT2[Any, Any, Any], a: String, b: String)) => (node, cpt.values.flatMap(_.chances.keySet).toSet)
        case x: BE[Any] => (node, x.chances.keySet)
      }.toMap
    }
    new DSLGraph(nodes, cases, priors, _ => None)
  }
}

/**
 *
 * @param nodes  graph's nodes
 * @param cases  mapping of values of all random variables
 * @param priors prior values
 */
class DSLGraph(val nodes: Map[String, BNode], val cases: Map[String, Set[Any]], val priors: CPT1[String, Any], previous: String => Option[BE[Any]]) {

  /**
   * returns calculated values of forward probability
   */
  val deduce: (String, CPT1[String, Any]) => CPT1[String, Any] = (name, session) => session.get(name) match {
    case Some(_) => session
    case None => nodes(name).source
      .flatMap(s => s(sub => session.get(sub).orElse(deduce(sub, session).get(sub))))
      .map(newValue => session + ((name, newValue)))
      .getOrElse(session)
  }

  /**
   * returns only a list of calculated posteriors (inverse probability)
   */
  val induce: (String, CPT1[String, Any], Set[String]) => Map[String, BE[Any]] = (nodeName, session, evidences) => {
    nodes(nodeName).posterior match {
      case Some(posterior) => {
        val children = posterior.ends().flatMap(induce(_, session, evidences)).toMap
        posterior(end => children
          .get(end)
          .orElse(deduce(end, session).get(end))
        )
          .map(v => children + (nodeName -> v))
          .getOrElse(children)
      }
      case None => Map.empty
    }
  }

  def solve[T](toSolve: String): Iteration[T] = Iteration(
    value = deduce(toSolve, priors).get(toSolve).map(_.asInstanceOf[BE[T]]),
    next = new DSLGraph(nodes, cases, priors, solve(_).value))

  def evidences(evidences: (String, Any)*) = new {
    val evs: Map[String, BE[Any]] = evidences.map {
      case (name: String, x: BE[Any]) => (name, x)
      case (name: String, x: Any) => (name, Sure(x, cases(name)))
    }.toMap

    def solve[T](toSolve: String): Iteration[T] = {
      val forwardValues = deduce(toSolve, priors ++ evs)
      val posteriors = induce(toSolve, forwardValues, evs.keySet)
      val result = (forwardValues ++ posteriors).get(toSolve).asInstanceOf[Option[BE[T]]]
      Iteration(result, new DSLGraph(nodes, cases, forwardValues ++ posteriors, solve(_).value))
    }

  }

  case class Iteration[T](value: Option[BE[T]], next: DSLGraph)

}


