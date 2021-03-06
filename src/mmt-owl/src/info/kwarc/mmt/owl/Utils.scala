package info.kwarc.mmt.owl

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import org.semanticweb.owlapi.model._

object Utils {
  def IRILast(i: IRI): String = {
    val uri = utils.URI.fromJava(i.toURI)
    uri.fragment match {
      case Some(s) => s
      case None => uri.path.last
    }
  }
}

object OWLOMS {
  val path = new DPath(utils.URI("http", "latin.omdoc.org") / "logics" / "description" / "owl" / "owl.omdoc")

  def apply(m: String, n: String) = {
    OMID(path ? m ? n)
  }

  def unapply(t: Term): Option[(String, String)] = {
    t match {
      case OMID((this.path ? !(SimpleStep(m))) ?? !(SimpleStep(n))) => Some((m, n))
      case _ => None
    }
  }
}

object OWL2OMS {
  val path = new DPath(utils.URI("http", "latin.omdoc.org") / "logics" / "description" / "owl" / "owl2.omdoc")

  def apply(m: String, n: String) = {
    OMID(path ? m ? n)
  }

  def unapply(t: Term): Option[(String, String)] = {
    t match {
      case OMID((this.path ? !(SimpleStep(m))) ?? !(SimpleStep(n))) => Some((m, n))
      case _ => None
    }
  }
}
