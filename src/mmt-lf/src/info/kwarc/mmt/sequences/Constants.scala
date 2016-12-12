package info.kwarc.mmt.sequences

import info.kwarc.mmt.lf._
import info.kwarc.mmt.api._
import utils._
import objects._
import uom._

import Nat._

object Sequences {
   val _base = Typed._base
   val _path = _base ? "Sequences"
   
   object rep extends BinaryConstantScala(_path, "rep")
   
   object index extends BinaryConstantScala(_path, "index")

   object ellipsis {
      val path = _path ? "ellipsis"
      def apply(to: Term, index: LocalName, body: Term): Term =
         OMBIND(OMS(path), OMV(index) % to, body)
      def unapply(t: Term): Option[(Term, LocalName, Term)] = t match {
         case OMBIND(OMS(this.path), Context(VarDecl(index, Some(to), None, _)), body) =>
           Some((to, index, body))
         case _ => None
      }
   }
   
   object flatseq extends FlexaryConstantScala(_path, "flatseq")
   
   object comp extends UnaryConstantScala(_path, "comp")
}