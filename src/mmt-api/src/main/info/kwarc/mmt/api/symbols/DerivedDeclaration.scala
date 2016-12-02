package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import modules._
import frontend._
import checking._
import info.kwarc.mmt.api.libraries.ElaboratedElement
import objects._

import scala.xml.Elem

/** A [[DerivedDeclaration]] is a syntactically like a nested theory.
 *  Its semantics is defined by the corresponding [[StructuralFeature]]
 */
class DerivedDeclaration(h: Term, name: LocalName, val feature: String,
                         components: List[DeclarationComponent], mt : Option[MPath] = None) extends {
   private val t = new DeclaredTheory(h.toMPath.parent, h.toMPath.name/name, mt)
} with NestedModule(h, name, t) {
   // overriding to make the type stricter
  override def module: DeclaredModule = t

  override def getComponents = components
  override def toNode : Elem = {
    <derived feature={feature} name={name.toString} base={t.parent.toString}>
      {components.map(c => <component key={c.key.toString}>
      {c.value match {
        case t: TermContainer => t.get.get.toNode
        case p: MPathContainer => p.get.get.toNode
      }}
    </component>)}{t.getDeclarations map (_.toNode)}
    </derived>
  }
  // override def toNodeElab
  override def toNode(rh: presentation.RenderingHandler) {
    rh << s"""<derived feature="$feature" name="${t.name}" base="${t.parent}">"""
    components foreach (c => {
      rh << s"""<component key="${c.key}">"""
      c.value match {
        case t: TermContainer => t.get.get.toNode(rh)
        case p: MPathContainer => p.get.get.toNode(rh)
      }
      rh << s"""</component>"""
    })
    t.getDeclarations foreach(_.toNode(rh))
    rh << "</derived>"
  }
  override def toString = {
    val s1 = {
      name match {
        case LocalName(ComplexStep(p) :: Nil) => ""
        case _ => " " + name
      }
    }
    val s2 = if (components.nonEmpty) "(" + components.map(_.value.toString).mkString(",") + ")"
        else if (components.length == 1) " " + components.head.value.toString
        else ""
    val s3 = if (t.getDeclarations.nonEmpty) " =\n" + t.innerString else ""
    feature + s1 + s2 + s3
  }

  override def translate(newHome: Term, prefix: LocalName, translator: Translator): DerivedDeclaration = {
     // translate this as a [[NestedModue]], then extend the result to a DerivedDeclaration
     val superT = super.translate(newHome, prefix, translator) // temporary, will be used to build result
     // TODO using applyDef for all term components is not right, it's unclear if any translation is possible at all
     def tl(t: Term) = translator.applyDef(Context.empty, t)
     val compsT = components map {
       case DeclarationComponent(k,tc: TermContainer) =>
         DeclarationComponent(k, tc map tl)
       case DeclarationComponent(k,nc: notations.NotationContainer) =>
         DeclarationComponent(k, nc.copy)
     }
     // splice super in to res
     val res = new DerivedDeclaration(superT.home, superT.name, feature, compsT, mt)
     superT.module.getDeclarations.foreach {d =>
       res.module.add(d)
     }
     res
   }
}


/**
 * a rule that legitimizes a [[StructuralFeature]]
 */
case class StructuralFeatureRule(feature: String, components : List[ComponentKey], mt : Option[MPath] = None, hasname : Boolean = true,usedom : Boolean = false, usecod : Boolean = false) extends Rule

/**
 * A StructureFeature defines the semantics of a [[DerivedDeclaration]]
 *
 * The semantics consists of a set of declarations that are injected into the parent theory after the [[DerivedDeclaration]]
 * These are called the 'outer declarations'
 */
abstract class StructuralFeature(val feature: String) extends FormatBasedExtension {
   def isApplicable(s: String) = s == feature

   /** called after checking components and inner declarations */
   def check(d: DerivedDeclaration)(implicit env: CheckingEnvironment): Unit

   /**
    * defines the outer perspective of a derived declaration
     *
     * @param parent the containing module
    * @param dd the derived declaration (pre: dd.feature == feature)
    */
   def elaborate(parent: DeclaredModule, dd: DerivedDeclaration): Elaboration

   def modules(d: DerivedDeclaration): List[Module]

}

/**
 * the return type of elaborating a [[DerivedDeclaration]] by a [[StructuralFeature]]
 */
abstract class Elaboration extends ElementContainer[Declaration] {
    def domain: List[LocalName]
    
    /**
     * default implementation in terms of the other methods
     * may be overridden for efficiency
     */
    def getDeclarations = {
      domain.map {n => getO(n).get}
    }

    def getMostSpecific(name: LocalName): Option[(Declaration,LocalName)] = {
      domain.reverse.foreach {n =>
         name.dropPrefix(n) foreach {suffix =>
           return getO(n) map {d => (d,suffix)}
         }
      }
      return None
    }
}

/**
 * Generative, definitional functors/pushouts with free instantiation
 * called structures in original MMT
 */

object GenerativePushoutRule extends StructuralFeatureRule("generative",List(DomComponent),usedom = true)

class GenerativePushout extends StructuralFeature("generative") {

  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) = {
      val dom = dd.getComponent(DomComponent) getOrElse {
        throw GetError("")
      } match {
        case tc: TermContainer => tc.get.getOrElse {
          throw GetError("")
        }
        case dc: MPathContainer => dc.get.getOrElse {
          throw GetError("")
        }
        case _ =>
          throw GetError("")
      }
      val context = parent.getInnerContext
      val body = controller.simplifier.materialize(context, dom, true, None) match {
        case m: DeclaredModule => m
        case m: DefinedModule => throw ImplementationError("materialization failed")
      }
      
      new Elaboration {
        /** the morphism dd.dom -> parent of the pushout diagram: it maps every n to dd.name/n */
        private val morphism = new DeclaredView(parent.parent, parent.name/dd.name, dom, parent.toTerm, false)
        /** precompute domain and build the morphism */
        val domain = body.getDeclarationsElaborated.map {d =>
          val ddN = dd.name / d.name
          val assig = Constant(morphism.toTerm, d.name, Nil, None, Some(OMS(parent.path ? ddN)), None)
          morphism.add(assig)
          ddN
        }
        // translate each declaration and merge the assignment (if any) into it
        private val translator = new ApplyMorphism(morphism.toTerm)
        def getO(name: LocalName): Option[Declaration] =
          if (name.steps.startsWith(dd.name.steps)) {
            val rest = name.drop(dd.name.steps.length)
            body.getO(rest) map {
              case d: Declaration =>
                val dT = d.translate(parent.toTerm, dd.name, translator)
                val dTM = dd.module.getO(rest) match {
                  case None => dT
                  case Some(a) => dT merge a
                }
                dTM
              case ne => throw LocalError("unexpected declaration in body of domain: " + ne.name)
            }
          } else None
      }
   }

   def modules(d: DerivedDeclaration): List[Module] = Nil

   def check(d: DerivedDeclaration)(implicit env: CheckingEnvironment) {}
}

// Binds theory parameters using Lambda/Pi in an include-like structure
class BoundTheoryParameters(id : String, pi : GlobalName, lambda : GlobalName, applys : GlobalName) extends StructuralFeature(id) {

  def elaborate(parent: DeclaredModule, dd: DerivedDeclaration) : Elaboration = {
    val dom = dd.getComponent(DomComponent) getOrElse {
      throw GetError("")
    } match {
      case tc: TermContainer => tc.get.getOrElse {
        throw GetError("")
      }
      case mc : MPathContainer => mc.get.getOrElse {
        throw GetError("")
      }
      case _ =>
        throw GetError("")
    }
    val context = parent.getInnerContext
    val body = controller.simplifier.getBody(context, dom) match {
      case t : DeclaredTheory => t
      case _ => throw GetError("Not a declared theory: " + dom)
    }
    controller.simplifier.apply(body)
    val vars = body.parameters
    val varsNames = vars.map(_.name)
    /*
    if (vars.isEmpty) return new Elaboration {
      val includes = PlainInclude(parent.path,body.path) :: body.getIncludes.map(PlainInclude(parent.path,_))
      def domain = includes.map(_.name)
      def getO(name: LocalName): Option[Declaration] = includes.find(_.name == name)
    }
    */

    def bindPi(t : Term) = if (vars.nonEmpty) OMBIND(OMS(pi),vars,t) else t
    def bindLambda(t : Term) = if (vars.nonEmpty) OMBIND(OMS(lambda),vars,t) else t
    val applyPars = new StatelessTraverser {
        def traverse(t: Term)(implicit con: Context, state: State) : Term = t match {
          case OMS(p) if p.module == dd.module.path =>
            vars.foldLeft(t)((tm,v) => OMA(OMS(applys),List(tm,OMV(v.name))))
          case OMBINDC(bind, bvars, scps) =>
            // rename all bound variables that are among the parameters to avoid capture
            val (bvarsR, bvarsSub) = Context.makeFresh(bvars, varsNames ::: con.domain)
            OMBINDC(traverse(bind), traverseContext(bvarsR), scps map {s => traverse(s ^? bvarsSub)(con++bvarsR, state)})
          case _ => Traverser(this,t)
        }
    }
    val translator = new Translator {
      def applyType(c: Context, t: Term) = bindPi(applyPars(t, c))
      def applyDef(c: Context, t: Term) = bindLambda(applyPars(t, c))
    }
    val prefix = dd.name
    new Elaboration {
      val decls = body.getDeclarationsElaborated.flatMap {
        case d =>
          List(d.translate(parent.toTerm, prefix, translator))
      /*  DM's old code
        case c: Constant =>
          //TODO @DM I don't think it's correct to treat generated constants differently here --FR 
          val tpE = if (c.isGenerated) c.tp else c.tp.map(t => bindPi(applyPars(t, Context.empty)))
          val dfE = if (c.isGenerated) c.df else c.df.map(t => bindLambda(applyPars(t, Context.empty)))
          val cE = Constant(parent.toTerm, prefix/c.name, c.alias.map(prefix/_),tpE, dfE, c.rl, c.notC)
          List(cE)
        case s : DeclaredStructure =>
          //TODO @DM This does not look right. We may have to drop the structure if vars.nonEmpty --FR 
          val ns = DeclaredStructure(parent.toTerm, LocalName(s.parent)/s.name, s.from, s.isImplicit)
          ElaboratedElement.set(ns)
          List(ns)
        case decl => throw LocalError("TODO: BoundTheoryParameters/Elaboration Declaration case " + decl.getClass)
      */
      }
      //body
      def domain: List[LocalName] = decls.map(_.name)
      def getO(name: LocalName): Option[Declaration] = decls.find(_.name == name)
    }

  }
  def modules(d: DerivedDeclaration): List[Module] = Nil
  def check(d: DerivedDeclaration)(implicit env: CheckingEnvironment) {}
}