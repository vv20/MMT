package info.kwarc.mmt.api.libraries

import info.kwarc.mmt.api._
import frontend._
import documents._
import modules._
import objects._
import symbols._
import utils.MyList._

import scala.collection._
import scala.ref.SoftReference
import scala.xml.NodeSeq

/** auxiliary class of [[Library]] to optimize storage
  *
  * This uses [[scala.ref.SoftReference]]s so that modules are automatically removed when memory is needed.
  */
class ModuleHashMap {
  private val underlying = new mutable.HashMap[MPath, SoftReference[Module]]

  def get(p: MPath): Option[Module] = {
    underlying.get(p).flatMap(_.get)
  }

  def update(p: MPath, m: Module) {
    val r = new SoftReference(m)
    underlying.update(p, r)
  }

  def -=(p: MPath) {
    underlying -= p
  }

  def keys = underlying.keys

  def values = underlying.values.flatMap(_.get)

  def clear {
    underlying.clear
  }

  override def toString = {
    underlying.toList.sortBy(_._1.name.toPath).map { case (mp, r) =>
      val modString = r.get match {
        case Some(mod) => mod.toString
        case None => "[cleared]"
      }
      mp.toString + "\n" + modString
    }.mkString("\n")
  }
}

/** A Library represents an MMT theory graph.
  *
  * The Library implements the central structural algorithms, in particular lookup.
  * All access of the main data structures is through the library's get/add/update/delete methods.
  *
  * Invariance: This class guarantees structural well-formedness in the sense that
  * libraries conform to the MMT grammar and all declarations have canonical URIs.
  * The well-formedness of the objects in the declarations is not guaranteed.
  *
  * @param report parameter for logging.
  */
class Library(val report: frontend.Report) extends Lookup with Logger {
  val logPrefix = "library"

  // ************************ stateful data structures and basic accessors
  
  /** all known root documents except for those induced by modules */
  private val documents = new scala.collection.mutable.HashMap[DPath,Document]
  /** all known root modules (which also induce root documents) */
  private val modules = new ModuleHashMap
  /** the diagram of implicit morphisms */
  private val implicitGraph = new ThinGeneratedCategory

  override def toString = modules.values.map(_.toString).mkString("", "\n\n", "")

    /** returns all module paths indexed by this library */
  def getAllPaths = modules.keys

  /** retrieves all modules in any order */
  def getModules = modules.values

  /** direct lookup of p for mp = p / ln, also returns ln */
  private def modulesGetRoot(mp: MPath): (Module, LocalName) = {
    val top = mp.doc
    val name = mp.name
    val rootMod = name.prefixes.mapFind {n =>
       modules.get(top ? n)
    }.getOrElse {throw NotFound(mp)}
    val ln = name.drop(rootMod.name.length)
    (rootMod, ln)
  }
  
  /** direct lookup of p for dp = p / ln, also returns ln
   *  this considers both root documents  p / ln and root modules p / ln.init ? ln.head
   *  the longest known p is chosen in case of ambiguity 
   */
  private def documentsGetRoot(dp: DPath, error: String => Nothing): (Document, LocalName) = {
    val top = dp.^^
    val name = dp.name
    val rootDoc = name.prefixes.reverse.mapFind {n =>
       val topn = top / n
       documents.get(topn) orElse {
          if (n.nonEmpty) modules.get(topn.toMPath).map(m => seeAsDoc(m, error))
          else None // n may be empty if name is empty to begin with
       }
    }.getOrElse {throw NotFound(dp)}
    val ln = name.drop(rootDoc.path.name.length)
    (rootDoc,ln)
  }

  /** top level retrieval method */
  def get(p: Path): StructuralElement = {
     val error = (msg:String) => throw GetError("error while retrieving " + p + ": " + msg)
     p match {
        case d: DPath => getNarrative(d, error)
        case p: MPath => getContent(p, error)
        case p ?? n => get(OMMOD(p), n, error)
        case c: CPath => throw GetError("retrieval of components not possible")
     }
  }

  // ******************* document level retrieval
  /** 
   *  dereferences a narrative URI d
   *  Retrieval starts in the root document doc such that d = doc.path / left.
   *  Then it dereferences left step-wise (from left to right), considering any nesting (e.g., documents or modules).
   *  seeAsDoc is used to turn intermediate retrieval results into [[Document]]s.
   *
   *  Note the similarity to getContent.
   */
  private def getNarrative(d: DPath, error: String => Nothing): NarrativeElement = {
     /** step-wise looks up left in ne */
     def getNarrativeAux(ne: NarrativeElement, left: LocalName): NarrativeElement = {
        if (left.isEmpty) ne
        else {
            val doc = seeAsDoc(ne, error)
            val child = doc.getLocally(LocalName(left.head)).getOrElse {
               throw NotFound(doc.path / left.head)
               // no error here because the document may exist separately
               //error("no child " + left.head + " found in document " + doc.path)
            }
            getNarrativeAux(child, left.tail)
         }
     }
     // try root document
     val (doc, left) = documentsGetRoot(d, error)
     getNarrativeAux(doc, left)
  }
  
  /** tries to interpret a narrative element as a document
   *  in particular, this treats modules as documents, and dereferences NRefs
   */
  private def seeAsDoc(se: StructuralElement, error: String => Nothing): Document = se match {
     case d: Document => d
     case r: NRef => getO(r.target) match {
        case Some(se) => seeAsDoc(se, error)
        case None => error("referenced element does not exist: " + r.target)
     }
     case b: Body => b.asDocument
     case nm: NestedModule => seeAsDoc(nm.module, error)
     case _ => error("element exists but is not document-like: " + se.path)
  }

  // ******************* module level retrieval

  /** 
   *  dereferences a content URI p
   *  Retrieval starts in the root module mod such that p = mod.path / left.
   *  Then it dereferences left step-wise (from left to right), considering only the nesting of modules.
   *  seeAsMod is used to turn intermediate retrieval results into modules.
   *  
   *  Note the similarity to getNarrative.
   */
  private def getContent(p: MPath, error: String => Nothing): ContentElement = {
     /** step-wise looks up left in ce */
     def getContentAux(ce: ContentElement, left: LocalName): ContentElement = {
        if (left.isEmpty) ce
        else {
           val m = seeAsMod(ce, error)
           val d = getInAtomicModule(m, Nil, LocalName(left.head), error)
           getContentAux(d, left.tail)
         }
     }
     val (mod, left) = modulesGetRoot(p)
     val ce = getContentAux(mod, left)
     // at this level, NestedModules are artifacts, so we don't unwrap the module
     ce match {
        case nm: NestedModule => nm.module
        case ce => ce
     }
  }
  /** tries to interpret a content element as a module
   *  In particular, [[NestedModule]]s are turned into [[Module]]s, which allows referencing into nested theory-like elements.
   */
  // TODO Once structures are nested modules, this can return Module instead of ContentElement
  private def seeAsMod(ce: ContentElement, error: String => Nothing) = ce match {
     case m: Module => m
     case nm: NestedModule => nm.module
     case s: Structure => s
     case _ => error("element exists but is not module-like: " + ce.path)
  }
   
  // ******************* declaration level retrieval
  private val sourceError = (s: String) => throw GetError("error while looking up source declaration: "+s)

  /** dereferences a declaration level reference
   * 
   * Retrieval starts in the given module 'home'.
   * Then it dereferences 'name' step-wise, considering only the MMT module system, in particular elaboration.
   * 
   * This method goes beyond URI dereferencing because the module may be a complex expression.
   * For plain URI dereferencing, the 'home' is simply an OMMOD.
   * 
   * Defined modules are expanded, which allows referencing into their materialized body.
   */

  def get(home: Term, name: LocalName, error: String => Nothing): Declaration = home match {
    // lookup in atomic modules
    case OMPMOD(p, args) =>
       val mod = seeAsMod(getContent(p, error), error)
       getInAtomicModule(mod, args, name, error)
    // lookup in structures
    case OMDL(h, n) =>
      val s = getStructure(h ? n, msg => throw GetError("declaration exists but is not a structure: " + h ? n + "\n" + msg))
      try {
        getInLink(s, name, error)
      } catch {
        case PartialLink() =>
          // return default assignment
          val da = get(s.from, name, sourceError) match {
            case c: Constant => ConstantAssignment(s.toTerm, name, None, Some(OMID(h ? s.name / name)))
            case d: Structure => DefLinkAssignment(s.toTerm, name, d.from, OMDL(h, s.name / name))
            case _ => throw ImplementationError("unimplemented default assignment")
          }
          da.setOrigin(DefaultAssignment)
          da
      }
    // lookup in complex modules
    case ComplexTheory(cont) =>
      cont.mapVarDecls { case (before, vd) =>
        val vdDecl = vd.toDeclaration(ComplexTheory(before))
        vd match {
          case IncludeVarDecl(p, args) =>
            name.head match {
              case ComplexStep(q) =>
                getImplicit(q, p).foreach { m =>
                  val sym = get(OMPMOD(q, args), name.tail, sourceError)
                  val symT = translate(sym, m, error)
                  return symT
                }
              case _ =>
            }
          case StructureVarDecl(s, OMPMOD(p, args), dfOpt) =>
            name.head match {
              case s2@SimpleStep(_) if s == LocalName(s2) =>
                val sym = getSymbol(p ? name.tail)
                val struc = vdDecl.asInstanceOf[Structure]
                val symT = translateByLink(sym, struc, error)
                return symT
              case _ =>
            }
          case VarDecl(n, _, _, _) =>
            name.head match {
              case n2@SimpleStep(_) if n == LocalName(n2) =>
                val c = vdDecl.asInstanceOf[Constant]
                return c
              case _ =>
            }
        }
      }
      throw GetError("name " + name + " not found in " + cont)
    case TUnion(ts) => ts mapFind { t =>
      getO(t,name)
    } getOrElse {
      error("union of theories has no declarations except includes")
    }
    case OMCOMP(Nil) => throw GetError("cannot lookup in identity morphism without domain: " + home)
    case OMCOMP(hd :: tl) =>
      val a = get(hd, name, error)
      if (tl.isEmpty)
        a
      else a match {
        case a: Constant => ConstantAssignment(home, a.name, a.alias, a.df.map(_ * OMCOMP(tl)))
        case a: DefinedStructure => DefLinkAssignment(home, a.name, a.from, OMCOMP(a.df :: tl))
      }
    case OMIDENT(t) => get(t, name, error) match {
      case c: Constant => ConstantAssignment(home, name, None, Some(c.toTerm))
      case l: Structure => DefLinkAssignment(home, name, l.from, l.toTerm)
    }
    case Morph.empty => error("empty morphism has no assignments")
    case MUnion(ms) => ms mapFind {
      m => getO(m, name)
    } getOrElse {
      error("union of morphisms has no assignments except includes")
    }
    case _ => error("unknown module: " + home)
  }

  /** auxiliary method of get for lookups in a parent that has already been retrieved */
  private def getInAtomicModule(mod: ContentElement, args: List[Term], name: LocalName, error: String => Nothing): Declaration = {
    mod match {
      //lookup in atomic modules
      case t: DefinedTheory =>
        val d = get(t.df, name, error)
        instantiate(d, t.parameters, args)
      case t: DeclaredTheory =>
         getInTheory(t, args, name, error)
      case s: Structure =>
        if (0 != args.length)
          throw GetError("number of arguments does not match number of parameters")
        get(s.toTerm, name, error)
      case v: View =>
        if (0 != args.length)
          throw GetError("number of arguments does not match number of parameters")
        try {
          getInLink(v, name, error)
        } catch {
          case PartialLink() =>
            val da = get(v.from, name, sourceError) match {
              // return default assignment
              case c: Constant => ConstantAssignment(v.toTerm, name, None, None)
              case s: Structure => DefLinkAssignment(v.toTerm, name, s.from, Morph.empty)
              case _ => throw ImplementationError("unimplemented default assignment")
            }
            da.setOrigin(DefaultAssignment)
            da
        }
    }
  }
  
  /** auxiliary method of get for lookups in a [[DeclaredTheory]] */
  private def getInTheory(t: DeclaredTheory, args: List[Term], name: LocalName, error: String => Nothing) = { 
     val decl = t.getMostSpecific(name) map {
        case (d, ln) => (instantiate(d, t.parameters, args), ln)
     }
     decl match {
       case Some((d, LocalName(Nil))) => d // perfect match
       case Some((d, ln)) => d match {
         // a prefix exists and resolves to d, a suffix ln is left
         case _: Constant | _: RuleConstant => error("local name " + ln + " left after resolving to constant")
         case s: Structure =>
           val sym = get(s.from, ln, sourceError) // resolve ln in the domain of d
           translateByLink(sym, s, error) // translate sym along l
         case nm: NestedModule =>
           error("local name " + ln + " left after resolving to nested module")
       }
       case None => name match {
         // no prefix of name is declared in t
         case ComplexStep(mpath) / ln =>
           get(mpath) match {
             case _: Theory =>
               // continue lookup in mpath
               val imp = implicitGraph(OMMOD(mpath), t.toTerm) getOrElse {
                 throw GetError("no implicit morphism from " + mpath + " to " + t.path)
               }
               val sym = get(OMMOD(mpath), ln, sourceError)
               translate(sym, imp, error) // translate the result along the implicit morphism
             case l: Link =>
               // continue lookup in domain of l
               val sym = get(l.from, ln, sourceError)
               translateByLink(sym, l, error) // translate the result along the link
           }
         case _ => throw GetError(name + " is not declared in " + t.path)
          }
     }
  }

  
  /** thrown by getInLink if the link provides no assignment */
  private case class PartialLink() extends java.lang.Throwable
  
  /** auxiliary method of get to unify lookup in structures and views
    * throws PartialLink() if no assignment provided
    */
  private def getInLink(l: Link, name: LocalName, error: String => Nothing): Declaration = l match {
    case l: DefinedLink =>
      get(l.df, name, error)
    case l: DeclaredLink =>
      l.getMostSpecific(name.simplify) match {
        case Some((a, LocalName(Nil))) => a // perfect match TODO Here should probably happen something
        case Some((a, ln)) => a match {
          case a: Constant => error("local name " + ln + " left after resolving to constant assignment")
          case a: DefinedLink => get(a.df, name.simplify, error) // <- names in links should always start with [T]/...?

        }
        case None =>
          // TODO multiple ComplexSteps resulting from inclusions in a row must be normalized away somewhere
          throw PartialLink()
      }
  }

  /** instantiates parameters of a declaration with arguments
    *
    * the home of the new declarations is adapted
    */
  private def instantiate(decl: Declaration, params: Context, args: List[Term]): Declaration = {
    if (args.isEmpty) return decl // lookup from within parametric theory does not provide arguments
    val subs: Substitution = (params / args).getOrElse {
        throw GetError("number of arguments does not match number of parameters")
      }
    if (subs.isIdentity) return decl // avoid creating new instance
    val newHome = decl.home match {
        case OMPMOD(p, oldArgs) => OMPMOD(p, oldArgs ::: args)
        case _ => throw ImplementationError("cannot instantiate declaration of complex theory")
      }
    decl match {
      case c: Constant =>
        Constant(newHome, c.name, c.alias, c.tp.map(_ ^? subs), c.df.map(_ ^? subs), c.rl, c.notC)
      case s: DefinedStructure =>
        DefinedStructure(newHome, s.name, s.from ^? subs, s.df ^? subs, s.isImplicit)
      case s: DeclaredStructure =>
        val sS = DeclaredStructure(newHome, s.name, s.from ^? subs, s.isImplicit)
        s.getPrimitiveDeclarations.foreach { case d =>
          sS.add(instantiate(d, params, args))
        }
        sS
      case nm: NestedModule => throw ImplementationError("substitution of nested modules not implemented yet")
    }
  }

  /** translate a Declaration along a morphism */
  private def translate(d: Declaration, morph: Term, error: String => Nothing): Declaration = {
    morph match {
      case OMMOD(v) =>
        val link = getView(v)
        translateByLink(d, link, error)
      case OMDL(h, n) =>
        val link = getStructure(h ? n)
        translateByLink(d, link, error)
      case OMCOMP(Nil) => d
      case OMCOMP(hd :: tl) => translate(translate(d, hd, error), OMCOMP(tl), error)
      case OMIDENT(t) =>
        val imp = implicitGraph(d.home, t) getOrElse {
          throw GetError("no implicit morphism from " + d.home + " to " + t)
        }
        //TODO better copy d and change home to t and name to d.home/d.name
        translate(d, imp, error)
      //TODO remaining cases
    }
  }

  /** auxiliary method of translate to unify translation along structures and views */
  private def translateByLink(decl: Declaration, l: Link, error: String => Nothing): Declaration =
    l match {
      case l: DefinedLink => translate(decl, l.df, error)
      case l: DeclaredLink =>
        //compute e such that the home theory of decl is the domain of l by inserting an implicit morphism
        val imp = implicitGraph(decl.home, l.from) getOrElse {
          throw GetError("no implicit morphism from " + decl.home + " to " + l.from)
        }
        val declT = translate(decl, imp, error)
        // get the assignment for e provided by l (if any)
        val assigOpt = try {
          //Some(getInLink(l,declT.name, error))
          Some(getInLink(l, ComplexStep(declT.parent) / declT.name, error))
        } catch {
          case PartialLink() =>
            None
          //try {Some(getInLink(l,ComplexStep(declT.parent) / declT.name,error))} catch {
          //  case PartialLink() => None
          //}
        }
        // the prefix of the new constant
        val namePrefix = l match {
          case s: Structure => s.name
          case v: View => LocalName(ComplexStep(v.path))
        }
        // see assigOpt definition
        val newName = namePrefix / (assigOpt match {
          case Some(a: Constant) => a.name
          case _ => declT.name
        })
        // TODO Simplify here or somewhere else? (alternatively gettype-thingy of solver?)
        def mapTerm(t: Term) = ApplyMorphs.traverse(t)(Context.apply(l.from.toMPath), l.toTerm) //t * l.toTerm
      // translate declT along assigOpt
      val newDecl = declT match {
          case c: Constant =>
            val (alias, target, not) = assigOpt match {
              //use alias and target (as definiens) from assignment
              case Some(a: Constant) => (a.alias, a.df, a.not)
              //translate old alias and definiens if no assignment given
              case None => (None, None, None)
              case _ => throw GetError("link " + l.path + " provides non-ConstantAssignment for constant " + c.path)
            }
            val newAlias = alias orElse c.alias.map(namePrefix / _)
            val newDef = target orElse c.df.map(mapTerm)
            val newNotC = not.map(notations.NotationContainer(_)) getOrElse c.notC
            Constant(l.to, newName, newAlias, c.tp.map(mapTerm), newDef, c.rl, newNotC)
          case r: Structure => assigOpt match {
            case Some(a: DefinedStructure) =>
              DefinedStructure(l.to, newName, r.from, a.df, isImplicit = false)
            case None =>
              DefinedStructure(l.to, newName, r.from, OMCOMP(r.toTerm, l.toTerm), isImplicit = false) //TODO should be a DeclaredStructure
            case _ => throw GetError("link " + l.path + " provides non-StructureAssignment for structure " + r.path)
          }
        }
        newDecl
    }

  // ******************* additional retrieval methods
  
  def getDeclarationsInScope(mod: Term): List[Content] = {
    val impls = visibleVia(mod).toList
    val decls = impls flatMap { case (from, via) =>
      get(from.toMPath).getDeclarations
    }
    decls
  }

  /** retrieve the implicit morphism "from --implicit--> to" */
  def getImplicit(from: Term, to: Term): Option[Term] = implicitGraph(from, to)

  /** set the implicit morphism "from --implicit--> to" */
  // TODO should be private, public only because of Archive.readRelational
  def addImplicit(from: Term, to: Term, morph: Term) {
    implicitGraph(from, to) = morph
  }

  /** retrieve all implicit morphisms into a theory
    *
    * @param to the theory
    * @return all pairs (theory,via) such that "via: theory --implicit--> to" (transitive and reflexive)
    */
  private def visibleVia(to: Term): mutable.HashSet[(Term, Term)] = {
    val hs = implicitGraph.into(to)
    hs add(to, OMCOMP())
    hs
  }

  /** as visibleVia but dropping the implicit morphisms */
  def visible(to: Term): mutable.HashSet[Term] = visibleVia(to).map(_._1)

  /** as visible but only those where the morphism is trivial (i.e., all includes) */
  def visibleDirect(to: Term): mutable.HashSet[Term] = visibleVia(to).flatMap { case (from, via) =>
    if (via == OMCOMP()) List(from) else Nil
  }

  /** returns the symbol from which the argument symbol arose by a structure declaration */
  def preImage(p: GlobalName): Option[GlobalName] = p.name match {
    case hd / tl =>
      try {
        get(p.module ? hd) match {
          case s: Structure => Some(s.from.toMPath ? tl) //TODO
        }
      } catch {
        case _: Throwable => None
      }
    case !(hd) => None
    case _ => None
  }

  // ******************* state-changing interface: add, delete, update
  
  /** add, delete, and update must first locate the containing element
   *  Therefore, they share a lot of code, which is captured in this class.
   */
  private abstract class ChangeProcessor {
     def errorFun(msg: String): Nothing
     def primitiveDocument(dp: DPath): Unit
     def otherNarrativeElement(parent: Document, ln: LocalName): Unit
     def primitiveModule(mp: MPath): Unit
     def otherContentElement(parent: Body, ln: LocalName): Unit
     def component(cp: CPath, cont: ComponentContainer): Unit
     /** This does the relevant case distinction and then delegates to one of the abstract methods. */
     def apply(p: Path) {p match {
        case dp: DPath =>
           if (documents.isDefinedAt(dp))
             primitiveDocument(dp)
           else {
             val doc = seeAsDoc(getNarrative(dp ^, errorFun), errorFun)
             otherNarrativeElement(doc, LocalName(dp.name.last))
           }
        case mp: MPath =>
           primitiveModule(mp)
        case GlobalName(p,ln) =>
            val se = seeAsMod(getContent(p, errorFun), errorFun)
            se match {
              case b: Body =>
                 otherContentElement(b, ln)
              case _ => errorFun("parent does not resolve to container " + se.path)
            }
        case cp: CPath =>
           getO(cp.parent) foreach {se =>
              se.getComponent(cp.component).foreach {cont =>
                component(cp, cont)
              }
           }
     }}
  }
    
  // ******************* adding elements
  
  /**
   * adds a declaration
   * @param e the added declaration
   */
  def add(e: StructuralElement) {
    log("adding " + e.path + " (which is a " + e.getClass.getName + ")")
    val adder = new Adder(e)
    e match {
       case doc: Document if doc.root =>
          // special treatment for root documents, this case can't be detected by ChangeProcessor
          adder.primitiveDocument(doc.path)
       case ne: NarrativeElement if ne.name.length != 1 =>
          // this case is needed because ChangeProcessor assumes ne.name.length == 1 && ne.parentOpt = Some(ne.path ^)
          // can it be specified away?
          val parDoc = seeAsDoc(getNarrative(ne.parentOpt.get, adder.errorFun), adder.errorFun)
          adder.otherNarrativeElement(parDoc, ne.name)
       case _ =>
          adder.run
    }
    try {
      e match {
        case l: Link if l.isImplicit =>
          implicitGraph(l.from, l.to) = l.toTerm
        case t: DeclaredTheory =>
          t.getIncludes foreach {m =>
            implicitGraph(OMMOD(m), t.toTerm) = OMIDENT(OMMOD(m))
          }
        case t: DefinedTheory =>
          implicitGraph(t.df, t.toTerm) = OMIDENT(t.toTerm)
        case e: NestedModule =>
          add(e.module)
          implicitGraph(e.home, e.module.toTerm) = OMIDENT(e.home)
        case _ =>
        //TODO add equational axioms
      }
    } catch {
      case AlreadyDefined(from, to, old, nw) =>
        throw AddError(s"implicit morphism $nw from $from to $to induced by ${e.path} in conflict with existing implicit morphism $old")
    }
  }
  
  /** the bureaucracy of adding 'se' */
  private class Adder(se: StructuralElement) extends ChangeProcessor {
     def errorFun(msg: String) = throw AddError(msg)
     def wrongType(exp: String) {errorFun("expected a " + exp)}
     def run {
        apply(se.path)
     }
     def primitiveDocument(dp: DPath) = {
        se match {
           case doc: Document =>
              documents(dp) = doc
           case _ =>
              wrongType("document")
        }
     }
     def otherNarrativeElement(doc: Document, ln: LocalName) = {
        se match {
           case ne: NarrativeElement =>
              doc.add(ne)
           case _ =>
              wrongType("narrative element")
        }
     }
     def primitiveModule(mp: MPath) = {
        se match {
           case mod: Module =>
              modules(mp) = mod
           case _ =>
              wrongType("module")
        }
     }
     def otherContentElement(body: Body, ln: LocalName) = {
        se match {
           case d: Declaration =>
              body.add(d)
           case _ =>
              wrongType("declaration")
        }
     }
     def component(cp: CPath, cont: ComponentContainer) = {
        wrongType("content element") // should be impoosible
     }
  }


 // ******************* deleting elements

  /** deletes the element with the given URI
    *
    * @param path the path to the element to be deleted
    */
  def delete(path: Path) {
    log("deleting " + path)
    Deleter.apply(path)
  }
  
  private object Deleter extends ChangeProcessor {
     def errorFun(msg: String) = throw DeleteError(msg)
     def primitiveDocument(dp: DPath) = {
        documents -= dp
     }
     def otherNarrativeElement(doc: Document, ln: LocalName) = {
        doc.delete(ln)
     }
     def primitiveModule(mp: MPath) = {
        modules -= mp
        //if (mp.name.length > 1) delete(mp.toGlobalName)
     }
     def otherContentElement(body: Body, ln: LocalName) = {
       body.delete(ln) foreach {s =>
         s.getComponents.foreach {case DeclarationComponent(comp, cont) =>
           if (cont.isDefined) notifyUpdated(s.path $ comp)
         }
       }
     }
     def component(cp: CPath, cont: ComponentContainer) = {
        cont.delete
        notifyUpdated(cp)
     }
  }
  
 // ******************* updating elements  
  
  /** updates a StructuralElement */
  def update(e: StructuralElement) {
    log("updating " + e.path)
    new Updater(e).run
  }
  
  /** almost the same as Adder; the only overrides are replacing "add" and "Add" with "update" and "Update" */
  private class Updater(se: StructuralElement) extends Adder(se) {
     override def errorFun(msg: String) = throw UpdateError(msg)
     override def otherNarrativeElement(doc: Document, ln: LocalName) = {
        se match {
           case ne: NarrativeElement =>
              doc.update(ne)
           case _ =>
              wrongType("narrative element")
        }
     }
     override def otherContentElement(body: Body, ln: LocalName) = {
        se match {
           case d: Declaration =>
              body.update(d)
           case _ =>
              wrongType("declaration")
        }
     }
  }
  
  /** moves an element with a given path to the end of its parent document */
  def reorder(p: Path) {
     Reorderer.apply(p)
  }
  private object Reorderer extends ChangeProcessor {
     def errorFun(msg: String) = throw UpdateError(msg)
     def primitiveDocument(dp: DPath) {}
     def otherNarrativeElement(doc: Document, ln: LocalName) {
        doc.reorder(ln)
     }
     def primitiveModule(mp: MPath) {}
     def otherContentElement(body: Body, ln: LocalName) {
        body.reorder(ln)
     }
     def component(cp: CPath, cont: ComponentContainer) {}
  }
  
  // change management
  
  /** marks all known dependent components as dirty
    *
    * This method is public because components may be changed from the outside.
    * In that case, this method may be called additionally to maintain invariants.
    *
    * When deleting a Symbol or Component, this is called automatically.
    *
    * @param p path to the component that was changed/deleted
    */
  def notifyUpdated(p: CPath) {
    log("updated: " + p)
    logGroup {
      // notify the definiens (if any) if a type changed
      if (p.component == TypeComponent) {
         getO(p.parent) foreach {
            se => se.getComponent(DefComponent) foreach {
               case df: TermContainer =>
                  log("definiens needs recheck")
                  df.setAnalyzedDirty
               case _ =>
            }
         }
      }
      // notify all dependencies
      modules.values.foreach { case m: Module =>
        m.foreachComponent {
          case (comp, tc: TermContainer) =>
            if (tc.dependsOn contains p) {
              log("dependency needs recheck: " + comp)
              tc.setAnalyzedDirty
            }
          case _ =>
        }
      }
    }
  }

  // delete everything
  
  /** forgets everything */
  def clear {
    modules.clear
    implicitGraph.clear
    documents.clear
  }
}
