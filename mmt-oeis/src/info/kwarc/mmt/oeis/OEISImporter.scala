package info.kwarc.mmt.oeis

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.parser._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.informal._
import info.kwarc.mmt.stex.OMDoc

import scala.xml.{Node,Elem,NamespaceBinding}

// ERRORS
abstract class OEISError(msg : String) extends Error(msg)
case class OEISParseError(msg : String, sref : Option[SourceRef], severity : Option[Level.Level]) extends OEISError(msg) {
  private def srefS = sref.map(_.region.toString).getOrElse("")
  override def toNode = <error type={this.getClass().toString} shortMsg={this.shortMsg} level={severity.toString} sref={srefS}> {this.extraMessage} </error>
}
case class OEISLookupError(msg : String) extends OEISError(msg)

object OEISParseError {
  def from(e : Exception, preMsg : Option[String] = None, sref : Option[SourceRef] = None, severity : Option[Level.Level] = None) : OEISParseError = {
    val pre = preMsg.map(_ + ": ").getOrElse("")
    val errMsg = pre + {
      e match {
        case er : Error => er.shortMsg
        case ex : Exception => ex.getMessage
      }
    }
    val err = new OEISParseError(errMsg, sref, severity)
    err.setStackTrace(e.getStackTrace)
    err
  }
}

// IMPORTER
class OEISImporter extends Importer {
  val key : String = "oeis-omdoc"
  override val logPrefix = "oeisimporter"
  def inExts = List("txt") //stex/latexml generated omdoc
  
  override def init(controller: Controller) {
    this.controller = controller
    report = controller.report
   }
  
  def parseSourceRef(n : scala.xml.Node,dpath : DPath)(implicit errorCont : ErrorHandler) : Option[SourceRef] = {
    OMDoc.parseSourceRef(n, dpath)
  }
  
  def importDocument(bt : BuildTask, cont : Document => Unit) {
    try {
      val src = scala.io.Source.fromFile(bt.inFile.toString)
      val node = parser.DocumentParser.fromReaderToXML(src)
      print(node)
      //val cp = scala.xml.parsing.ConstructingParser.fromSource(src, true)
      //val node : Node = cp.document()(0)
      src.close 
      translateDocument(node)(bt.narrationDPath, bt.errorCont)
      val doc = controller.getDocument(bt.narrationDPath)
      cont(doc)
    } catch {
      case e : Throwable => 
        log("WARNING: Skipping article due to error: " + e.toString() + " \n" + e.getStackTrace.mkString("\n")) //skipping declaration
    }
  }
  
  /**
   * Translate a toplevel <omdoc> node
   */
  private def translateDocument(n : Node)(implicit dpath : DPath, errorCont : ErrorHandler) : Unit = {
    n.label match {
      case "omdoc" => 
        //creating document
        implicit val doc = new Document(dpath)
        controller.add(doc)
        //recursing into children
        n.child.foreach(translateModule)
    }
  }
  /**
   * translate second-level, in-document elements (typically modules)
   */
  private def translateModule(n : Node)(implicit doc : Document, errorCont : ErrorHandler) : Unit = {
    val sref = OMDoc.parseSourceRef(n, doc.path)
    try {
      n.label match {
        case "theory" => //create theory
          val name = LocalName((n \ "@name").text)
          val thy = new DeclaredTheory(doc.path, name, None)
          val ref = MRef(doc.path, thy.path, true)
          controller.add(ref)
          controller.add(thy)
          n.child.foreach(translateDeclaration(_)(doc, thy, errorCont))
        case "#PCDATA" | "#REM" => //Atom or Comment => do nothing
      }
    } catch {
      case e : Error => errorCont(e)
      case e : Exception => OEISParseError.from(e, Some("Skipping module-level element " + n.label + " due to error"), sref, None)
    }
  }
  
   /** 
   *  translate third level, in-module elements (typically declarations)
   */
  private def translateDeclaration(n : Node)(implicit doc : Document, thy : DeclaredTheory, errorCont : ErrorHandler) : Unit = {
    val sref = OMDoc.parseSourceRef(n, doc.path)
    implicit val dpath = doc.path
    implicit val mpath = thy.path
    try {
      n.label match {
        case "assertion" => 
          val nr = thy.getDeclarations.length + 1
          val name = LocalName(n.label + nr)
          //val tpWrapperO = n.child.find(_.label == "type")
          val tpO = None //tpWrapperO.map(tpN => translateTerm(tpN.child.head))
          val dfO = None //TODO, get also def
          val const = new FinalConstant(OMMOD(mpath), name, None, TermContainer(tpO), TermContainer(dfO), None, NotationContainer())
          controller.add(const)
        case "#PCDATA" | "#REM" => //Atom or Comment => do nothing
        case  "omtext" => 
          val nr = thy.getDeclarations.length + 1
          val name = LocalName(n.label + nr)
          parseNarrativeObject(n) match {
            case Some(t) => 
              val dfn = PlainNarration(OMMOD(mpath), name, t)
              controller.add(dfn)
            case _ => log("WARNING: Ignoring declaration due to no object " + n.toString)
          }
        case "p" if (n \ "@class").text == "formula" => //CMP inside
          val sname = LocalName("t" + thy.getDeclarations.length.toString)
          implicit val spath = mpath ? sname
          parseNarrativeObject(n) match {
            case Some(fo) => 
              val a = Assertion(OMMOD(mpath), sname, fo)
              controller.add(a)
            case _ => errorCont(OEISParseError("Ignoring declaration due to no object " + n.toString, sref, Some(Level.Warning)))
          }
        case _ => errorCont(OEISParseError("Ignoring declaration: " + n.label, sref, Some(Level.Warning)))
      }
    } catch {
      case e : Error => errorCont(e)
      case e : Exception => OEISParseError.from(e, Some("Skipping declaration-level element " + n.label + " due to error"), sref, None)
    }
  }
  
  def parseNarrativeObject(n : scala.xml.Node)(implicit dpath : DPath, mpath : MPath, errorCont : ErrorHandler) : Option[Term]= {
    OMDoc.parseNarrativeObject(n)(dpath, mpath, errorCont, resolveSPath)
  }
  
  def resolveSPath(tnameSO : Option[String], snameS : String, container : MPath)(implicit errorCont : ErrorHandler) : GlobalName = {
    val tNameDef = tnameSO.map(LocalName(_)).getOrElse(container.name)
    val sNameDef = LocalName(snameS)
    val docDef = container.doc
    val defaultPath = docDef ? tNameDef ? sNameDef
    val tpaths = controller.globalLookup.visible(OMMOD(container)) //includes container
    //filter those that match tname
    val options = tpaths.map(_.toMPath).filter(t => tnameSO.map(t.name.last.toPath == _).getOrElse(true))
    options.toList match {
      case Nil => 
        errorCont(OEISParseError("Cannot resolve module for " + tnameSO.getOrElse("*") + "?" + snameS + 
            " from theory " + container.toPath, None, Some(Level.Warning)))
        defaultPath
      case l => 
        val matches = l.map(controller.get(_)) flatMap {
          case d : DeclaredTheory => 
            d.getConstants.filter(_.name.last.toPath == snameS)
          case _ => None
        }
        matches match {
          case Nil => 
            errorCont(OEISParseError("Cannot resolve symbol for module=" + tnameSO.getOrElse("*") + " and symbol=" + snameS + 
                ". No matching modules " + l +  " , contain symbol " + snameS, None, Some(Level.Warning) ))
            defaultPath
          case hd :: Nil => hd.path
          case _ => 
            errorCont(OEISParseError("Cannot resolve symbol for module=" + tnameSO.getOrElse("*") + " and symbol=" + snameS + 
                ". Several matching symbols: " + matches.map(_.path), None, Some(Level.Warning)))
            defaultPath
        }
    }
  }
}

