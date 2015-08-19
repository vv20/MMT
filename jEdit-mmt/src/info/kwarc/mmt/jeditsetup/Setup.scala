package info.kwarc.mmt.jeditsetup

import info.kwarc.mmt.api.utils.File._
import info.kwarc.mmt.api.utils._

/**
 * install script for jEdit
 * 
 * copies jars, modes, abbreviations etc. to jEdit settings directory
 * installation is idempotent 
 */
object Setup {
  /**
   * This code works if run from setup script in the deploy/jedit-plugin folder
   * @param args the location of the jedit settings folder
   */
  def main(args: Array[String]) {
    val l = args.length
    val installOpt = if (l >= 1) args(0) match {
      case "install" => Some(true)
      case "uninstall" => Some(false)
      case _ => None
    } else None
    if (l > 3 || installOpt.isEmpty) {
      println("usage: setup (install | uninstall) /JEDIT/SETTINGS/FOLDER [/DEFAULT/CONTENT/FOLDER]")
      sys.exit
    }
    val jedit = File(args(1))
    val contentOpt = if (l == 3) Some(File(args(2))) else None
    val programLocation = File(getClass.getProtectionDomain.getCodeSource.getLocation.getPath).getParentFile
    val setup = programLocation.getParentFile
    val install = installOpt.get

    println("trying to (un)install from " + setup + " to " + jedit)
    if (!setup.isDirectory || !jedit.isDirectory) {
      println("error: not valid directories")
      sys.exit
    }
    doIt(setup, jedit, install, contentOpt)
  }
  
  /**
   * @param setup the deploy/jedit-plugin/plugin folder
   * @param jedit the jEdit settings folder
   * @param true/false for install/uninstall
   * @param the folder in which to look for archives
   */
  def doIt(setup:File, jedit: File, install: Boolean, contentOpt: Option[File]) {
    /** copies or deletes a file depending on install/uninstall */
    def copyOrDelete(f: List[String]) {
      if (install) {
        copy(setup / f, jedit / f)
      } else {
        delete(jedit / f)
      }
    }
    // copy/delete the jars
    (setup / "jars").list.foreach { e =>
      if (e.endsWith(".jar")) {
        copyOrDelete(List("jars", e))
      }
    }
    // modes
    // * copy/delete the mode files
    val modeFiles = (setup / "modes").list.filter(e => e.endsWith(".xml"))
    modeFiles.foreach { e => copyOrDelete(List("modes", e)) }
    // * read, update, write the catalog file
    val scat = setup / "modes" / "catalog"
    val jcat = jedit / "modes" / "catalog"
    var newCatalog: List[String] = Nil
    val modeEntries = modeFiles.map(e => "FILE=\"" + e + "\"")
    def isMMTEntry(line:String) = modeEntries.exists(e => line.contains(e))
    // read current catalog without MMT entries
    File.ReadLineWise(jcat) { line =>
      if (install) {
        if (line.contains("</MODES>")) {
          // append MMT modes if installing
          File.ReadLineWise(scat) { l => newCatalog ::= l }
        }
      }
      if (!isMMTEntry(line))
          newCatalog ::= line
    }
    // write new catalog
    if (!jcat.exists) {
      newCatalog = "</MODES>" :: newCatalog ::: List("<MODES>")
    }
    println("updating " + jcat)
    File.WriteLineWise(jcat, newCatalog.reverse)
    // abbrevs
    val sabb = setup / "abbrevs"
    val jabb = jedit / "abbrevs"
    var newAbbrevs: List[String] = Nil
    var remove = false
    // read current abbrevs without MMT abbrevs
    if (jabb.exists) File.ReadLineWise(jabb) { line =>
      if (install)
        newAbbrevs ::= line
      else {
        if (remove && line.startsWith("["))
          remove = false
        if (line.trim == "[mmt]")
          remove = true
        if (!remove)
          newAbbrevs ::= line
      }
    }
    // append MMT abbrevs if installing
    if (install) {
      File.ReadLineWise(sabb) { l => newAbbrevs ::= l }
    }
    // write new abbrevs
    println("updating " + jabb)
    File.WriteLineWise(jabb, newAbbrevs.reverse)
    // copy/delete pluginFolder
    val plug = List("plugins", "info.kwarc.mmt.jedit.MMTPlugin")
    copyOrDelete(plug ::: List("startup.msl"))
    val mars = plug ::: List("mars")
    val setupmars = setup / mars 
    if (setupmars.exists) {setupmars.list.foreach { e =>
      if (e.endsWith(".mar"))
        copyOrDelete(mars ::: List(e))
    }}
    if (!install) {
      val d = jedit / plug
      if (d.isDirectory) {
        d.deleteDir
        println("deleting directory " + d)
      }
    }
    
    if (install && contentOpt.isDefined) {
       println("adding property for content folder")
       val propsFile = jedit / "properties"
       val propsOld = File.read(propsFile)
       val encoded = contentOpt.get.toString.replace("\\","\\\\").replace(":","\\:").replace("=","\\=")
       val newValues = "info.kwarc.mmt.jedit.MMTPlugin.archives="+encoded
       val propsNew = propsOld+"\n"+newValues
       File.write(propsFile, propsNew)
    }
  }

  private def delete(f: File) {
    f.delete
    println("deleting " + f)
  }

  private def copy(from: File, to: File) {
    if (to.exists) {
      println("warning: " + to + " already exists, skipping; you probably want to run uninstall first")
    } else {
      to.getParentFile.mkdirs
      java.nio.file.Files.copy(from.toPath, to.toPath)
      println("copying " + from + " to " + to)
    }
  }
}
