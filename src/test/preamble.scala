import info.kwarc.mmt.api
import info.kwarc.mmt.api.frontend.Run
import info.kwarc.mmt.api.ontology.{DeclarationTreeExporter, DependencyGraphExporter, PathGraphExporter}
import info.kwarc.mmt.api.web.JSONBasedGraphServer

/** An abstract class for test methods. Instantiates a controller, sets the mathpath for archives,
  * loads the AlignmentsServer (so you can run a Server without getting an error message.
  *
  * You just need to give archivepath and instantiate the run method with your own arbitrary code
  *
  * @param archivepath    : the path to your archives
  * @param logprefixes    : A list of logprefixes to log
  * @param alignmentspath : the path to .align files (doesn't need to be set, therefore defaults to
  *                         empty string)
  * @param serverport     : Optional port to start a server. If None, no server is started
  * @param gotoshell      : if true, it will drop to the MMT shell afterwards
  * @param logfile        : If defined, will log into file
  */
abstract class Test(archivepath : String,
                    logprefixes : List[String] = Nil,
                    alignmentspath : String = "",
                    serverport : Option[Int] = None,
                    gotoshell : Boolean = true,
                    logfile : Option[String] = None) {
  val controller = Run.controller

  // If you want to log additional stuff, just put it in this list

  controller.handleLine("log console")
  if (logfile.isDefined) controller.handleLine("log html " + logfile.get)// /home/raupi/lmh/mmtlog.txt")
  logprefixes foreach (s => controller.handleLine("log+ " + s))
  controller.handleLine("extension info.kwarc.mmt.lf.Plugin")
  controller.handleLine("extension info.kwarc.mmt.odk.Plugin")
  controller.handleLine("extension info.kwarc.mmt.pvs.Plugin")
  // controller.handleLine("extension info.kwarc.mmt.metamath.Plugin")
  controller.handleLine("mathpath archive " + archivepath)
  controller.handleLine("extension info.kwarc.mmt.api.ontology.AlignmentsServer " + alignmentspath)


  def doFirst : Unit = {}

  def run : Unit

  def main(args: Array[String]): Unit = try {

    controller.extman.addExtension(new DependencyGraphExporter)
    controller.extman.addExtension(new DeclarationTreeExporter)
    controller.extman.addExtension(new JSONBasedGraphServer)
    controller.extman.addExtension(new PathGraphExporter)
      doFirst
      if (serverport.isDefined) {
        //controller.handleLine("clear")
        controller.handleLine("server on " + serverport.get)
      }
      run
      if (gotoshell) Run.main(Array())
    } catch {
      case e: api.Error => println(e.toStringLong)
        sys.exit
    }

  def hl(s : String) = controller.handleLine(s)
}

/**
  * As an example, here's my default. All test files of mine just extend this:
  */
abstract class DennisTest(prefixes : String*) extends Test(
  "/home/jazzpirate/work/MathHub",
  prefixes.toList,
  "/home/jazzpirate/work/Stuff/Public",
  Some(8080),
  true,
  Some("/home/jazzpirate/work/mmtlog.html")
) {
}

abstract class TomTest(prefixes : String*) extends Test(
  "/home/twiesing/Projects/KWARC/MathHub/localmh/MathHub",
  prefixes.toList,
  "/home/twiesing/Projects/KWARC/MathHub/localmh/MathHub/alignments/Public",
  Some(8080),
  true,
  None
)