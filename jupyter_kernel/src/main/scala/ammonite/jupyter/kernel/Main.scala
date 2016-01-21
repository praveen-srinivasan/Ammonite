package ammonite.jupyter.kernel

import java.io.{PrintStream, ByteArrayOutputStream, InputStream}

import _root_.caseapp.App
import _root_.caseapp.AppOf
import ammonite.ops.Path
import ammonite.repl.frontend.{AmmoniteFrontEnd, FrontEnd}
import ammonite.repl.interp.Interpreter
import ammonite.repl._
import jupyter.api.Publish
import jupyter.kernel.interpreter.DisplayData
import jupyter.kernel.interpreter.DisplayData.{RawData, EmptyData}
import jupyter.kernel.protocol.Output.LanguageInfo
import jupyter.kernel.protocol.ParsedMessage
import jupyter.kernel.{interpreter, KernelInfo}
import jupyter.kernel.config.Module
import jupyter.kernel.interpreter//.{InterpreterKernel, Interpreter}
import jupyter.kernel.server.{ServerAppOptions, ServerApp}
import scalaz.BuildInfo

import caseapp._
import com.typesafe.scalalogging.slf4j.LazyLogging

import scalaz.\/

object DebugUtils {
  def myPrint(s: String): Unit = {
    System.err.println(s)
  }
}

object ScalaModule extends Module {
  val scalaBinaryVersion = scala.util.Properties.versionNumberString.split('.').take(2).mkString(".")

  val kernelId = s"scala${scalaBinaryVersion.filterNot(_ == '.')}"
  val kernel = new jupyter.kernel.interpreter.InterpreterKernel {

    val interpreter = ScalaInterpreter()

    val a = DebugUtils.myPrint("ScalaModule: just before init")
    interpreter.init()
    val b = DebugUtils.myPrint("ScalaModule: just after init")

    def apply() = {DebugUtils.myPrint("ScalaModule: just before disjunction!")
      \/.fromTryCatchNonFatal(interpreter)}
  }
  val kernelInfo = KernelInfo(s"Scala $scalaBinaryVersion", kernelId)

  def kernels = Map(
    kernelId -> (kernel, kernelInfo)
  )
}


case class JupyterScala(options: ServerAppOptions) extends App with LazyLogging {

  // FIXME Shouldn't sbt-pack put this in system property "prog.name"?
  val progName = "jupyter-scala"

  lazy val isWindows: Boolean =
    Option(System.getProperty("os.name")).exists(_ startsWith "Windows")

  lazy val progSuffix =
    if (isWindows) ".bat"
    else ""

  def progPath =
    Option(System getProperty "prog.home").filterNot(_.isEmpty)
      .map(List(_, "bin", progName + progSuffix).mkString(java.io.File.separator))
      .getOrElse {
        Console.err println "Cannot get program home dir, it is likely we are not run through pre-packaged binaries."
        Console.err println "Please edit the generated file below, and ensure the first item of the 'argv' list points to the path of this program."
        progName
      }

  def readFully(is: InputStream) = {
    val buffer = new ByteArrayOutputStream()

    var nRead = 0
    val data = Array.ofDim[Byte](16384)

    nRead = is.read(data, 0, data.length)
    while (nRead != -1) {
      buffer.write(data, 0, nRead)
      nRead = is.read(data, 0, data.length)
    }

    buffer.flush()
    buffer.toByteArray
  }

  def resource(path: String): Option[Array[Byte]] = {
    for (is <- Option(Thread.currentThread().getContextClassLoader.getResourceAsStream(path))) yield {
      try readFully(is)
      finally is.close()
    }
  }

  val scalaBinaryVersion = _root_.scala.util.Properties.versionNumberString.split('.').take(2).mkString(".")

  ServerApp(
      ScalaModule.kernelId,
    ScalaModule.kernel,
    //new jupyter.kernel.interpreter.InterpreterKernel {
     // def apply() =  ScalaModule.kernel() //\/.fromTryCatchNonFatal(ScalaInterpreter())
    //},
    ScalaModule.kernelInfo,
    progPath,
    options,
    logos = Seq(
      resource(s"kernel/scala-$scalaBinaryVersion/resources/logo-64x64.png").map((64, 64) -> _),
      resource(s"kernel/scala-$scalaBinaryVersion/resources/logo-32x32.png").map((32, 32) -> _)
    ).flatten
  )
}

final class Evidence private[jupyter] (private[jupyter] val underlying: Any)

class RawAndHtmlData(s: String, htmls: Array[String]) extends RawData(s) {
  override def data: Seq[(String,String)] =
    { val r = RawData(s).data(0)._2 // Truncated string



      val txtPlain =  Seq( "text/plain" -> r)

      lazy val txtHtml = {r.isEmpty match {
        case true => Seq.empty[String]
        case false => Seq(r.replaceAll("(\r\n|\n)", "<BR/>"))
      }}


      (htmls.length > 0) match {
        case false => txtPlain
        case true =>
          Seq( "text/html" ->  (txtHtml ++ Seq(htmls.reduce ( _ + "<BR/>\n" +_))).reduce ( _ + "<BR/>\n" + _  ))
      }
  }
}


object ScalaInterpreter {
  trait InterpreterDefaults extends interpreter.Interpreter {
    override def resultDisplay = true // Displaying results directly, not under Jupyter "Out" prompt

    val scalaVersion = scala.util.Properties.versionNumberString
    val scalaBinaryVersion = scala.util.Properties.versionNumberString.split('.').take(2).mkString(".")


    val languageInfo = LanguageInfo(
      name=s"scala${scalaBinaryVersion.filterNot(_ == '.')}",
      version = scalaVersion,
      codemirror_mode = "text/x-scala",
      file_extension = "scala",
      mimetype = "text/x-scala",
      pygments_lexer = "scala"
    )
    //${BuildInfo.ammoniteVersion}

    override val implementation = ("jupyter-scala", s"${BuildInfo.version} (scala $scalaVersion)")
    override val banner =
      s"""Jupyter Scala ${BuildInfo.version} (Ammonite  fork) (Scala $scalaVersion)
       """.stripMargin
  }


  def apply() : jupyter.kernel.interpreter.Interpreter = {

    def defaultAmmoniteHome = Path(System.getProperty("user.home"))/".ammonite"


      val predefFile = None
      val storage = Storage(defaultAmmoniteHome, predefFile) // TODO: predef file instead of None

      val prompt = Ref("@ ")

      val colors = Ref[Colors](Colors.Default)
      val frontEnd = Ref[FrontEnd](AmmoniteFrontEnd(
        PartialFunction.empty
      ))

      //val printer = new PrintStream(output, true)
      var history = new History(Vector())

      Timer("Repl init printer")

      var initialized0 = false

      lazy val underlying: Interpreter = {
        initialized0 = false;
        val intp = new Interpreter(
          prompt,
          frontEnd,
          //frontEnd().width,
          119,
          30,
          pprint.Config.Colors.PPrintConfig,
          colors,
          println,
          //printer.print,
          storage,
          history,
          "",
          Nil
        )
        initialized0 = true
        intp
      }

    var currentPublish = Option.empty[Publish[Evidence]]
    var currentMessage = Option.empty[ParsedMessage[_]]

    new jupyter.kernel.interpreter.Interpreter with InterpreterDefaults {


        override def initialized = initialized0
        override def init() = {DebugUtils.myPrint("ScalaInterpreter: just before underlying; initialized: " + initialized)
          underlying
          DebugUtils.myPrint("ScalaInterpreter: just after underlying; initialized: " + initialized) }

        def executionCount = underlying.replApi.history.length

        override def publish(publish: Publish[ParsedMessage[_]]) = {
          currentPublish = Some(publish.contramap[Evidence](e => e.underlying.asInstanceOf[ParsedMessage[_]]))
        }

        def complete(code: String, pos: Int) = {
          val (pos0: Int, completions: Seq[String],
            signatures) = underlying.pressy.complete(pos, underlying.eval.previousImportBlock, code)

          //signatures.foreach{s => println(s)}
          // Check if we have
          val otherCompletions =
            underlying.completionHandlers.flatMap{ handler => handler.complete(code,pos)._2}


          //val (pos0, completions, _) = underlying.complete(pos, code)
          (pos0, otherCompletions ++ completions)
        }

        def interpret(line: String,
                      output: Option[(String => Unit, String => Unit)],
                      storeHistory: Boolean,
                      current: Option[ParsedMessage[_]]) = {

          //currentMessage = current
          //println("PRAVEEN: Interpreting line: " + line)

         if (line != "") {
            ///underlying.storage.fullHistory() = underlying.storage().fullHistory() :+ code
            history = history :+ line
          }

          val existingOut = System.out
          try {
            //val rawDataResult = new jupyter.kernel.interpreter.DisplayData.RawData
            var outputString = ""
            Parsers.Splitter.parse(line) match {

              case f: fastparse.core.Parsed.Failure if line.drop(f.index).trim() == "" =>
                interpreter.Interpreter.Incomplete
              case f: fastparse.core.Parsed.Failure =>
                interpreter.Interpreter.Error(SyntaxError.msg(line, f.lastParser, f.index))
              case fastparse.core.Parsed.Success(split, parseEndIdx) =>
                // case fastparse.core.Result.Success(split, _) =>

                // Capture stdout

                val baos = new ByteArrayOutputStream();
                val ps = new PrintStream(baos, false)

                System.setOut(ps)
                Console.setOut(ps)
                val res = underlying.processLine(line,
                  split,
                  (it:Iterator[String]) => (outputString = it.mkString)
                )




                //System.setOut(existingOut)

                val htmlOutput: Array[String] = underlying.htmlBuffer.toArray

                underlying.htmlBuffer.clear

                res match {
                  case Res.Exit(v) => interpreter.Interpreter.Error("Close this notebook to exit")
                  case Res.Failure(reason) => {interpreter.Interpreter.Error(reason)}
                  case Res.Exception(t,s) => {interpreter.Interpreter.Exception(t.getClass.getName, t.getMessage,
                    t.getStackTrace.map{ _.toString }.toList,
                    t)}
                  case Res.Skip => interpreter.Interpreter.NoValue

                  case r @ Res.Success(ev) =>
                    underlying.handleOutput(r)

                    interpreter.Interpreter.Value( new RawAndHtmlData({ps.flush();

                      (((baos.size() > 0) match {
                        case true => Seq(baos.toString)
                        case false => Seq.empty[String]
                      }) ++ Seq(outputString)).reduce { _ + "\n" + _ }
                       },
                      htmlOutput) )

                  //new RawData(outputString))
                }
            }
          }
          finally {
            System.setOut(existingOut)
            Console.setOut(existingOut)

            currentMessage = None
          }

        }
      }





  }
}


object JupyterScala extends AppOf[JupyterScala] {
  val parser = default
  override def main(args: Array[String]): Unit = {
    super.main(args)
         //println("Hello, world!")
    }
}
