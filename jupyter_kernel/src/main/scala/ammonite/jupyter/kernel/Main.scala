package ammonite.jupyter.kernel

import java.io.{PrintStream, ByteArrayOutputStream, InputStream}

import _root_.caseapp.App
import _root_.caseapp.AppOf
import ammonite.ops.Path
import ammonite.repl.frontend.{AmmoniteFrontEnd, FrontEnd}
import ammonite.repl.interp.Interpreter
import ammonite.repl._
import jupyter.kernel.protocol.Output.LanguageInfo
import jupyter.kernel.{interpreter, KernelInfo}
import jupyter.kernel.config.Module
import jupyter.kernel.interpreter//.{InterpreterKernel, Interpreter}
import jupyter.kernel.server.{ServerAppOptions, ServerApp}

import caseapp._
import com.typesafe.scalalogging.slf4j.LazyLogging

import scalaz.\/

object ScalaModule extends Module {
  val scalaBinaryVersion = scala.util.Properties.versionNumberString.split('.').take(2).mkString(".")

  val kernelId = s"scala${scalaBinaryVersion.filterNot(_ == '.')}"
  val kernel = new jupyter.kernel.interpreter.InterpreterKernel {
    def apply() = {} // \/.fromTryCatchNonFatal(ScalaInterpreter())
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
    new jupyter.kernel.interpreter.InterpreterKernel {
      def apply() = \/.fromTryCatchNonFatal(ScalaInterpreter())
    },
    ScalaModule.kernelInfo,
    progPath,
    options,
    logos = Seq(
      resource(s"kernel/scala-$scalaBinaryVersion/resources/logo-64x64.png").map((64, 64) -> _),
      resource(s"kernel/scala-$scalaBinaryVersion/resources/logo-32x32.png").map((32, 32) -> _)
    ).flatten
  )
}


object ScalaInterpreter {
  trait InterpreterDefaults extends interpreter.Interpreter {
    override def resultDisplay = true // Displaying results directly, not under Jupyter "Out" prompt

    val languageInfo = LanguageInfo(
      name=s"scala${scalaBinaryVersion.filterNot(_ == '.')}",
      version = scalaVersion,
      codemirror_mode = "text/x-scala",
      file_extension = "scala",
      mimetype = "text/x-scala",
      pygments_lexer = "scala"
    )

    override val implementation = ("jupyter-scala", s"${BuildInfo.version} (scala $scalaVersion)")
    override val banner =
      s"""Jupyter Scala ${BuildInfo.version} (Ammonite ${BuildInfo.ammoniteVersion} fork) (Scala $scalaVersion)
       """.stripMargin
  }


  def apply() : Interpreter = {

    def defaultAmmoniteHome = Path(System.getProperty("user.home"))/".ammonite"

    new jupyter.kernel.interpreter.Interpreter with InterpreterDefaults {
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
      val underlying: Interpreter = new Interpreter(
        prompt,
        frontEnd,
        frontEnd().width,
        frontEnd().height,
        pprint.Config.Colors.PPrintConfig,
        colors,
        println,
        //printer.print,
        storage,
        history,
        "",
        Nil
      )





    }
  }
}


object JupyterScala extends AppOf[JupyterScala] {
  val parser = default
}
