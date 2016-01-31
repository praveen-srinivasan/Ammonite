package ammonite.jupyter.simple_kernel

import java.io.{PrintStream, ByteArrayOutputStream}
import java.nio.charset.Charset

import ammonite.ops.Path
import ammonite.repl.frontend.{AmmoniteFrontEnd, FrontEnd}
import ammonite.repl._
import ammonite.repl.interp.Interpreter
import org.apache.commons.io.input.{Tailer, TailerListenerAdapter}

object InterpreterWrapper {

  case class ExecuteResult(text: Option[String] = None, html: Option[String] = None)

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

    def interpret(line: String, storeHistory:Boolean = true )
                  ///output: Option[(String => Unit, String => Unit)],
                 // storeHistory: Boolean)
                  = {

      //currentMessage = current
      println("PRAVEEN: Interpreting line: " + line)

      if (storeHistory && (line != "" )) {
        ///underlying.storage.fullHistory() = underlying.storage().fullHistory() :+ code
        history = history :+ line
      }

      val existingOut = System.out
      try {
        //val rawDataResult = new jupyter.kernel.interpreter.DisplayData.RawData
        var outputString = ""
        Parsers.Splitter.parse(line) match {

          case f: fastparse.core.Parsed.Failure if line.drop(f.index).trim() == "" =>
            ExecuteResult(Some("Incomplete"))
            //interpreter.Interpreter.Incomplete
          case f: fastparse.core.Parsed.Failure =>
            ExecuteResult(Some(SyntaxError.msg(line, f.lastParser, f.index)))
            //interpreter.Interpreter.Error(SyntaxError.msg(line, f.lastParser, f.index))
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
              case Res.Exit(v) => ExecuteResult(Some("Close this notebook to exist"))//interpreter.Interpreter.Error("Close this notebook to exit")
              case Res.Failure(reason) => ExecuteResult(Some(reason))// {interpreter.Interpreter.Error(reason)}
              case Res.Exception(t,s) =>  ExecuteResult(Some(
                (t.getClass.getName + ": " + t.getMessage :: t.getStackTrace.map{ _.toString }.toList.map("    " + _)).mkString("\n")))

               // s"$name: $msg" :: stackTrace.map("    " + _)

              //{interpreter.Interpreter.Exception(t.getClass.getName, t.getMessage,
                //t.getStackTrace.map{ _.toString }.toList,
                //t)}
              case Res.Skip => ExecuteResult(Some(""))//interpreter.Interpreter.NoValue

              case r @ Res.Success(ev) =>
                underlying.handleOutput(r)

                ExecuteResult(Some({ps.flush();

                  (((baos.size() > 0) match {
                    case true => Seq(baos.toString)
                    case false => Seq.empty[String]
                  }) ++ Seq(outputString)).reduce { _ + "\n" + _ }
                }))

                /*interpreter.Interpreter.Value( new RawAndHtmlData({ps.flush();

                  (((baos.size() > 0) match {
                    case true => Seq(baos.toString)
                    case false => Seq.empty[String]
                  }) ++ Seq(outputString)).reduce { _ + "\n" + _ }
                },
                  htmlOutput) )*/

              //new RawData(outputString))
            }
        }
      }
      finally {
        System.setOut(existingOut)
        Console.setOut(existingOut)

      }

    }
  }

class MyTailerListener(outputStream: java.io.BufferedWriter) extends TailerListenerAdapter {
  override def handle(line: String) {
    println("Interpreting: " + line)
    val output = InterpreterWrapper.interpret(line)
    println("Returning: " + output)

    output.text match {
      case Some(s) => outputStream.write(s + "\n")
    }
    //System.out.println(line);
  }
}

object SimpleJupyterScala {

  def main(args: Array[String]): Unit = {
    import java.io._
    val underlyingStr = InterpreterWrapper.underlying.toString


    val inputPipeFilename = args(0)
    val outputPipeFilename = args(1)


    println("Input pipe filename: " + inputPipeFilename)
    println("Output pipe filename: " + outputPipeFilename)

    val inputStream = new BufferedReader(new InputStreamReader(new BufferedInputStream(
      new FileInputStream(inputPipeFilename))))



    //val listener = new MyTailerListener(outputStream)
    //val tailer = Tailer.create(new java.io.File(inputPipeFilename), listener, 10);

     while(true) {
       val line = inputStream.readLine()
        if (line != null && !line.isEmpty) {

          println("Interpreting: " + line)
          val output = InterpreterWrapper.interpret(line)
          println("Returning: " + output)

          output.text match {
            case Some(s) => {
              val outputStream = //new BufferedWriter(new OutputStreamWriter(new BufferedOutputStream(
                new FileOutputStream(outputPipeFilename) //)))

              outputStream.write((s + "\n").getBytes(Charset.forName("UTF-8")))
              outputStream.close() }
          }
        }
        Thread.sleep(10L)
      //outputStream.write(output.text.get + "\n")
    }



  }

}