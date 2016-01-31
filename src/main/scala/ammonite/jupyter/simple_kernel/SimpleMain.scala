package ammonite.jupyter.simple_kernel


object SimpleJupyterScala {

  def main(args: Array[String]): Unit = {

    val inputPipeFilename = args(1)
    val outputPipeFilename = args(2)

    println("Input pipe filename: " + inputPipeFilename)
    println("Output pipe filename: " + outputPipeFilename)



  }

}