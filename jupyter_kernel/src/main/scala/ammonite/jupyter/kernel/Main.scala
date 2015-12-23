package ammonite.jupyter.kernel

import ammonite.jupyter.kernel.utils.{HeartbeatListener, SocketListener}
import org.json4s._
import org.json4s.jackson.JsonMethods.{parse => jsonParse}
import scala.io.Source

import org.zeromq.ZContext;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZMQ;



case class JupyterSocketInfo(socketType: Int)

object JupyterSocketInfo {
  import org.zeromq.ZMQ._
  import ammonite.jupyter.kernel.utils.ShellListener

  type JupyterSocketKey = String

  val Shell: JupyterSocketKey = "shell"
  val IOPub:JupyterSocketKey = "iopub"
  val Stdin:JupyterSocketKey = "stdin"
  val Control:JupyterSocketKey = "control"
  val Hb: JupyterSocketKey = "hb"

  val SocketKeys = Seq(Shell, IOPub, Stdin, Control, Hb)

  val SocketTypes: Map[JupyterSocketKey, JupyterSocketInfo] = Map(Shell -> JupyterSocketInfo(ROUTER),
  IOPub -> JupyterSocketInfo(PUB),
  Stdin -> JupyterSocketInfo(ROUTER),
  Control -> JupyterSocketInfo(ROUTER),
  Hb -> JupyterSocketInfo(REP))


  def createListener(ctx: ZContext, key: JupyterSocketKey, ip: String, port: Int): SocketListener = {
    val socket = ctx.createSocket(SocketTypes(key).socketType)
    socket.bind("tcp://" + ip + ":" + port)

    key match {
      case Shell => new ShellListener(socket)
      case Hb => new HeartbeatListener(socket)
      case _ => new ShellListener(socket)
    }
  }



}




case class JupyterConnectionInfo(portNumMap: Map[JupyterSocketInfo.JupyterSocketKey, Int],
                                 transport: String,
                                 signature_scheme: String,
                                 ip: String,
                                 key: String) {

  import JupyterSocketInfo._
  val ctx = new ZContext()

  val listenerMap: Map[JupyterSocketKey, SocketListener] = portNumMap.map{ case (key, portNum) =>
    (key, createListener(ctx, key, ip, portNum)) }

  val poller = new ZMQ.Poller(SocketKeys.size)

  SocketKeys.foreach {case key =>
    poller.register(listenerMap(key).socket, ZMQ.Poller.POLLIN)
  }


  def run = {
    while (!Thread.currentThread().isInterrupted) {
      //println("Polling")
      poller.poll()
      //println(" Past polling")
      SocketKeys.zipWithIndex.foreach{ case (key, idx) =>
          poller.pollin(idx) match {
            case true =>
              println("Socket " + key  + " got msg")
              listenerMap(key).recvAndProcessMsg
            case false =>
          }
      }

    }
  }

}

object JupyterConnectionInfoUtils {
  def parse(txt: String): JupyterConnectionInfo = {
    val connObj : JValue =  jsonParse(txt)

    implicit val format = org.json4s.DefaultFormats

    val portMap: Map[JupyterSocketInfo.JupyterSocketKey, Int] =
      JupyterSocketInfo.SocketKeys.map{ k => (k, (connObj \ (k + "_port")).extract[Int])}.toMap

    JupyterConnectionInfo(portMap,
      (connObj \ "transport").extract[String],
      (connObj \ "signature_scheme").extract[String],
      (connObj \ "ip").extract[String],
      (connObj \ "key").extract[String])
  }
}


object Main {

  def main(args: Array[String]) = {
    val connectionFile = args(0)


    val connInfo = JupyterConnectionInfoUtils.parse(io.Source.fromFile(connectionFile).mkString)

    connInfo.run


  }
}