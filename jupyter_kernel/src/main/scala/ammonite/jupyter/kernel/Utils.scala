package ammonite.jupyter.kernel.utils

import org.zeromq.ZMQ.Socket;
import org.json4s._
import org.json4s.jackson.JsonMethods.{parse => jsonParse}
import org.json4s.jackson.Serialization.write

import scala.annotation.tailrec


case class SocketMessage(data: Array[Byte], hasReceiveMore: Boolean) {

  override def toString = "msg: " + new String(data) + "\n\nhasReceiveMore: " + hasReceiveMore
}

object SocketMessage {

  type SocketMessageQueue = scala.collection.mutable.Queue[SocketMessage]
  type RawDataBuffer = Array[Byte]

}

import SocketMessage._

abstract case class SocketListener(socket: Socket) {

  def recvMsg: SocketMessage = {
    SocketMessage(socket.recv(), socket.hasReceiveMore)
  }


  def recvAndProcessMsg = {
    processMsg(recvMsg)
  }

  var msgQueue = new SocketMessageQueue()

  def processMsg(msg: SocketMessage) = {
    //println("Got shell msg: " + msg)
    msgQueue += msg

    if (!msg.hasReceiveMore) {
      //println("\tGot last of multipart shell msgs")
      processQueue
    }
  }

  private def processQueue() = {
    val msg = JupyterMessage.createFromQueue(msgQueue)

    processJupyterMsg(msg)
  }

  def processJupyterMsg(msg: JupyterMessage)



}

object JupyterMessage {
  val DELIMITER = "<IDS|MSG>"

  def createFromQueue(q: SocketMessageQueue) : JupyterMessage = {
    val uuids = readUUIDs(q)
    readFixedFields(q, uuids)
  }


  def readUUIDs(q: SocketMessageQueue, uuids: Array[String] = new Array[String](0)) : Array[String] = {
    val headStr = new String(q.dequeue.data)
    headStr match {
      case JupyterMessage.DELIMITER => uuids
      case _ => {
        //val newMsg = msg.copy(uuids = msg.uuids :+ headStr)
        readUUIDs(q, uuids :+ headStr)
      }
    }
  }

  def readFixedFields(q: SocketMessageQueue, uuids: Array[String]) : JupyterMessage = {

    val hmac = new String(q.dequeue.data)
    val header = jsonParse(new String(q.dequeue.data))
    val parentHeader = jsonParse(new String(q.dequeue.data))
    val metadata = jsonParse(new String(q.dequeue.data))
    val content = jsonParse(new String(q.dequeue.data))

    JupyterMessage(uuids, hmac, header, parentHeader, metadata, content,
      q.dequeueAll{ _ => true }.map { _.data }.toArray)
  }




}

case class JupyterMessage(uuids: Array[String] = new Array[String](0),
                          hmac: String = "",
                          header: JValue = JNothing,
                          parentHeader: JValue = JNothing,
                          metadata: JValue = JNothing,
                          content: JValue = JNothing,
                          rawDataBuffers: Array[RawDataBuffer] = new Array[RawDataBuffer](0)) {



  override def toString : String = {


    Seq("uuids: " + uuids.reduce( _ + " " + _ ),
      "hmac: " + hmac,
      "header: " + header,
      "parentHeader: " + parentHeader,
      "metadata: " + metadata,
      "content: " + content,
      "rawDataBuffers: " + ((rawDataBuffers.isEmpty) match {
        case true => ""
        case false=> rawDataBuffers.map
        { new String(_) }.reduce( _ + " " + _)})

    ).map { "\t" + _ } .reduce ( _ + "\n" + _ )
  }

  implicit val format = org.json4s.DefaultFormats
  def msgType = (header \ "msg_type").extract[String]

  def code = (content \ "code").extract[String]


  def send(socket: Socket) = {

    uuids.foreach{ u => socket.sendMore(u)}
    socket.sendMore(hmac)
    socket.sendMore(write(header))
    socket.sendMore(write(parentHeader))
    socket.sendMore(write(metadata))
    socket.sendMore(write(content))



  }



}

object ShellMsgInfo {
  val KERNEL_INFO_REQUEST = "kernel_info_request"
  val EXECUTE_REQUEST = "execute_request"

  val KERNEL_INFO_REQUEST_CONTENT = jsonParse(
    """
      | content = { "protocol_version": "5.0",
      | "implementation": "Ammonite Scala",
      | "implementation_version":"0.0.1",
      | "language_info": {
      |   "name":"scala",
      |   "version":"2.11.7",
      |   "mimetype":"text/x-scala-source",
      |   "file_extension":".scala",
      |   },
      |   "banner":"Ammonite Scala Kernel",
      | }
      |
      |
      |
    """.stripMargin)

}


class ShellListener(socket: Socket) extends SocketListener(socket) {
  import org.json4s.{JValue, JNothing}
  import ShellMsgInfo._


  private def processJupyterMsg(msg: JupyterMessage) = {

    msg.msgType match {
      case KERNEL_INFO_REQUEST => {
        //println("Sending kernel info request")
       // socket.send(KERNEL_INFO_REQUEST_RESPONSE)

      }
      case EXECUTE_REQUEST => {
        val code = msg.code
        println("Will execute code: " + code)
      }
    }
  }

}


class HeartbeatListener(socket: Socket) extends SocketListener(socket) {

  def processMsg(msg: SocketMessage) = {
    println("Got heartbeat msg")
    socket.send(msg.data)
  }
}

