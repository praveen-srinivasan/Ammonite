package ammonite.jupyter.kernel.utils

import org.zeromq.ZMQ.Socket;
import org.json4s._
import org.json4s.jackson.JsonMethods.{parse => jsonParse}


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

  def processMsg(msg: SocketMessage)

  def recvAndProcessMsg = {
    processMsg(recvMsg)
  }
}

object ShellMessage {
  val DELIMITER = "<IDS|MSG>"

  def createFromQueue(q: SocketMessageQueue) : ShellMessage = {

    readUUID(q, new ShellMessage())
  }



  def readUUID(q: SocketMessageQueue, msg: ShellMessage) : ShellMessage = {
    val headStr = new String(q.dequeue.data)
    headStr match {
      case ShellMessage.DELIMITER => readFixedFields(q, msg)

      case _ => {
        val newMsg = msg.copy(uuids = msg.uuids :+ headStr)
        readUUID(q, newMsg)
      }
    }
  }

  def readFixedFields(q: SocketMessageQueue, msg: ShellMessage) : ShellMessage = {




    val hmac = new String(q.dequeue.data)

    val header = jsonParse(new String(q.dequeue.data))

    val parentHeader = jsonParse(new String(q.dequeue.data))

    val metadata = jsonParse(new String(q.dequeue.data))

    val content = jsonParse(new String(q.dequeue.data))

    msg.copy(hmac = hmac,
      header = header,
      parentHeader = parentHeader,
      metadata = metadata,
      content = content,
      rawDataBuffers = q.dequeueAll{ _ => true }.map { _.data }.toArray
    )



    //readmsg.copy(hmac = new String(q.head.data))

  }



}

case class ShellMessage(uuids: Array[String] = new Array[String](0),
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
}


class ShellListener(socket: Socket) extends SocketListener(socket) {
  import org.json4s.{JValue, JNothing}







  var msgQueue = new SocketMessageQueue()


  def processMsg(msg: SocketMessage) = {
    println("Got shell msg: " + msg)
    msgQueue += msg

    if (!msg.hasReceiveMore) {
      println("\tGot last of multipart shell msgs")
      processQueue
    }

    //println("Got msg: " + msg)
  }

  private def processQueue() = {
    val shellMsg = ShellMessage.createFromQueue(msgQueue)

    println("shellMsg: " + shellMsg)



  }

}


class HeartbeatListener(socket: Socket) extends SocketListener(socket) {

  def processMsg(msg: SocketMessage) = {
    println("Got heartbeat msg")
    socket.send(msg.data)
  }
}

