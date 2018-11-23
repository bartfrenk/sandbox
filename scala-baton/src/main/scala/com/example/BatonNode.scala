package com.example.baton

import akka.actor.{ Actor, ActorLogging, ActorRef, ActorSystem, Props }

object Node {

  def props(id: Int, seed: Option[ActorRef]): Props = Props(new Node(id, seed))

  case class Route(address: ActorRef)

  case class JoinRequest()
  case class JoinResponse(next: Option[ActorRef])

  type RoutingTable = List[Option[Route]]
}


class Node(id: Int, seed: Option[ActorRef]) extends Actor with ActorLogging {
  import Node._

  log.info(s"Starting node ${id}")
  var left = None
  var right = None
  var parent = None
  val leftRoutes = List.empty
  val rightRoutes = List.empty

  seed match {
    case None => log.info("No seed passed")
    case Some(actorRef) => actorRef ! JoinRequest()
  }

  def receive = {
    case JoinRequest() => {
      val currentSender = sender()
      log.info(s"Received join request from ${currentSender}")
      sender() ! JoinResponse(None)
    }
    case JoinResponse(next) => {
      val currentSender = sender()
      log.info(s"Received join response from ${currentSender}")
    }
  }
}



object Main extends App {

  val system: ActorSystem = ActorSystem("baton")

  val actor1: ActorRef = system.actorOf(Node.props(0, None))
  val actor2: ActorRef = system.actorOf(Node.props(1, Some(actor1)))

}
