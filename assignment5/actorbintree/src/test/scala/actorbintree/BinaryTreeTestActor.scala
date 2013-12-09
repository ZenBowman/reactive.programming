package actorbintree

import akka.actor.{Props, Actor}
import actorbintree.BinaryTreeSet.{Remove, GC, Contains, Insert}


class BinaryTreeTestActor extends Actor {
  def receive: Receive = {
    case msg => println(s"Received message: $msg")
  }
}

object BinaryTreeTester {
  def main(args: Array[String]) {
    val system = akka.actor.ActorSystem.create()
    val topNode = system.actorOf(Props[BinaryTreeSet])
    val tester = system.actorOf(Props[BinaryTreeTestActor])

    topNode ! Insert(tester, 1, 1)
    topNode ! Insert(tester, 2, 2)
    topNode ! Insert(tester, 3, 3)
    topNode ! Insert(tester, 4, 4)
    topNode ! Contains(tester, 5, 5)
    topNode ! Contains(tester, 3, 6)
    topNode ! Remove(tester, 2, 7)
    topNode ! GC
    topNode ! Contains(tester, 2, 8)

    Thread.sleep(2000)
    system.shutdown()

  }
}