package actorbintree

import junit.framework.TestCase
import akka.actor.{Props, ActorSystem}
import akka.testkit.TestProbe
import actorbintree.BinaryTreeSet.{OperationFinished, Insert}

class BinaryTreeCustomTests extends TestCase {

  def testBasicFunctionality() {
    implicit val system = ActorSystem("Tester")
    val btree = system.actorOf(Props[BinaryTreeSet])
    val p = TestProbe()
    p.send(btree, Insert(p.ref, 1, 10))
    p.expectMsg(OperationFinished(1))

    system.shutdown()
  }

}
