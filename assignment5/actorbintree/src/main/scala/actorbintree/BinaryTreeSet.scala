/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef

    def id: Int

    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection */
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}

class BinaryTreeSet extends Actor {

  import BinaryTreeSet._
  import BinaryTreeNode._

  trait BTreeState

  case object NormalState extends BTreeState

  case object GCState extends BTreeState

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var state: BTreeState = NormalState
  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = {
    if (state == NormalState) {
      normal
    } else {
      val oldRoot = root
      val newRoot = createRoot
      garbageCollecting(newRoot)
    }
  }

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case x: Insert =>
      root ! x
    case x: Contains =>
      root ! x
    case x: Remove =>
      root ! x
    case GC =>
      state = GCState
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case _ => {}
  }

}

object BinaryTreeNode {

  trait Position

  case object Left extends Position

  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)

  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode], elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {

  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  def insert(requester: ActorRef, id: Int, element: Int) = {
    if (element < elem) {
      if (subtrees.contains(Left)) {
        subtrees(Left) ! Insert(requester, id, element)
      } else {
        val lSubtree = context.actorOf(BinaryTreeNode.props(element, initiallyRemoved = false))
        subtrees = subtrees + (Left -> lSubtree)
        requester ! OperationFinished(id)
      }
    }
    else if (element > elem) {
      if (subtrees.contains(Right)) {
        subtrees(Right) ! Insert(requester, id, element)
      } else {
        val rSubtree = context.actorOf(BinaryTreeNode.props(element, initiallyRemoved = false))
        subtrees = subtrees + (Right -> rSubtree)
        requester ! OperationFinished(id)
      }
    }
    else {
      // Same as item
      removed = false
      requester ! OperationFinished(id)
    }
  }

  def remove(requester: ActorRef, id: Int, element: Int) = {
    if (element < elem) {
      if (subtrees.contains(Left)) {
        subtrees(Left) ! Remove(requester, id, element)
      } else {
        requester ! OperationFinished(id) // no such element exits
      }
    }
    else if (element > elem) {
      if (subtrees.contains(Right)) {
        subtrees(Right) ! Remove(requester, id, element)
      } else {
        requester ! OperationFinished(id) // no such element exists
      }
    }
    else {
      // Same as item
      removed = true
      requester ! OperationFinished(id)
    }
  }

  def contains(requester: ActorRef, id: Int, element: Int) = {
    if (element < elem) {
      if (subtrees.contains(Left)) {
        subtrees(Left) ! Contains(requester, id, element)
      } else {
        requester ! ContainsResult(id, false) // no such element exits
      }
    }
    else if (element > elem) {
      if (subtrees.contains(Right)) {
        subtrees(Right) ! Contains(requester, id, element)
      } else {
        requester ! ContainsResult(id, false) // no such element exists
      }
    }
    else {
      // Same as item
      if (removed) {
        requester ! ContainsResult(id, false)
      } else {
        requester ! ContainsResult(id, true)
      }
    }
  }


  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Insert(requester, id, element) =>
      insert(requester, id, element)
    case Remove(requester, id, element) =>
      remove(requester, id, element)
    case Contains(requester, id, element) =>
      contains(requester, id, element)
  case _ => {}
}

// optional
/** `expected` is the set of ActorRefs whose replies we are waiting for,
  * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
  */
def copying (expected: Set[ActorRef], insertConfirmed: Boolean): Receive = ???

}
