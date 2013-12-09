/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.concurrent.duration.Duration

object BinaryTreeSet {

  trait BinaryTreeOp

  trait Operation extends BinaryTreeOp {
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
  case object GC extends BinaryTreeOp

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

  case class CopyFinished(newRoot: ActorRef)

  case object Cleanup

}

class BinaryTreeSet extends Actor {

  import BinaryTreeSet._
  import BinaryTreeNode._

  trait BTreeState

  case object NormalState extends BTreeState

  case object GCState extends BTreeState

  case object InitiateGarbageCollection

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))
  var root = createRoot

  // optional
  val pendingQueue = mutable.Queue.empty[Operation]

  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case x: Insert =>
      processPendingOperations()
      root ! x
    case x: Contains =>
      processPendingOperations()
      root ! x
    case x: Remove =>
      processPendingOperations()
      root ! x
    case GC =>
      context.become(garbageCollecting)
      self ! InitiateGarbageCollection

  }


  def processOperation(oper: Operation) {
    oper match {
      case x: Insert =>
        root ! x
      case x: Contains =>
        root ! x
      case x: Remove =>
        root ! x
      case _  =>
        println("Unknown operation")
    }
  }

  def processPendingOperations() {
    while (pendingQueue.length > 0) {
      val oper = pendingQueue.dequeue()
      processOperation(oper)
    }
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting: Receive = {
    case InitiateGarbageCollection =>
      val newRoot = createRoot
      root ! CopyTo(self, newRoot)
    case x: Operation =>
      pendingQueue.enqueue(x)
    case CopyFinished(newRoot) =>
      val oldRoot = root
      root = newRoot
      oldRoot ! Cleanup
      processPendingOperations()
      context.become(normal)
  }


}

object BinaryTreeNode {

  trait Position

  case object Left extends Position

  case object Right extends Position

  case class CopyTo(requester: ActorRef, treeNode: ActorRef)

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode], elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {

  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = if (!waitingForCopy) normal else copying

  val sentConfirmations = new mutable.HashSet[Int]()

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
    else if (element == elem) {
      // Same as item
      if (removed) {
        requester ! ContainsResult(id, false)
      } else {
        requester ! ContainsResult(id, true)
      }
    }
  }

  var waitingForCopy = false
  var numCopiesRequested = 0
  var copyRequester: ActorRef = null
  var copyDestination: ActorRef = null

  def copy(req: ActorRef, newRoot: ActorRef) = {
    waitingForCopy = true
    numCopiesRequested = 0
    copyRequester = req
    copyDestination = newRoot

    if (!removed) {
      newRoot ! Insert(self, 0, elem)
      numCopiesRequested += 1
    }
    if (subtrees.contains(Left)) {
      subtrees(Left) ! CopyTo(self, newRoot)
      numCopiesRequested += 1
    }
    if (subtrees.contains(Right)) {
      subtrees(Right) ! CopyTo(self, newRoot)
      numCopiesRequested += 1
    }
  }

  def decrementCopyOperations() {
    numCopiesRequested -= 1
    if (numCopiesRequested == 0) {
      copyRequester ! CopyFinished(copyDestination)
      waitingForCopy = false
      context.become(normal)
    }
  }

  def cleanup() {
    import scala.concurrent.duration._

    if (subtrees.contains(Left)) {
      subtrees(Left) ! Cleanup
    }
    if (subtrees.contains(Right)) {
      subtrees(Right) ! Cleanup
    }
    sender ! Cleanup
    context.system.scheduler.scheduleOnce(5 seconds, self, PoisonPill)(context.dispatcher)
  }

  def copying: Receive = {
    case CopyFinished(_) => decrementCopyOperations()
    case OperationFinished(_) => decrementCopyOperations()
    case Cleanup => cleanup()
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
    case CopyTo(req, newRoot) =>
      if (!waitingForCopy) {
        copy(req, newRoot)
        context.become(copying)
      }
    case Cleanup => cleanup()
    case _ => {}
  }


}
