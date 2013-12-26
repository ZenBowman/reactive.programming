package kvstore

import akka.actor.{OneForOneStrategy, Props, ActorRef, Actor}
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ask, pipe}
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout
import kvstore.Persistence.{Persisted, Persist}
import kvstore.Replicator.{Waive, Replicated, Replicate, SnapshotAck}
import kvstore.Replica.{OperationFailed, OperationAck}

object Replica {

  sealed trait Operation {
    def key: String

    def id: Long
  }

  case class Insert(key: String, value: String, id: Long) extends Operation

  case class Remove(key: String, id: Long) extends Operation

  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply

  case class OperationAck(id: Long) extends OperationReply

  case class OperationFailed(id: Long) extends OperationReply

  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class PersistenceHelper(val persistor: ActorRef, val ackTarget: ActorRef, val persistenceMsg: Persist) extends Actor {

  import Persistence._
  import scala.concurrent.duration._
  import context.dispatcher

  val scheduledMsg = context.system.scheduler.schedule(5 milliseconds, 50 milliseconds, persistor, persistenceMsg)


  def receive = {
    case Persisted(key, id) =>
      ackTarget ! SnapshotAck(key, id)
      scheduledMsg.cancel()
      context.stop(self)
  }

}

class OperationPersistenceAndReplicationHelper(val ackTarget: ActorRef,
                                               persistor: ActorRef,
                                               replicators: Set[ActorRef],
                                               replicationMsg: Replicate) extends Actor {

  import scala.concurrent.duration._
  import context.dispatcher

  var persisted: Boolean = false
  var replicationsAcknowleged = Set.empty[ActorRef]

  case object FlushResult

  case object CheckResult

  val scheduledMsg1 = context.system.scheduler.schedule(100 milliseconds, 100 milliseconds, self, CheckResult)
  val scheduledMsg2 = context.system.scheduler.scheduleOnce(1 second, self, FlushResult)

  val persistenceMessage = Persist(replicationMsg.key, replicationMsg.valueOption, replicationMsg.id)
  context.system.actorOf(Props(classOf[PersistenceHelper], persistor, self, persistenceMessage))


  for (replicator <- replicators) {
    replicator ! replicationMsg
  }

  def conditionsSatisfied: Boolean = {
    (replicators == replicationsAcknowleged) && persisted
  }

  def acknowledgeIfComplete() {
    if (conditionsSatisfied) {
      ackTarget ! OperationAck(replicationMsg.id)
      scheduledMsg1.cancel()
      scheduledMsg2.cancel()
      context.stop(self)
    }
  }

  def acknowledgeOrFail() {
    if (conditionsSatisfied) {
      ackTarget ! OperationAck(replicationMsg.id)
    } else {
      Console.println("Replicators: " + replicators)
      Console.println("Acknowledged: " + replicationsAcknowleged)
      ackTarget ! OperationFailed(replicationMsg.id)
    }
    scheduledMsg1.cancel()
    scheduledMsg2.cancel()
    context.stop(self)
  }

  def receive = {
    case Replicated(key, id) =>
      val r = sender
      replicationsAcknowleged += r
      acknowledgeIfComplete()

    case Waive(replicator) =>
      replicationsAcknowleged += replicator
      acknowledgeIfComplete()

    case SnapshotAck(key, id) =>
      persisted = true
      acknowledgeIfComplete()

    case CheckResult =>
      acknowledgeIfComplete()

    case FlushResult =>
      acknowledgeOrFail()
  }
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {

  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  arbiter ! Join

  val persistor = context.system.actorOf(persistenceProps)

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Map.empty[ActorRef, ActorRef]

  var pendingOperations = Set.empty[ActorRef]

  def receive = {
    case JoinedPrimary => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Insert(key, value, id) =>
      kv += key -> value
      val ackTarget = sender
      val replicationMsg = Replicate(key, Some(value), id)
      val newPendingOperation = context.system.actorOf(Props(classOf[OperationPersistenceAndReplicationHelper],
        ackTarget, persistor, replicators.values.toSet, replicationMsg))
      pendingOperations += newPendingOperation
    case Remove(key, id) =>
      kv -= key
      val ackTarget = sender
      val replicationMsg = Replicate(key, None, id)
      val newPendingOperation = context.system.actorOf(Props(classOf[OperationPersistenceAndReplicationHelper],
        ackTarget, persistor, replicators.values.toSet, replicationMsg))
      pendingOperations += newPendingOperation

    case Get(key, id) =>
      if (kv.contains(key)) {
        sender ! GetResult(key, Some(kv(key)), id)
      } else {
        sender ! GetResult(key, None, id)
      }

    case Replicas(replicaSet) =>
      for (replica <- replicaSet) {
        if (!replicators.contains(replica)) {
          if (replica != self) {
            val replicator = context.actorOf(Props(classOf[Replicator], replica))
            replicators += replica -> replicator
            for (elem <- kv.keys) {
              replicator ! Replicate(elem, Some(kv(elem)), 0L)
            }
          }
        }
      }
      val replicasToRemove = for (replicaKey <- replicators.keySet if (!replicaSet.contains(replicaKey))) yield replicaKey
      for (rKey <- replicasToRemove) {
        for (pendingOperation <- pendingOperations) {
          pendingOperation ! Waive(replicators(rKey))
        }
        replicators(rKey) ! PoisonPill
        replicators -= rKey
      }

    case _ =>
  }

  var expectedSequenceNumber = 0L
  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    case Snapshot(key, valueOption, sequenceNumber) =>
      if (sequenceNumber == expectedSequenceNumber) {
        valueOption match {
          case Some(value) =>
            kv += key -> value
          case None =>
            kv -= key
        }
        val ackTarget = sender
        val persistenceMessage = Persist(key, valueOption, sequenceNumber)
        context.system.actorOf(Props(classOf[PersistenceHelper], persistor, ackTarget, persistenceMessage))
        expectedSequenceNumber += 1
      } else {
        if (sequenceNumber < expectedSequenceNumber) {
          sender ! SnapshotAck(key, sequenceNumber)
        }
      }

    case Get(key, id) =>
      if (kv.contains(key)) {
        sender ! GetResult(key, Some(kv(key)), id)
      } else {
        sender ! GetResult(key, None, id)
      }


    case _ =>
  }

}
