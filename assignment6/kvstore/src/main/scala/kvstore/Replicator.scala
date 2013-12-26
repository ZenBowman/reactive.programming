package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import scala.concurrent.duration._

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  case class Waive(replicator: ActorRef)

  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  case object RetransmitUnacknowledged

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher
  import scala.concurrent.duration._

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Replicate)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]
  
  var sequenceNumber = 0L


  context.system.scheduler.schedule(100 milliseconds, 70 milliseconds, self, RetransmitUnacknowledged)
  
  /* TODO Behavior for the Replicator. */
  def receive: Receive = {
    case RetransmitUnacknowledged =>
      for (seq <- acks.keys) {
        val replicate = acks(seq)._2
        replica ! Snapshot(replicate.key, replicate.valueOption, seq)
      }

    case Replicate(key, valueOption, id) =>
      val replyTarget = sender
      acks += sequenceNumber -> (replyTarget, Replicate(key, valueOption, id))
      replica ! Snapshot(key, valueOption, sequenceNumber)
      sequenceNumber += 1

    case SnapshotAck(key, sequenceNum) =>
      val originalSender = acks(sequenceNum)._1
      originalSender ! Replicated(key, sequenceNum)
      acks -= sequenceNum

    case _ =>
  }

}
