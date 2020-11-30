package com.aahsk.homeworks.akka.actors

import akka.actor.{Actor, ActorRef, Props}

object BinaryTreeNode {
  private sealed trait Position

  private case object Left  extends Position
  private case object Right extends Position

  def props(elem: Int, initiallyRemoved: Boolean): Props =
    Props(new BinaryTreeNode(elem, initiallyRemoved))
}

final class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet.Operation._
  import BinaryTreeSet.OperationReply._

  private var subtrees = Map[Position, ActorRef]()
  private var removed  = initiallyRemoved

  override def receive: Receive = {
    case i: Insert   => doInsert(i)
    case r: Remove   => doRemove(r)
    case c: Contains => doContains(c)
  }

  private def doInsert(m: Insert): Unit = {
    if (m.elem == elem) {
      // Sender is requesting to insert self
      // Do nothing and finish operation
      m.requester ! OperationFinished(m.id)
    } else if (m.elem < elem) {
      // Sender is requesting to insert a smaller than self value i.e. Left
      subtrees.get(Left) match {
        // No one is Left => store insert as left
        case None =>
          subtrees = subtrees.updated(
            Left,
            context.actorOf(BinaryTreeNode.props(m.elem, initiallyRemoved = false))
          )
          m.requester ! OperationFinished(m.id)
        // There's a Left => forward this insert to it
        case Some(actorRef) => actorRef forward m
      }
    } else if (m.elem > elem) {
      // Sender is requesting to insert a larger than self value i.e. Right
      subtrees.get(Right) match {
        // No one is Right => store insert as left
        case None =>
          subtrees = subtrees.updated(
            Right,
            context.actorOf(BinaryTreeNode.props(m.elem, initiallyRemoved = false))
          )
          m.requester ! OperationFinished(m.id)
        // There's a Right => forward this insert to it
        case Some(actorRef) => actorRef forward m
      }
    }
  }

  private def doContains(m: Contains): Unit = {
    if (m.elem == elem) {
      // Sender is requesting whether self is present => yes
      m.requester ! ContainsResult(m.id, result = !removed)
    } else if (m.elem < elem) {
      // Sender is requesting whether element smaller than self is contained i.e. Left
      subtrees.get(Left) match {
        // No one is Left => the requested element isn't present
        case None => m.requester ! ContainsResult(m.id, result = false)
        // There's a Left => forward this contains request to it
        case Some(actorRef) => actorRef forward m
      }
    } else if (m.elem > elem) {
      // Sender is requesting whether element larger than self is contained i.e. Right
      subtrees.get(Right) match {
        // No one is Right => the requested element isn't present
        case None => m.requester ! ContainsResult(m.id, result = false)
        // There's a Right => forward this contains request to it
        case Some(actorRef) => actorRef forward m
      }
    }
  }

  private def doRemove(m: Remove): Unit = {
    if (m.elem == elem) {
      // Sender is requesting to remove self => comply
      removed = true
      m.requester ! OperationFinished(m.id)
    } else if (m.elem < elem) {
      // Sender is requesting to insert a smaller than self value i.e. Left
      subtrees.get(Left) match {
        // No one is Left => do nothing and finish
        case None => m.requester ! OperationFinished(m.id)
        // There's a Left => forward this removal to it
        case Some(actorRef) => actorRef forward m
      }
    } else if (m.elem > elem) {
      // Sender is requesting to insert a larger than self value i.e. Right
      subtrees.get(Right) match {
        // No one is Right => do nothing and finish
        case None => m.requester ! OperationFinished(m.id)
        // There's a Right => forward this removal to it
        case Some(actorRef) => actorRef forward m
      }
    }
  }
}
