package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val moveTime: Int = 5
    val prevalenceRate: Double = 0.01
    val incubationTime: Int = 6
    val timeToDeath: Int = 8
    val transmissionRate: Double = 0.4
    val mortalityRate: Double = 0.25
    val immunityDelay: Int = 2
    val healthyDelay: Int = 2
  }

  import SimConfig._

  case class Position(x: Int, y: Int) {
    def bound(rows: Int, cols: Int) = Position((x + rows) % rows, (y + cols) % cols)
  }

  def getNeighbors(pos: Position): List[Position] = {
    List(Position(pos.x + 1, pos.y),
      Position(pos.x, pos.y + 1),
      Position(pos.x - 1, pos.y),
      Position(pos.x, pos.y - 1)).map(_.bound(roomRows, roomColumns))
  }

  val persons: List[Person] = (for (i <- 0 until population) yield new Person(i)).toList
  for (i <- 0 until (prevalenceRate * population).toInt) {
    persons(i).infected = true
  }
  for (p <- persons) {
    p.init()
    p.moveAction()
  }

  def roomMeetsStatus(f: Person => Boolean)(pos: Position): Boolean = {
    for (p <- persons if f(p) && p.position == pos) {
      return true
    }
    false
  }

  def roomVisiblyInfected(pos: Position): Boolean = roomMeetsStatus({
    _.visiblyInfected
  })(pos)

  def roomInfected(pos: Position): Boolean = roomMeetsStatus({
    _.infected
  })(pos)

  def canMoveTo(pos: Position) = !roomVisiblyInfected(pos)

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def visiblyInfected = sick || dead

    def incubating = infected && (!sick) && (!immune) && (!dead)

    def canGetSick = (!infected) && (!immune)

    def position = Position(row, col)

    def makeHealthy() {
      immune = false
      infected = false
    }

    def makeImmune() {
      if (!dead) {
        immune = true
        sick = false
        afterDelay(healthyDelay)(makeHealthy)
      }
    }

    def maybeDie() {
      if (random < mortalityRate) {
        dead = true
      } else {
        afterDelay(immunityDelay)(makeImmune)
      }
    }

    def makeSick() {
      sick = true
      afterDelay(timeToDeath)(maybeDie)
    }

    def sickenify() {
      if (roomInfected(position) && canGetSick) {
        if (random < transmissionRate) {
          infected = true
          afterDelay(incubationTime)(makeSick)
        }
      }
    }

    def doMove() {
      if (!dead) {
        val neighbors = getNeighbors(position).filter(canMoveTo)
        if (neighbors.length > 0) {
          val i = randomBelow(neighbors.length)
          row = neighbors(i).x
          col = neighbors(i).y
        }
        sickenify()
        moveAction()
      }
    }

    def moveAction() {
      if (!dead) {
        val moveDelay = randomBelow(incubationTime)
        afterDelay(moveDelay)(doMove)
      }
    }

    def init() {
      if (incubating) {
        afterDelay(incubationTime)(makeSick)
      }
    }
  }

}
