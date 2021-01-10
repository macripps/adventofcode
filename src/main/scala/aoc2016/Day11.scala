package aoc2016

import aoc.{Day, Search}

class Day11 extends Day(2016, 11) {

  import Day11._

  override def part1: String = {
    val state = toState(input)
    var result = Search.breadthFirst[State](state, transition, isGoal)
    var length = 0
    while (result.parent.isDefined) {
      length = length + 1
      result = result.parent.get
    }
    length.toString
  }

  val example: State = State(
    elevator = 1,
    firstFloor = Set(Microchip("H"), Microchip("L")),
    secondFloor = Set(Generator("H")),
    thirdFloor = Set(Generator("L")),
    fourthFloor = Set(),
  )(None)

  def transition(state: State): Set[State] = {
    val newStates = state.elevator match {
      case 1 =>
        state.firstFloor.map { i =>
          State(elevator = 2,
            firstFloor = state.firstFloor - i,
            secondFloor = state.secondFloor + i,
            thirdFloor = state.thirdFloor,
            fourthFloor = state.fourthFloor,
          )(Some(state))
        } ++ state.firstFloor.toSeq.combinations(2).map { is =>
          State(
            elevator = 2,
            firstFloor = state.firstFloor.removedAll(is),
            secondFloor = state.secondFloor ++ is,
            thirdFloor = state.thirdFloor,
            fourthFloor = state.fourthFloor,
          )(Some(state))
        }
      case 2 =>
        state.secondFloor.flatMap { i =>
          Set(
            State(elevator = 1,
              firstFloor = state.firstFloor + i,
              secondFloor = state.secondFloor - i,
              thirdFloor = state.thirdFloor,
              fourthFloor = state.fourthFloor,
            )(Some(state)),
            State(elevator = 3,
              firstFloor = state.firstFloor,
              secondFloor = state.secondFloor - i,
              thirdFloor = state.thirdFloor + i,
              fourthFloor = state.fourthFloor,
            )(Some(state))
          )
        } ++ state.secondFloor.toSeq.combinations(2).flatMap { is =>
          Set(
            State(elevator = 1,
              firstFloor = state.firstFloor ++ is,
              secondFloor = state.secondFloor.removedAll(is),
              thirdFloor = state.thirdFloor,
              fourthFloor = state.fourthFloor,
            )(Some(state)),
            State(elevator = 3,
              firstFloor = state.firstFloor,
              secondFloor = state.secondFloor.removedAll(is),
              thirdFloor = state.thirdFloor ++ is,
              fourthFloor = state.fourthFloor,
            )(Some(state)),
          )
        }
      case 3 =>
        state.thirdFloor.flatMap { i =>
          Set(
            State(elevator = 2,
              firstFloor = state.firstFloor,
              secondFloor = state.secondFloor + i,
              thirdFloor = state.thirdFloor - i,
              fourthFloor = state.fourthFloor,
            )(Some(state)),
            State(elevator = 4,
              firstFloor = state.firstFloor,
              secondFloor = state.secondFloor,
              thirdFloor = state.thirdFloor - i,
              fourthFloor = state.fourthFloor + i,
            )(Some(state))
          )
        } ++ state.thirdFloor.toSeq.combinations(2).flatMap { is =>
          Set(
            State(elevator = 2,
              firstFloor = state.firstFloor,
              secondFloor = state.secondFloor ++ is,
              thirdFloor = state.thirdFloor.removedAll(is),
              fourthFloor = state.fourthFloor,
            )(Some(state)),
            State(elevator = 4,
              firstFloor = state.firstFloor,
              secondFloor = state.secondFloor,
              thirdFloor = state.thirdFloor.removedAll(is),
              fourthFloor = state.fourthFloor ++ is,
            )(Some(state)),
          )
        }
      case 4 =>
        state.fourthFloor.map { i =>
          State(elevator = 3,
            firstFloor = state.firstFloor,
            secondFloor = state.secondFloor,
            thirdFloor = state.thirdFloor + i,
            fourthFloor = state.fourthFloor - i,
          )(Some(state))
        } ++ state.fourthFloor.toSeq.combinations(2).map { is =>
          State(
            elevator = 3,
            firstFloor = state.firstFloor,
            secondFloor = state.secondFloor,
            thirdFloor = state.thirdFloor ++ is,
            fourthFloor = state.fourthFloor.removedAll(is),
          )(Some(state))
        }
      case _ => Set[State]()
    }
    newStates.filter(isValid)
  }

  def isValid(state: State): Boolean = {
    Seq(state.firstFloor, state.secondFloor, state.thirdFloor, state.fourthFloor).forall { floor =>
      var chipZapped = false
      var generators = floor.filter(_.isInstanceOf[Generator])
      floor.foreach {
        case Microchip(m) => if (!generators.contains(Generator(m)) && generators.nonEmpty) {
          chipZapped = true
        }
        case _ =>
      }
      !chipZapped
    }
  }

  def isGoal(state: State): Boolean = {
    state.elevator == 4 && state.firstFloor.isEmpty && state.secondFloor.isEmpty && state.thirdFloor.isEmpty
  }

  def toState(input: Array[String]): State = {
    State(elevator = 1,
      firstFloor = Set(
        Generator("polonium"),
        Generator("thulium"),
        Microchip("thulium"),
        Generator("promethium"),
        Generator("ruthenium"),
        Microchip("ruthenium"),
        Generator("cobalt"),
        Microchip("cobalt"),
      ),
      secondFloor = Set(
        Microchip("polonium"),
        Microchip("promethium"),
      ),
      thirdFloor = Set(),
      fourthFloor = Set(),
    )(None)
  }

  def toState2(input: Array[String]): State = {
    State(elevator = 1,
      firstFloor = Set(
        Generator("polonium"),
        Generator("thulium"),
        Microchip("thulium"),
        Generator("promethium"),
        Generator("ruthenium"),
        Microchip("ruthenium"),
        Generator("cobalt"),
        Microchip("cobalt"),
        Generator("elerium"),
        Microchip("elerium"),
        Generator("dilithium"),
        Microchip("dilithium"),
      ),
      secondFloor = Set(
        Microchip("polonium"),
        Microchip("promethium"),
      ),
      thirdFloor = Set(),
      fourthFloor = Set(),
    )(None)
  }

  override def part2: String = {
    val state = toState2(input)
    var result = Search.breadthFirst[State](state, transition, isGoal)
    var length = 0
    while (result.parent.isDefined) {
      length = length + 1
      result = result.parent.get
    }
    length.toString
  }
}

object Day11 {
  def apply() = new Day11()

  trait Item

  case class Microchip(t: String) extends Item

  case class Generator(t: String) extends Item

  case class State(elevator: Int, firstFloor: Set[Item], secondFloor: Set[Item], thirdFloor: Set[Item], fourthFloor: Set[Item])(val parent: Option[State]) {
    override def toString: String = {
      Seq(
        "F4 " + (if (elevator == 4) "E" else ".") + " " + fourthFloor.map {
          case Microchip(m) => "M" + m
          case Generator(m) => "G" + m
        }.mkString(" "),
        "F3 " + (if (elevator == 3) "E" else ".") + " " + thirdFloor.map {
          case Microchip(m) => "M" + m
          case Generator(m) => "G" + m
        }.mkString(" "),
        "F2 " + (if (elevator == 2) "E" else ".") + " " + secondFloor.map {
          case Microchip(m) => "M" + m
          case Generator(m) => "G" + m
        }.mkString(" "),
        "F1 " + (if (elevator == 1) "E" else ".") + " " + firstFloor.map {
          case Microchip(m) => "M" + m
          case Generator(m) => "G" + m
        }.mkString(" "),
      ).mkString("\n")
    }
  }

}
