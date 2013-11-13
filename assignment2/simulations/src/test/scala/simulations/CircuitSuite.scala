package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("Test 8 output demux") {
    val in, c1, c2, c3, out1, out2, out3, out4, out5, out6, out7, out8 = new Wire
    val controlIn = List(c1, c2, c3)
    val wireOut = List(out1, out2, out3, out4, out5, out6, out7, out8)
    demux(in, controlIn, wireOut)

    def values = wireOut.map(_.getSignal)

    in.setSignal(false)
    c1.setSignal(false)
    c2.setSignal(false)
    c3.setSignal(false)
    run

    assert(values === List(false, false, false, false, false, false, false, false))

    in.setSignal(true)
    run
    assert(values === List(true, false, false, false, false, false, false, false), "Select 000")

    c1.setSignal(true)
    run
    assert(values === List(false, false, false, false, true, false, false, false), "Select 100")

    c2.setSignal(true)
    run
    assert(values === List(false, false, false, false, false, false, true, false), "Select 110")

    c1.setSignal(false)
    run
    assert(values === List(false, false, true, false, false, false, false, false), "Select 010")


    c1.setSignal(true)
    c2.setSignal(true)
    c3.setSignal(true)
    run
    assert(values === List(false, false, false, false, false, false, false, true), "Select 111")
  }

  test("Test 4 output demux") {
    val in, c1, c2, out1, out2, out3, out4 = new Wire
    val controlIn = List(c1, c2)
    val wireOut = List(out1, out2, out3, out4)
    demux(in, controlIn, wireOut)

    def values = wireOut.map(_.getSignal)

    in.setSignal(false)
    c1.setSignal(false)
    c2.setSignal(false)
    run

    assert(values === List(false, false, false, false))

    in.setSignal(true)
    run
    assert(values === List(true, false, false, false))

    c1.setSignal(true)
    run
    assert(values === List(false, false, true, false))

    c2.setSignal(true)
    run
    assert(values === List(false, false, false, true))

    c1.setSignal(false)
    run
    assert(values === List(false, true, false, false))
  }

  test("Test 2 output demux") {
    val in, control, out1, out2 = new Wire
    demux2(in, control, out1, out2)
    in.setSignal(false)
    control.setSignal(false)
    run

    assert(out1.getSignal === false)
    assert(out2.getSignal === false)

    in.setSignal(true)
    run

    assert(out1.getSignal === true, "First element should be selected")
    assert(out2.getSignal === false, "Second element should be deselected")

    control.setSignal(true)
    run

    assert(out1.getSignal === false, "First element should be deselected")
    assert(out2.getSignal === true, "Second element should be selected")

  }

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("orGate example") {
    val in1, in2, out2 = new Wire
    orGate(in1, in2, out2)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out2.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out2.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out2.getSignal === true, "or 3")
  }

  test("orGate 2 example") {
    val in1, in2, out2 = new Wire
    orGate2(in1, in2, out2)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out2.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out2.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out2.getSignal === true, "or 3")
  }

  test("Detect single element list") {

    def matcher(a: List[Int]) = {
      a match {
        case head :: Nil =>
          true
        case head :: tail =>
          false
      }
    }

    assert(matcher(List(1, 2)) === false, "Two element")
    assert(matcher(List(1)) === true, "Single element")
  }

  test("List splitter") {
    def halver(a: List[Int]) = {
      (a.slice(0, a.length / 2), a.slice(a.length / 2, a.length))
    }

    assert(halver(List(1, 2)) ===(List(1), List(2)))
    assert(halver(List(1, 2, 3, 4)) ===(List(1, 2), List(3, 4)))

  }


}
