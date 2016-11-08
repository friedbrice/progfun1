package recfun

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal
  test("pascal: col=0,row=2") {
    assert(pascal(0,2) === 1)
  }

  test("pascal: col=1,row=2") {
    assert(pascal(1,2) === 2)
  }

  test("pascal: col=1,row=3") {
    assert(pascal(1,3) === 3)
  }

  test("pascal: col=3,row=5") {
    assert(pascal(3,5) === 10)
  }

  test("row 0") {
    assert(pascal(0,0) === 1)
  }

  test("row 1") {
    assert(pascal(0,1) === 1 && pascal(1,1) === 1)
  }

  test("row 2") {
    assert(0.to(2).map(n => pascal(n,2)) === List(1,2,1))
  }

  test("row 3") {
    assert(0.to(3).map(n => pascal(n,3)) === List(1,3,3,1))
  }

  test("row 4") {
    assert(0.to(4).map(n => pascal(n,4)) === List(1,4,6,4,1))
  }

}
