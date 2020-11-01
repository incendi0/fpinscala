package fpinscala.datastructures

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ListSuite extends AnyFunSuite {
  test("apply saves the hand work") {
    val oneTwoThree = Cons(1, Cons(2, Cons(3, Nil)))
    assert(oneTwoThree == List.apply(1, 2, 3))
  }
}