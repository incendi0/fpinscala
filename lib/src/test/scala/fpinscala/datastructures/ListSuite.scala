package fpinscala.datastructures

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.have
import org.scalatest.matchers.should.Matchers.{convertToAnyShouldWrapper, the}
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ListSuite extends AnyFunSuite {
  test("apply saves the hand work") {
    val oneTwoThree = Cons(1, Cons(2, Cons(3, Nil)))
    assert(oneTwoThree == List.apply(1, 2, 3))
  }

  test("tail abandons the first element") {
    val oneTwoThree = Cons(1, Cons(2, Cons(3, Nil)))
    assert(List.tail(oneTwoThree) == Cons(2, Cons(3, Nil)))
  }

  the[RuntimeException] thrownBy {
    List.tail(Nil)
  } should have message "tail on empty list"

  test("setHead replace the first element") {
    val oneTwoThree = Cons(1, Cons(2, Cons(3, Nil)))
    assert(List.setHead(0, oneTwoThree) == Cons(0, Cons(2, Cons(3, Nil))))
  }

  the[RuntimeException] thrownBy {
    List.setHead(1, Nil)
  } should have message "setHead on empty list"
}