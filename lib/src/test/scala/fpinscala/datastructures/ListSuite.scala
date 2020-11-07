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

    the[RuntimeException] thrownBy {
      List.tail(Nil)
    } should have message "tail on empty list"
  }

  test("setHead replace the first element") {
    val oneTwoThree = Cons(1, Cons(2, Cons(3, Nil)))
    assert(List.setHead(0, oneTwoThree) == Cons(0, Cons(2, Cons(3, Nil))))

    the[RuntimeException] thrownBy {
      List.setHead(1, Nil)
    } should have message "setHead on empty list"
  }

  test("drop removes the first n elements") {
    val oneTwoThree = Cons(1, Cons(2, Cons(3, Nil)))
    assert(List.drop(oneTwoThree, -1) == oneTwoThree)
    assert(List.drop(oneTwoThree, 0) == oneTwoThree)
    assert(List.drop(oneTwoThree, 1) == Cons(2, Cons(3, Nil)))
    assert(List.drop(oneTwoThree, 2) == Cons(3, Nil))
    assert(List.drop(oneTwoThree, 3) == Nil)
    assert(List.drop(oneTwoThree, 4) == Nil)
  }

  test("dropWhile removes the first suitable elements") {
    def odd(n: Int): Boolean = n % 2 == 1
    def even(n: Int): Boolean = n % 2 == 0
    val oneTwoThree = Cons(1, Cons(2, Cons(3, Nil)))
    assert(List.dropWhile(oneTwoThree)(odd) == Cons(2, Cons(3, Nil)))
    assert(List.dropWhile(oneTwoThree)(even) == Cons(1, Cons(2, Cons(3, Nil))))
  }

  test("append concatenates the two lists") {
    val n1 = Nil
    val n2 = Nil
    val oneTwoThree = Cons(1, Cons(2, Cons(3, Nil)))
    val fourFive = Cons(4, Cons(5, Nil))
    val oneToFive = List.apply(1, 2, 3, 4, 5)
    val rotate = List.apply(4, 5, 1, 2, 3)
    assert(List.append(n1, n2) == Nil)
    assert(List.append(n1, oneTwoThree) == oneTwoThree)
    assert(List.append(oneTwoThree, n1) == oneTwoThree)
    assert(List.append(oneTwoThree, fourFive) == oneToFive)
    assert(List.append(fourFive, oneTwoThree) == rotate)
  }

  test("init get the elements but the last one") {
    val one = List.apply(1)
    assert(List.init(one) == Nil)
    val oneTwoThree = List.apply(1, 2, 3)
    assert(List.init(oneTwoThree) == List.apply(1, 2))

    the[RuntimeException] thrownBy {
      val e = List.init(Nil)
    } should have message "init on empty list"
  }

}