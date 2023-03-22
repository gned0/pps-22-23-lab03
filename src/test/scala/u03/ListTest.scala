package u03

import org.junit.*
import org.junit.Assert.*
import u02.Modules.Person.*
import Lists.*


import scala.Option

class ListTest:
  import List.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))
  val foldList: List[Int] = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))

  @Test def testSum(): Unit =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_+1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_+""))

  @Test def testFilter(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_>=20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_!=20))
  
  @Test def testDrop(): Unit =
    assertEquals(Cons (20 , Cons (30 , Nil ())), drop(l, 1))
    assertEquals(Cons (30 , Nil ()), drop(l, 2))
    assertEquals(Nil(), drop(l, 5))

  @Test def testAppend(): Unit =
    val tail = Cons (40 , Nil () )
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(l, tail))

  @Test def testFlatmap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Cons (11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))),
      flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test def testMax(): Unit =
    assertEquals(Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))
    assertEquals(Some(30), max(Cons(10, Cons(20, Cons(30, Nil())))))
    assertEquals(None, max(Nil()))

  @Test def testCourses(): Unit =
    val teachers = Cons(Teacher("Viroli", "PPS"), Cons(Teacher("Ricci", "PCD"), Nil()))
    assertEquals(Cons("PPS", Cons("PCD", Nil())), retrieveCourses(teachers))

  @Test def testLeftFold(): Unit =
    assertEquals(-16, foldLeft(foldList)(0)(_ - _))

  @Test def testRightFold(): Unit =
    assertEquals(-8, foldRight(foldList)(0)(_ - _))  


