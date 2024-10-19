package u03

import org.junit.*
import org.junit.Assert.*
import u03.Streams.Stream.*
import u03.Lists.List

class StreamTest:

  @Test def testConstant(): Unit =
    val constStream = take(constant(5))(3)
    assertEquals(List.Cons(5, List.Cons(5, List.Cons(5, List.Nil()))), toList(constStream))

  @Test def testFibs(): Unit =
    val fibStream = take(fibs())(7)
    assertEquals(List.Cons(0, List.Cons(1, List.Cons(1, List.Cons(2, List.Cons(3, List.Cons(5, List.Cons(8, List.Nil()))))))), toList(fibStream))

  @Test def testDrop(): Unit =
    val stream = take(constant(5))(3)
    assertEquals(List.Cons(5, List.Cons(5, List.Cons(5, List.Nil()))), toList(stream))

