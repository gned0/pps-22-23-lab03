package u03

import scala.annotation.tailrec
import u02.Modules.*
import u02.Modules.Person.Teacher

object Lists extends App :

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List
  object List:
    
    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    // Ex. 1a
    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match
      case (Cons(_, t), n) if n > 0 => drop(t, n-1)
      case _ => l

    // Ex. 1b
    def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
      case (Nil(), _) => right
      case (Cons(h, t), _) => Cons(h, append(t, right))

    // Ex. 1c
    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match
      case Nil() => Nil()
      case Cons(h, t) => append(f(h), flatMap(t)(f))

    // Ex. 1d
    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(_, _) => flatMap(l)(e => Cons(mapper(e), Nil()))
      case _ => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = flatMap(l1)(e => if (pred(e)) Cons(e, Nil()) else Nil())

    // Ex. 2
    @tailrec
    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, Nil()) => Some(h)
      case Cons(h1, Cons(h2, t)) if h1 < h2 => max(Cons(h2, t))
      case Cons(h1, Cons(h2, t)) if h1 > h2 => max(Cons(h1, t))
      case _ => None

    // Ex. 3
    def retrieveCourses(l: List[Person]): List[String] =
      map(l)(_ match
        case Teacher(_, c) => c
      )

    // Ex. 4
    @tailrec
    def foldLeft[A](l: List[A])(d: A)(f: (A, A) => A): A = l match
      case Cons(h, t) => foldLeft(t)(f(d, h))(f)
      case _ => d

    def foldRight[A](l: List[A])(d: A)(f: (A, A) => A): A = l match
      case Cons(h, t) => f(h, foldRight(t)(d)(f))
      case _ => d


