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

    // Task 1a, svolto con Pirazzoli
    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match
      case (Cons(_, t), 1) => t
      case (Cons(_, t), n) => drop(t, n-1)
      case _ => Nil()

    // Task 1b, svolto con Pirazzoli
    def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
      case (_, Nil()) => left
      case (Nil(), _) => right
      case (Cons(h, Nil()), right) => Cons(h, right)
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1, append(t1, Cons(h2, t2)))

    // Task 1c, svolto con Pirazzoli
    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match
      case Nil() => Nil()
      case Cons(h, t) => append(f(h), flatMap(t)(f))

    // Task 1d
    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(_, _) => flatMap(l)(e => Cons(mapper(e), Nil()))
      case _ => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(_, _) => flatMap(l1)(e => pred(e) match
        case true => Cons(e, Nil())
        case _ => Nil()
      )

    // Task 2
    @tailrec
    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, Nil()) => Some(h)
      case Cons(h1, Cons(h2, t)) if h1 < h2 => max(Cons(h2, t))
      case Cons(h1, Cons(h2, t)) if h1 > h2 => max(Cons(h1, t))
      case _ => None

    // Task 3
    def retrieveCourses(l: List[Person]): List[String] =
      flatMap(l)(_ match
        case Teacher(_, c) => Cons(c, Nil())
        case _ => Nil()
      )

    // Task 4
    @tailrec
    def foldLeft[A](l: List[A])(d: A)(f: (A, A) => A): A = l match
      case Cons(h, t) => foldLeft(t)(f(d, h))(f)
      case _ => d

    def foldRight[A](l: List[A])(d: A)(f: (A, A) => A): A = l match
      case Cons(h, t) => f(h, foldRight(t)(d)(f))
      case _ => d