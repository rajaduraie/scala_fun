package fpinscala.datastructures
import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def sum(ints: List[Int]): Int = {
    ints match {
      case Cons(x, xs) => x + sum(xs)
      case Nil => 0
    }
  }
  def product(ds: List[Double]): Double = {
    ds match {
      case Cons(0-.0, _) => 0
      case Cons(d, ds) => d + product(ds)
      case Nil => 1.0
    }
  }
  def tail[A](ds: List[A]): List[A] = {
    ds match {
      case Cons(a, as) => as
      case Nil => Nil
    }
  }
  def setHead[A](ds: List[A], newHead: A): List[A] = {
    ds match {
      case Cons(a, as) => Cons(newHead, as)
      case Nil => Nil
    }
  }
  @tailrec
  def drop[A](ds: List[A], n: Int): List[A] = {
    if (ds == Nil) Nil
    else {
      n match {
        case 0 => ds
        case _ => drop(tail(ds), n - 1)
      }
    }
  }
  @tailrec
  def dropWhile[A](ds: List[A])(f: A => Boolean): List[A] = {
    ds match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => ds
    }
  }

  def append[A](ds: List[A], fs: List[A]): List[A] = {
    ds match {
      case Nil => fs
      case Cons(a, as) => Cons(a, append(as, fs))
    }
  }
  def init[A](ds: List[A]): List[A] = {
    @tailrec
    def loop(as: List[A], fs: List[A]): List[A] = {
      as match {
        case Cons(h, Nil) => fs
        case Cons(h, t) => loop(t, Cons(h, fs))
      }
    }
    ds match {
      case Nil => Nil
      case _ => loop(ds, Nil)
      }
    }
  def foldRight[A,B](ds: List[A], z: B)(f: (A, B) => B): B = {
        ds match {
          case Cons(x, xs) => f(x, foldRight(xs, z)(f))
          case Nil => z
        }
  }
  def sumFoldRight(ds: List[Int]): Int = {
    foldRight(ds, 1)(_+_)
  }
  def lenFoldRight[A, B](ds: List[A], l: Int): Int = {
    foldRight(ds, 0)((_, acc) => acc + 1)
  }
  @tailrec
  def foldLeft[A,B](ds: List[A], z: B)(f: (B,A) => B): B = {
    ds match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
    }
  }
  def sumFoldLeft(ds: List[Double]): Double = {
    foldLeft(ds, 0.0)(_ + _)
  }
  def productFoldLeft(ds: List[Double]): Double = {
    foldLeft(ds, 1.0)(_*_)
  }
  def lenFoldLeft[A](ds: List[A]): Int = {
    foldLeft(ds, 0)((acc, _) => acc + 1)
  }
  def reverse[A](ds: List[A]): List[A] = {
    foldLeft(ds, List[A]())((acc, h) => Cons(h, acc))
  }
  def appendFoldRight[A](element: A, ds: List[A]): List[A] = {
    foldRight(ds, List(element))(Cons(_,_))
  }
  def concat[A](ds: List[List[A]]): List[A] = {
    foldLeft(ds, Nil: List[A])(append)
  }
  def incrementEach(ds: List[Int]): List[Int] = {
    foldLeft(ds, Nil: List[Int])((acc, i) => Cons(i+1, acc))
  }
  def convertString(ds: List[Double]): List[String] = {
    foldLeft(ds, Nil: List[String])((lst, i) => Cons(i.toString, lst))
  }
  def map[A,B](ds: List[A])(f: A => B): List[B] = {
    foldLeft(ds, Nil: List[B])((lst, i) => Cons(f(i), lst))
  }
  def filter[A](ds: List[A], f: A => Boolean ): List[A] = {
    @tailrec
    def loop(lst: List[A], newlst: List[A]): List[A] = {
      lst match {
        case Cons(x,xs) if f(x) => loop(xs, Cons(x, newlst))
        case Nil => newlst
        case _ => loop(tail(lst), newlst)
      }
    }
    loop(ds, Nil)
  }
  def flatMap[A,B](ds: List[A], f: A => List[B]): List[B] = {
    concat(map(ds)(f))
  }
  def head[A](xs: List[A]): A = {
    xs match {
      case Cons(h, t) => h
      case _ => throw new NotImplementedError()
    }
  }
  def zip(ds: List[Int], es: List[Int]): List[Int] = {
    @tailrec
    def loop(as: List[Int], bs: List[Int], newLst: List[Int]): List[Int] = {
      as match {
        case Cons(h, t) => loop(t, tail(bs), Cons(h + head(bs), newLst))
        case Nil => newLst
      }
    }
    if (lenFoldLeft(ds) != lenFoldLeft(es) ) Nil
    else loop(ds, es, Nil)
  }
  def zipWith[A,B, C](ds: List[A], es: List[B])(f: (A,B) => C): List[C] = {
    @tailrec
    def loop(as: List[A], bs: List[B], newLst: List[C]): List[C] = {
      as match {
        case Cons(h, t) => loop(t, tail(bs), Cons(f(h, head(bs)), newLst))
        case Nil => newLst
      }
    }
    if (lenFoldLeft(ds) != lenFoldLeft(es) ) Nil
    else loop(ds, es, Nil)
  }
  def hasSubsequence[A](ds: List[A], sub: List[A]): Boolean = {
    @tailrec
    def loop[A](a: List[A], b: List[A]): Boolean = {
      b match {
        case Nil => true
        case _ =>
          a match {
            case Nil => false
            case _ =>
              if (head(a) != head(b)) false
              else loop(tail(a), tail(b))
          }
      }
    }
    @tailrec
    def loop2[A](c: List[A], d: List[A]): Boolean = {
      c match {
        case Nil => false
        case _ =>
          if (loop(c, d)) true
          else loop2(tail(c), d)
      }
    }
    loop2(ds, sub)
  }
}
