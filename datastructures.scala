package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](xs: A*): List[A] = 
    if (xs.isEmpty) Nil
    else Cons(xs.head, apply(xs.tail: _*))

  // 3.11 - sum 

  def sum(xs: List[Int]): Int = 
   xs match {
     case Nil => 0
     case Cons(a, as) => a + sum(as)
   }

  // 3.11 - product 

  def product(xs: List[Int]): Int = 
    xs match {
      case Cons(0, as) => 0
      case Cons(a, Nil) => a
      case Cons(a, as) => a * product(as)
      case Nil => 0
    }

  def head[A](xs: List[A]): Option[A] =
    xs match {
      case Cons(h, t) => Some(h)
      case Nil => None
    }

  def length[A](xs: List[A]): Int = {
    def loop(as: List[A], n: Int): Int =
      as match {
        case Cons(h, t) => loop(t, n + 1 )
        case Nil => n
      }
    loop(xs, 0)
  }

  //3.2 - Implement function to remove the first element
  
  def tail[A](xs: List[A]): List[A] = 
    xs match {
      case Cons(a, as) => as
      case Nil => Nil
    }

  //3.3 - Implement function to replace first element
  
  def setHead[A](a: A, as:List[A]): List[A] =
    as match {
      case Cons(x, xs) => Cons(a, xs)
      case Nil => Cons(a, Nil)
    }

  //3.4 - Remove first n elements

  def drop[A](i: Int, xs: List[A]): List[A] = {
    def loop(n: Int, xs: List[A]): List[A] = 
      xs match {
        case Nil => Nil
        case Cons(a,as)  => {
          if ( n == i ) xs
          else loop(n+1, as)
        }
      }
    loop(0, xs)
  }

  //3.5 - drop elements till a condition is satisfied

  def dropWhile[A](xs: List[A], f: A => Boolean): List[A] =
    xs match {
      case Cons(a, as) => {
        if (f(a)) dropWhile(as, f)
        else as
      }
      case Nil => Nil
    }

  // 3.15 - Add 2 lists

  def addLists[A](as: List[A], bs: List[A]): List[A] =
    as match {
      case Nil => bs
      case Cons(h, t) => Cons(h, addLists(t, bs))
    }

  //3.6 - function returning all but last element

  def init[A](xs: List[A]): List[A] = {
    def loop(as: List[A], bs:List[A]): List[A] = 
      as match {
        case Nil => Nil
        case Cons(h, Nil) => bs
        case Cons(h, t) => loop(t, Cons(h, bs))
      }
    reverse(loop(xs, Nil))
  }

  //3.12 - reverse a list

  def reverse[A](xs: List[A]): List[A] = {
    def change(as: List[A], bs: List[A]): List[A] =
      as match {
        case Nil => bs
        case Cons(h, t) => change(t, Cons(h, bs))
      }
    change(xs, Nil)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  def foldLeft[A,B](xs: List[A], z: B)(f: (B, A) => B): B =
    xs match {
      case Nil => z 
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  //3.16 - Add number to list
  
  def addNumToList( xs: List[Int], n: Int ): List[Int] = {
    def addNum(as: List[Int], bs: List[Int]): List[Int] =
      as match {
        case Cons(h, t) => addNum(t, Cons(h+n, bs))
        case Nil => bs
      }
    addNum(xs, Nil)
  }

  //3.17- Make Ints double  

  def mkDouble(xs: List[Int]): List[Double] = {
    def convDouble(as: List[Int], bs: List[Double]): List[Double] =
      as match {
        case Cons(h, t) => convDouble(t, Cons(h.toDouble, bs))
        case Nil => bs
      }
    convDouble(xs, Nil)
  }

  //3.18 - Write map function

  def map[A,B](xs: List[A])(f: A => B): List[B] = {
    def makeFunc(as: List[A], bs: List[B]): List[B] =
      as match {
        case Cons(h, t) => makeFunc(t, Cons(f(h), bs))
        case Nil => bs
      }
    makeFunc(xs, Nil)
  }

  // 3.19 - Filter

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    def filList(xs: List[A], ys: List[A]): List[A] =
      xs match {
        case Cons(h, t) => {
          if (f(h)) filList(t, Cons(h, ys))
          else filList(t, ys)
        }
        case Nil => ys
      }
    reverse(filList(as, Nil))
  }

  def main(args: Array[String]): Unit = {
    val lst = List(1,3,5,7,9,2,4,6,8)
    val ls = List(10,11,12,13,14)
    println("Sum of list elements " + sum(lst))
    assert(sum(lst) == 45) //println("Sum of list elements " + sum(lst))
    println("Product of list elements "+ product(lst))
    println("Tail of list elements "+ tail(lst))
    println("setHead to 10 for list " + setHead(10, lst))
    println("drop 2 elements from list " + drop(2, lst))
    println("dropWhile element is less than 5 " + dropWhile(lst, (x: Int) => x < 5))
    println("Add List ls with lst " + addLists(lst, ls))
    println("Init List is " + init(lst))
    println("Reverse List is " + reverse(lst))
    println("foldRight List add is " + foldRight(lst, 0)( (x: Int, y: Int) => x + y))
    println("foldRight List multiply is " + foldRight(lst, 1)( (x: Int, y: Int) => x * y))
    println("FoldRight calculate number of elements " + foldRight(lst, 0)( (x, y) => y + 1))
    println("FoldLeft calculate number of elements " + foldLeft(lst, 0)( (x, y) => y + 1))
    println("FoldLeft calculate sum of elements " + foldLeft(lst, 0)( (x, y) => x + y))
    println("FoldLeft calculate product of elements " + foldLeft(lst, 1.0)( (x, y) => x * y))
    println("FoldLeft same list " + reverse(foldLeft(lst, Nil: List[Int])((x, y) => Cons(y, x))))
    // 3.13 - append with fold left
    println("FoldLeft append " + reverse(foldLeft(lst, List(15))( (x, y) => Cons(y, x))))
    println("FoldLeft calculate product of elements " + foldLeft(lst, 1.0)( (x, y) => x * y))

  }
  
}


