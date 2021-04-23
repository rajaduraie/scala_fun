/*
 *Santa moves around in 4 directions - ^<>v. Write a program to find out
 *the number of houses he has given gifts to at least once.
 */

import scala.collection.immutable.ListMap
object Santa extends App {
  def move ( ch: String, a: (Int,  Int) ) : ( Int, Int ) =
    ch match {
      case ">" => ( a._1 + 1, a._2 )
      case "<" => ( a._1 - 1, a._2 )
      case "^" => ( a._1, a._2 + 1 )
      case "v" => ( a._1, a._2 - 1 )
    }

  def getHouses( str: List[String], point: (Int, Int) , houses: List[(Int, Int)]): List[(Int, Int)] =
    str match {
      case Nil => houses
      case _ => {
        val p = move(str.head, point)
        getHouses(str.tail, p, p::houses)
      }
    }

  def countHouses(houses: List[(Int, Int)]): Int = 
    houses.toSet.size
  
  def countGiftsForEveryHouse(houses: List[(Int, Int)]): Map[String, Int] = {
    val groupPresents = houses.groupBy(a => "(" + a._1.toString + ", " + a._2.toString + ")")
    val presents =
      for ( 
           (a, b) <- groupPresents
         ) yield (a, b.size)
    presents
  }
    
  def printFormattedGifts(giftsForEveryHouse: Map[String, Int]): Unit = {
    giftsForEveryHouse.foreach{
      case ( x, y) => println(s"${x} has ${y} gift(s)")
    }
  }

  def sortedListGiftsForHouses(giftsForEveryHouse: Map[String, Int]): Map[String, Int] = {
    ListMap(giftsForEveryHouse.toSeq.sortWith(_._2 > _._2): _*)
  }

  val moves = "<>>><vvvv^>^>^<>>>>>>>>vvvvvvvv^^^<<<<<^^^^^^^^^^^<<<>>>><<<<>>>^^^^^^^^vvvvvv>>>>>>>>>>><<<<<<vvvvvvvvvvvvvvvvvvv>>>>>>>>>>>>>>>^^^^^^^^^^^^^^^^^^^^^^^^^^<><>>>><><<<<<>>>><vvvvv>>>>>>vvvv<<<<"
  val houses = getHouses(moves.split("").toList, (0, 0), Nil)
  println("Houses visiteed " + houses.toString)
  val numHouses = countHouses(houses)
  println("Number of houses visited atleast once : " + numHouses)
  val giftsForEveryHouse = countGiftsForEveryHouse(houses)
  printFormattedGifts(giftsForEveryHouse)
  println("The below is the sorted list ++++++++++++++++++++++++++")
  val sortedList = sortedListGiftsForHouses(giftsForEveryHouse)
  printFormattedGifts(sortedList)
}
