package sandbox

import json.Json
import json.JsonWriter
import json.JsonSyntax._
import json.JsonWriterInstances._
import printable.Printable
import printable.PrintableSyntax._

import cats.Show
import cats.syntax.show._
import cats.syntax.option._
import java.util.Date
import cats.kernel.Eq
import cats.syntax.eq._

trait Monad[F[_]] {
  def pure[A](a: A): F[A] 
  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
  def map[A, B](value: F[A])(func: A => B): F[B] = {
    val xxx = flatMap(value)(x => pure(func(x)))
    xxx
  }
}

object Main extends App {
  // println("Hello " |+| "Cats!")
  val dave = Person("Dave", "dave@example.com")
  println(dave)

  println(Json.toJson(dave))
  println(dave.toJson)
  val whatAmI = implicitly[JsonWriter[String]]
  println(whatAmI)
  val optionJson = Option("I'm an option").toJson
  // val optionJson = Some("I'm an option").toJson
  println(optionJson)

  val cat = Cat("Kasmi", 1, "striped")
  Printable.print(cat)
  cat.print

  val showInt = Show.apply[Int]
  val showStr = Show.apply[String]
  println(showInt.show(7))
  println(showStr.show("xxx"))

  implicit val dateShow0: Show[Date] =
    Show.show(x => s"${x.getTime}ms since the epoch.")
  println(new Date().show)
  println(cat.show)

  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  println(cat1 === cat1)
  println(Eq.eqv(cat1, cat2))
  println(optionCat1 =!= optionCat2)
  println(optionCat1 === Option(cat1))

  println(1.some === none[Int])

  // pure(a).flatMap(func) == func(a)
  // val optionMonad = Monad[Option].pure(1).flatMap()

}
