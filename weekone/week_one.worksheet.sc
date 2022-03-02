abstract class JSON
object JSON:
    case class Seq(elems: List[JSON])       extends JSON
    case class Obj(obj: Map[String, JSON])  extends JSON
    case class Num(num: Double)             extends JSON
    case class Str(str: String)             extends JSON
    case class Bool(b: Boolean)             extends JSON
    case object Null                        extends JSON

/*
{
    "firstName" : "John",
    "lastName" : "Smith",
    "address": {
        "streetAddress": "21 2nd Street",
        "state": "NY",
        "postalCode": 10021
    },
    "phoneNumbers": [
        { "type": "home", "number": "212 555-1234" },
        { "type": "fax", "number": "646 555-4567" }
    ]
}
*/

val jsData = JSON.Obj(Map(
    "firstName" -> JSON.Str("John"),
    "lastName" -> JSON.Str("Smith"),
    "address" -> JSON.Obj(Map(
        "streetAddress" -> JSON.Str("21 2nd Street"),
        "state" -> JSON.Str("NY"),
        "postalCode" -> JSON.Num(10021)
    )),
    "phoneNumbers" -> JSON.Seq(List(
        JSON.Obj(Map(
            "type" -> JSON.Str("home"),
            "number" -> JSON.Str("212 555-1234")
        )),
        JSON.Obj(Map(
            "type" -> JSON.Str("fax"),
            "number" -> JSON.Str("646 555-4567")
        ))
    ))
))

val bwData = JSON.Obj(Map(
    "firstName" -> JSON.Str("ben"),
    "lastName" -> JSON.Str("Weather"),
    "address" -> JSON.Obj(Map(
        "streetAddress" -> JSON.Str("21 2nd Street"),
        "state" -> JSON.Str("NY"),
        "postalCode" -> JSON.Num(10020)
    )),
    "phoneNumbers" -> JSON.Seq(List(
        JSON.Obj(Map(
            "type" -> JSON.Str("home"),
            "number" -> JSON.Str("212 555-1235")
        )),
        JSON.Obj(Map(
            "type" -> JSON.Str("fax"),
            "number" -> JSON.Str("646 555-4566")
        ))
    ))
))

val peopleData = JSON.Obj(Map("people" -> JSON.Seq(List(jsData, bwData))))

def inQuotes(str: String): String = "\"" + str + "\""

def show(json: JSON): String = json match
    case JSON.Seq(elems) =>
        elems.map(show).mkString("[", ",", "]")
    case JSON.Obj(bindings) =>
        val assocs = bindings.map(
            (key, value) => s"${inQuotes(key)}: ${show(value)}")
        assocs.mkString("{", ",\n", "}")
    case JSON.Num(num) => num.toString
    case JSON.Str(str) => inQuotes(str)
    case JSON.Bool(b)  => b.toString
    case JSON.Null     => "null"

show(jsData)

/*
* Get, out of the dataset, all phone numbers starting with 212
*/

def bindings(json: JSON): List[(String, JSON)] = json match
    case JSON.Obj(bindings) => bindings.toList
    case _                  => Nil

for
    case ("phoneNumbers", JSON.Seq(numberInfos)) <- bindings(jsData)
    numberInfo <- numberInfos
    case ("number", JSON.Str(number)) <- bindings(numberInfo)
    if (number.startsWith("212"))
yield number

show(peopleData)

for
    case ("people", JSON.Seq(people)) <- bindings(peopleData)
    person <- people
    case ("phoneNumbers", JSON.Seq(numberInfos)) <- bindings(person)
    numberInfo <- numberInfos
    case ("number", JSON.Str(number)) <- bindings(numberInfo)
    if (number.startsWith("212"))
yield number

case class Book(title: String, authors: List[String])

/* sample extension method */
extension (b: Book)
    def printTitle = println(b.title)

Book("mybook", List("Jesse")).printTitle

val books: List[Book] = List(
    Book(title = "Structure and Interpretation of Computer Programs",
        authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(title = "Introduction to Functional Programming",
        authors = List("Bird, Richard", "Wadler, Phil")),
    Book(title = "Effective Java",
        authors = List("Bloch, Joshua")),
    Book(title = "Java Puzzlers",
        authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(title = "Programming in Scala",
        authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))

books
    .map(_.authors)
    .flatten
    .groupBy(el => el)
    .map((e, l) => (e, l.size))
    .filter(_._2 > 1)

val bookSet = books.toSet
for
    b1 <- bookSet
    b2 <- bookSet
    if b1 != b2
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
yield a1

/* find all books that are written by Block, Joshua */
for
    book <- books
    author <- book.authors
    if author.startsWith("Bird")
yield book.title

books.flatMap(
    book => book.authors.withFilter(
        _.startsWith("Bird")).map(a => book.title))

/* INTERESTING NOTE for-expressions only need map, flatMap and withFilter so you can use the syntax for your own types as well */

trait Generator[+T]:
    def generate(): T

val integers = new Generator[Int]:
    val rand = java.util.Random()
    def generate() = rand.nextInt()

val booleans = new Generator[Boolean]:
    def generate() = integers.generate() > 0

val pairs = new Generator[(Int, Int)]:
    def generate() = (integers.generate(), integers.generate())

integers.generate()
booleans.generate()
pairs.generate()

/* extend Generator with .map and .flatMap methods */
extension [T, S](g: Generator[T])
    def map(f: T => S) = new Generator[S]:
        def generate() = f(g.generate())

    def flatMap(f: T => Generator[S]) = new Generator[S]:
        def generate() = f(g.generate()).generate()


val newBoolean = for x <- integers yield x > 0
newBoolean.generate()

/* more general function to generate pairs using any type of generator */
def newPairs[T, U](t: Generator[T], u: Generator[U]) = new Generator[(T, U)]:
    def generate() = (t.generate(), u.generate())

newPairs(newBoolean, integers).generate()

def single[T](x: T): Generator[T] = new Generator[T]:
    def generate() = x

single(5).generate()

def range(lo: Int, hi: Int): Generator[Int] =
    for x <- integers yield lo + x.abs % (hi - lo)

range(5, 100).generate()

/* reuse the range generator */
def oneOf[T](xs: List[T]): Generator[T] =
    for idx <- range(0, xs.length) yield xs(idx)

oneOf(List(1, 2, 3)).generate()

/* a list generator using recursion (o_O Dafuq) */
def lists: Generator[List[Int]] =
    for
        numInRange <- range(0, 100)
        list <- if numInRange <= 20 then emptyList else nonEmptyList
    yield list

def emptyList = single(Nil)
def nonEmptyList =
    for
        head <- integers
        tail <- lists
    yield head :: tail

lists.generate()

enum Tree:
    case Inner(left: Tree, right: Tree)
    case Leaf(x: Int)

def trees: Generator[Tree] =
    for
        isLeaf <- booleans
        tree <- if isLeaf then leafNode else innerNode
    yield tree

def leafNode =
    for x <- integers yield Tree.Leaf(x)

def innerNode =
    for x <- trees; y <- trees yield Tree.Inner(x, y)

trees.generate()

/* INTERESTING: you can use the brackets after the function call to match the second () */
def test[T](g: Generator[T], numTimes: Int = 100)(test: T => Boolean): Unit =
    for i <- 0 until numTimes do
        val value = g.generate()
        assert(test(value), s"test failed for $value")
    println(s"passed $numTimes tests")

/* the brackets here provide the function (test: T => Boolean) in the test() function */
test(newPairs(lists, lists)) {
    (xs, ys) => (xs ++ ys).length >= xs.length
}

/* A monad is a parametric type M[T] with two operations, flatMap and unit, that have to satisfy some laws */
/*
extension [T, U](m: M[T])
    def flatMap(f: T => M[U]): M[U]

def unit[T](x: T): M[T]

Associativity:
    m.flatMap(f).flatMap(g) == m.flatMap(f(_).flatMap(g))

Left unit:
    unit(x).flatMap(f) == f(x)

right unit:
    m.flatMap(unit) == m

Monads are just Monoids in the category of Endofunctors:
    https://stackoverflow.com/questions/3870088/a-monad-is-just-a-monoid-in-the-category-of-endofunctors-whats-the-problem

*/

/* ????? Brruuuhhhh o_O */
extension [A, B, C](f: A => B)
    infix def andThen(g: B => C): A => C =
        x => g(f(x))

def funcOne(x: Int): Int = x + 1
def funcTwo(x: Int): Int = x + 2

List(1, 2, 3).map(funcOne andThen funcTwo)

/* exceptions java.util.{Try, Success, Failure} */
import scala.util.{Try, Success, Failure}

def convertNumSafe(s: String): Try[Int] =
    try Success(s.toInt)
    catch case ex: NumberFormatException => Failure(ex)

convertNumSafe("1")
convertNumSafe("bad")
convertNumSafe("bad").getOrElse(5)


