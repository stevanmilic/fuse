package fuse

import munit.*
import scala.concurrent.duration.Duration

class CompilerTests extends munit.FunSuite {
  import CompilerTests.*
  override val munitTimeout = Duration(10, "m")

  test("check integer addition") {
    fuse("""
fun main() -> i32
    2 + 2
        """)
  }
  test("check float addition") {
    fuse("""
fun main() -> f32
    2.0 + 2.2
        """)
  }
  test("check string addition") {
    fuse("""
fun main() -> i32
    let s = "1" + "2"
    0
        """)
  }
  test("check generic option") {
    fuse("""
type Option[A]:
    None
    Some(A)

impl Option[A]:
    fun is_some() -> bool
        match this:
            Some(v) => true
            _ => false

    fun is_none() -> bool
        match this:
            Some(v) => false
            _ => true

    fun map[B](f: A -> B) -> Option[B]
        match this:
            Some(v) => Some(f(v))
            _ => None

    fun get_or_else(default: A) -> A
        match this:
            Some(a) => a
            None => default

    fun flat_map[B](f: A -> Option[B]) -> Option[B]
        let v = this.map(f)
        v.get_or_else(None)

    fun filter(f: A -> bool) -> Option[A]
        match this:
            Some(a) => {
                match f(a):
                    true => this
                    _ => None
            }
            _ => None

fun option_str(v: i32) -> Option[str]
    let s = Some(v)
    s.map(a => int_to_str(a))

fun main() -> i32
    let l = option_str(5)
    match l:
        Some(v) => 0
        None => 1
        """)
  }
  test("check generic list") {
    fuse("""
type List[A]:
    Cons(h: A, t: List[A])
    Nil

fun foldRight[A, B](as: List[A], z: B, f: (A, B) -> B) -> B
    match as:
        Cons(x, xs) => f(x, foldRight(xs, z, f))
        Nil => z

impl List[A]:
    fun map[B](f: A -> B) -> List[B]
        foldRight(this, Nil[B](), (h, t) => Cons(f(h), t))

    fun map_2[B](f: A -> B) -> List[B]
        let iter = (acc, l) => {
            match l:
                Cons(h, t) => Cons(f(h), iter(acc, t))
                Nil => acc
        }
        iter(Nil[B](), this)

fun main() -> Unit
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.map_2(v => v + 1)
    ()
        """)
  }
  test("check lambda calc") {
    fuse("""
type List[T]:
    Cons(h: T, t: List[T])
    Nil

type Option[T]:
    None
    Some(T)

impl Option[T]:
    fun is_some() -> bool
        match this:
            Some(v) => true
            _ => false

    fun is_none() -> bool
        match this:
            Some(v) => false
            _ => true

type Tuple[A, B](A, B)

type Type:
    TypeVar(index: i32, length: i32)
    TypeId(i: str)
    TypeArrow(t1: Type, t2: Type)
    TypeRecord(fields: List[Tuple[str, Type]])
    TypeVariant(values: List[Tuple[str, Type]])
    TypeUnit
    TypeBool
    TypeInt
    TypeString
    TypeFloat

type Term:
    TermTrue
    TermFalse
    TermInt(i: i32)
    TermFloat(f: f32)
    TermString(s: str)
    TermUnit

fun println(s: str) -> Unit
    print(s)
    print("\n")
    ()

fun type_of(t: Term) -> Option[Type]
    match t:
        TermTrue => Some(TypeBool())
        TermFalse => Some(TypeBool())
        TermInt(i) => Some(TypeInt())
        TermFloat(f) => Some(TypeFloat())
        TermString(s) => Some(TypeString())
        TermUnit => Some(TypeUnit())
        _ => None

fun is_type() -> bool
    let ty = type_of(TermTrue())
    ty.is_some()

fun main() -> i32
    0
        """)
  }
  test("check simple sum type") {
    fuse("""
type Animal:
  Dog
  Cat

fun value(a: Animal) -> i32
  match a:
      Dog => 0
      Cat => 1

fun main() -> i32
    value(Dog)
        """)
  }
  test("check inline lambda type inference") {
    fuse("""
fun main() -> i32
    let value = (a) => a + 1
    value(1)
        """)
  }
  test("check inline lambda type inference with two variables") {
    fuse("""
fun main() -> i32
    let value = (a, b) => a + b + 2
    value(1, 2)
        """)
  }
  test("check inline lambda type inference with unknown param") {
    fuse("""
fun main() -> i32
    let id = o => o
    id(1)
        """)
  }
  test("check inline lambda type inference invalid param") {
    fuse(
      """
fun main() -> i32
    let value = (a) => a + 1
    value("2")
        """,
      Some("expected type of `i32`, found `str`")
    )
  }
  test("check closure inference with match statement for primitive type") {
    fuse("""
fun main() -> i32
    let value = (b) => {
        match b:
            true => 1
            false => 0
    }
    value(true)
        """)
  }
  test("check closure inference with match statement for sum type") {
    fuse("""
type Animal:
  Dog
  Cat

fun main() -> i32
    let value = (a) => {
        match a:
            Dog => 0
            Cat => 1
    }
    value(Dog)
        """)
  }
  test("check clsoure inference with match statement for product type") {
    fuse("""
type Point:
  x: i32
  y: i32

fun main() -> i32
    let value = (a) => {
        match a:
            Point(x, y) => x + y
    }
    value(Point(2, 3))
        """)
  }
  test("check closure inference with match statement for option type") {
    fuse("""
type Option[T]:
    None
    Some(T)

fun main() -> i32
    # closure type should be Option[i32] -> i32
    # inferred by the first's case expression type -> i32
    let value = (a) => {
        match a:
            None => 0
            Some(v) => v
    }
    value(Some(5))
        """)
  }
  test("check closure inference with match statement for invalid arg") {
    fuse(
      """
type Option[T]:
    None
    Some(T)

fun main() -> i32
    let value = (a) => {
        match a:
            Some(v) => v
            None => 0
    }
    value(123)
        """,
      Some("expected type of `Option[i32]`, found `i32`")
    )
  }
  test("check closure inference with match statement for invalid generic arg") {
    fuse(
      """
type Option[T]:
    None
    Some(T)

fun main() -> i32
    let value = (a) => {
        match a:
            None => 0
            Some(v) => v
    }
    value(Some("123"))
        """,
      Some("expected type of `Option[i32]`, found `Option[str]`")
    )
  }
  test("check closure inference with match statement for nested option type") {
    fuse(
      """
type Animal[A]:
    Cat(A)
    Dog

type Option[T]:
    None
    Some(T)

fun main() -> i32
    let value = (a) => {
        match a:
            Cat(v) => v
            Dog => None[str]()
    }
    let v = value(Cat(Some("123")))
    match v:
      Some(_) => 1
      None => 0
        """
    )
  }
  test(
    "check closure inference with match statement for nested option type invalid arg"
  ) {
    fuse(
      """
type Animal[A]:
    Cat(A)
    Dog

type Option[T]:
    None
    Some(T)

fun main() -> i32
    let value = (a) => {
        match a:
            Cat(v) => v
            Dog => None[str]()
    }
    let v = value(Cat(Some("123")))
    let n = value(Cat(Some(123)))
        """,
      Some(
        "expected type of `Animal[Option[str]]`, found `Animal[Option[i32]]`"
      )
    )
  }
  test("check closure inference with match statement for list type") {
    fuse("""
type List[A]:
    Cons(h: A, t: List[A])
    Nil

fun main() -> i32
    let value = (a) => {
        match a:
            Nil => 0
            Cons(v, _) => v
    }
    value(Cons(1, Nil))
        """)
  }
  test("check recursive closure inference with match statement for list type") {
    fuse("""
type List[A]:
    Cons(h: A, t: List[A])
    Nil

fun main() -> i32
    let f = (a: i32) => int_to_str(a)
    let iter = (a, acc) => {
        match a:
            Nil => acc
            Cons(h, t) => Cons(f(h), iter(t, acc))
    }
    let v = iter(Cons(1, Nil), Nil[str]())
    match v:
      Cons(_, _) => 0
      Nil => 1
        """)
  }
  test(
    "check recursive closure inference with match statement for list type annotations required"
  ) {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

fun main() -> i32
    let iter = (a, acc) => {
        match a:
            Nil => acc
            Cons(h, t) => Cons(h, iter(t, acc))
    }
    let v = iter(Cons(1, Nil), Nil[str]())
    match v:
      Cons(_, _) => 0
      Nil => 1
        """,
      Some("can't determine the type of variable, type annotation is required")
    )
  }
  test("check inference with match statement for either type") {
    fuse("""
type Either[A, B]:
    Right(A)
    Left(B)

fun main() -> i32
    let value = (a) => {
        match a:
            Right(t) => t
            Left(b) => b
    }
    match value(Right("123")):
      "123" => 0
      _ => 1
        """)
  }
  test("check inference with match statement for either type invalid arg") {
    fuse(
      """
type Either[A, B]:
    Right(A)
    Left(B)

fun main() -> i32
    let value = (a) => {
        match a:
            Right(t) => t
            Left(b) => b
    }
    let r = value(Right("123"))
    let l = value(Left(123))
        """,
      Some(
        "expected type of `Either[str][str]`"
      )
    )
  }
}

object CompilerTests {

  /** Asserts fuse code is type checked. */
  def fuse(code: String, expectedError: Option[String] = None) = {
    (check(code), expectedError) match {
      case (t, None) => assert(t.isRight, s"\n${t.merge}")
      case (t, Some(error)) if t.isLeft =>
        assert(t.merge.contains(error), s"\n${error} not in:\n${t.merge}")
      case (_, Some(error)) =>
        assert(false, s"\ncheck passed, error not thrown: '${error}'")

    }

  }

  def check(code: String, fileName: String = "test.fuse") =
    Compiler.compile(CheckFile(fileName), code.trim, fileName)
}
