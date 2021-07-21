**Fuse** is a statically typed functional language.

# Features

* Algebraic Data Types (ADT)
* Generics (Parametric Polymorphism)
* Type Methods
* Pattern Matching

# Road-map

- [x] Fuse Parser
- [x] Type Checker for System F with ADT and Pattern Matching
- [ ] Improve Type Error Messages
- [ ] Code Generation to LLVM
- [ ] Implement Type Inference (HM or Bidirectional)
- [ ] Add modules & imports
- [ ] Implement Type Classes

# Working Example with Tree-Sitter

![fuse](https://user-images.githubusercontent.com/6879030/126560462-08a46755-9162-4ba3-900c-f5421cff1e4c.png)

# ADTs (Algebraic Data Types)

## Sum type (Enumeration)

```
type bool:
    true
    false
```

## Record (Product type)

```
type Point:
    x: i32
    y: i32
```

## Tuple (Product type)

```
type Pair(i32, str)
```

## Generics (parametric polymorphism)

```
type Option[T]:
    None
    Some(T)

type Map[K, V]:
    key: K
    value: V

type Data[T](Option[Map[str, T]])
```

# Type Methods

```
impl Option[T]:
    fun is_some() -> bool
        match this:
            Some(_) => True
            None => False

impl Point:
    fun distance(other: Point) -> f32
        let x_diff = this.x - other.x
        let y_diff = this.y - other.y
        math.sqrt(x_diff * x_diff - y_diff * y_diff)
```

# Type Alias

```
type Ints = List[i32]
```

# Expressions

## Operators

```
1 + 1
# output: 2 (i32)

2 - 1
# output: 1 (i32)

"Hello " + " World"
# output: "Hello World" (str)

1 == 2
# output: false (bool)

# comparison and equality
less < than
less_than <= or_equal
greater > then
greaterThan >= orEqual
```

## Let expression

```
let x = 5
let y = x + 1

let m = Some(5)
```

## Match expression

```
let x = 2

match x:
    1 => "one"
    2 => "two"
    _ => "whatever"

let m = Some(5)
match m:
    Some(v) => v + 1
    None => 0
```

# Functions

```
fun sum(x: i32, y: i32) -> i32
    x + y

sum(5, 3)
```

## Recursive Functions

```
fun fib(n: i32, a: i32, b: i32) -> i32
    match n:
        0 => b
        _ => fib(n - 1, b, a + b)
```

## Lambda Functions

```
let f = a => a + 1

let g = (x, y) => x + y

let f_annotated: i32 = (a: i32) -> i32 => a + 1
```
