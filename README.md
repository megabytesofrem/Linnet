# linnet
Small purely functional language inspired by Rust and Haskell

## Features
- Pure immutability
- Implicitly monadic control flow via blocks (`{ }`)
- Zero `try/catch`, monadic error types are used instead

## Syntax

```rs
// most general type will be inferred
// add : Numeric a => a -> a -> a
fn add first second = first + second

// complex calculation using let .. in
fn calc_complex = 
    let first_part = 5 in 
    let next_part  = 10 in 
    let final_part = 5 in 
    first_part + next_part - final_part

// complex calculation using 'where'
fn calc_complex_2 = result
    where
      let first_part = 5
      let next_part  = 10
      let final_part = 5
      let result = first_part + next_part - final_part

fn main () -> Eff Unit = {
    let result: Int = add 1 2
    print result
    pure result
}
```

```rs
fn main () -> Eff Unit = {
    let shape = Circle(15.0)

    // Rust/Haskell style pattern matching
    let describe = match shape {
        Circle(radius) -> "Radius: " ++ radius 
        Square(x, y)   -> "Size: " ++ x ++ "*" ++ y
    }
}
```

## Type system
Side-effects (IO, launching a missile etc.) are represented via a special `Eff` type. Side-effectful control flow is handled
using a specialization of monadic IO and monadic result types.

> [!WARNING]
> `Eff a` currently translates to `IO a` from Haskell, but a proper effect system is planned later.