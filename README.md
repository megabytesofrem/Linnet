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

// pointfree functions
fn add_pf = (+)

fn main () = {
    let result: Int = add 1 2
    print result
    return result
}
```

```rs
fn main () = {
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