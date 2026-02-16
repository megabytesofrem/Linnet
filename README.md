# linnet
Small language, designed as a hybrid of Rust and ML.

## Features
- Immutability by default
- Implicitly monadic control flow via blocks (`{ }`)
- Zero `try/catch`

## Syntax

```rs
fn add (first:int second:int) = first + second

fn main () = {
    let result: int = add 1 2
    print result
    return result
}
```

```rs

fn main () = {
    let shape = Circle(15.0)
    let describe = match shape {
        Circle(radius) => "Radius: " ++ radius 
        Square(x, y)   => "Size: " ++ x ++ "*" ++ y
    }
}

```

## Type system
Side-effects (IO, launching a missile etc.) are represented via a special `Eff` type. Side-effectful control flow is handled
using a specialization of monadic IO and monadic result types, and Linnet provides a few syntax features to handle monadic flow.

The type system encodes side-effects, more akin to a simplification of Algebraic Effects.

- Side effect encoding: `fn maybe_launch_missile () -> Eff Unit (IO + FS)`
- Monadic binding: `let result <- World.maybe_launch_missile`
- Monadic sequencing: `result >>= print`, `log >> throw >> close`
- Monadic blocks: `{ let result = World.maybe_launch missile; log result; close result }`