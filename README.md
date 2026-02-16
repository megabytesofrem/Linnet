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