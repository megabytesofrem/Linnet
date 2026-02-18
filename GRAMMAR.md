# Linnet Language Specification

---

## I. Binding Operations
Bindings in Linnet are immutable assignments that establish an identity within a specific lexical or monadic scope.

### 1. Atomic Binding
- `let x = e`  
Binds the identifier `x` to the result of expression `e` within the current monadic context.

### 2. Explicitly Typed Binding
- `let x: T = e`  
An explicitly typed binding where `T` represents a type variable or a ground type. This ensures strict type-checking at the point of definition.

### 3. Local Binding (Let-In and Where)
- `let x = e in b`  
Introduces a local variable `x` into the scope of the expression `b`. This is semantically equivalent to the application of a lambda abstraction: $(\lambda x. b) e$.

- `let x = e where e = s`  
By allowing both let and where bindings, this allows for top-down readability in complex expressions.

---

## II. Control Flow
Linnet enforces totality; all conditional branches must be exhaustive

### 1. Conditional Expressions
- `if c then t else f`  
A expression where both `t` and `f` must evaluate to the same type $t$. In a monadic sequence where no return value is intended, both branches must return the Unit type `()`.

---

## III. Monadic Structure
Linnet treats sequential computation as a series of transformations within a Monad $m$. Sequential blocks are desugared into nested applications of the bind operator.

### 1. Monadic Block
- `{ e1; e2; ...; en }`  
Desugars into a chain of monadic binds (`>>=`). Each expression $e_i$ is bound to the subsequent expression, threading the internal state through the sequence.

- `let a <- n`
Introduces a new monadic binding `a` that lives within the inner monad. `a` can be used to perform side-effects, since
side-effects are wrapped in a monad.

### 2. Bind Operator (`>>=`)
- `m >>= f`  
The standard monadic bind. It extracts the value from the computation `m` and passes it to the function `f`, which returns a new monadic value.

### 3. Sequence Operator (`>>`)
- `m >> n`  
Sequences two computations, discarding the value produced by `m` while preserving its computational effects.

### 4. Iterative Structures (Loop)
- `loop (init) { body }`  
Implements stateful iteration via tail-recursive semantics. The `next` keyword is used to re-enter the `body` with an updated state, ensuring stack-safe execution.

---

## IV. Functor and Applicative Algebra
Linnet provides standard algebraic operators for working with lifted types.

### 1. Covariant Mapping (fmap)
- `f <$> x`  
Applies a function $f: A \to B$ to a value of type $F A$ to produce a result of type $F B$.

### 2. Applicative Application
- `f <*> x`  
Applies a wrapped function $f: F (A \to B)$ to a wrapped value $x: F A$ to produce a result of type $F B$.