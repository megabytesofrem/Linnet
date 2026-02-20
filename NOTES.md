# Type system

## Primitive types λ→

**Axis**: Terms depending on terms. T → T. `Int` → `String`.
**Morphism**: a: A ⊢ f (a) : B
**Result**: The arrow works between types.

## Polymorphic types λ2

**Axis**: Types that accept a polymorphic parameter α. This is how C# and Java generics 
function e.g `List<α>` can be filled in with `α → String`.

In λ2, type constructors (`List`) only accept a single level of polymorphism - it is
impossible to encode `List (m α)` or in C# syntax `List<m<α>>`.

**Morphism**: Λα.e : ∀α.A

**Result**: The arrow works not just for `Int` (as in λ→) but on `forall α`, and becomes
generic between all `α`. This allows for universal quantification.

## Higher Kinded Type Polymorphism λω

**Axis**: Types that depend on other types (`* → *`)

In λω, we introduce **kinds** to classify types. A concrete type like `Int` has the kind
`*`. A type constructor like `List` is no longer a compiler-magic primitive, but a first-class inhabitant of the type system with kind `* → *`.

This allows for **higher order type operators**. You can define a type that accepts a **type constructor** as an argument (`T : (* → *) → *`) - meaning `T` is a constructor that accepts as another constructor (`List` or `Maybe`) to produce a concrete type.

**Morphism**: λα : κ.T : κ1 ​→ κ2​

**Result**: The arrow is lifted to the type level. This enables the encoding of structures that are generic over any type constructors that have kind `* → *`, such as `Functor`. It removes the "single-level" restriction of λ2 by allowing the application of types to type constructors.