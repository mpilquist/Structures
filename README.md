fundamentum
===========

[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/mpilquist/Structures?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Fundamentum is a zero-dependency Scala library that defines commonly used type classes for functional programming.

There are two modules, core and laws. The former provides the API of the library and the latter provides [Discipline](http://github.com/typelevel/discipline) based laws for each of the type classes. The laws project is intended to be used in test scope, to verify that instances of the type classes are law abiding.

## FAQ

### Is this a fork of Scalaz?

Yes and no. This is a reimplementation of the core type classes which borrows heavily from the ideas in Scalaz. It differs in a number of significant ways though:

 - Fundamentum defines type classes (and a minimal set of types needed for the implementation of those type classes), whereas Scalaz encompasses much more functionality, including data types (e.g., `NonEmptyList`, `Validation`, `\/`), replacements for standard library types (e.g., `Order`), and monad transformers (e.g., `Reader` / `Kleisli`). Fundamentum has a much smaller focus than Scalaz.
 - Fundamentum attempts to fit in with the Scala standard library when possible, even when the standard library provides unsafe functionality. This is contrary to Scalaz, which often provides safe alternatives to standard library constructs. For instance, `Maybe` is equivalent to `Option` but omits the unsafe `get` operation, and `Order` is equivalent to `scala.math.Ordering`, but defines the result of a comparison to be an enumerated type rather than an integer. There is great value in using safe alternatives, but providing those alternatives is not in scope of Fundamentum.
 - Fundamentum defines a small subset of the type classes defined by Scalaz. For example, Fundamentum does not provide abstractions for modeling categories, arrows, or adjoint functors.
 - Fundamentum uses strict evaluation in many places that Scalaz uses lazy evaluation. For example, `Semigroup` is defined strictly in both arguments. This results in a loss of expressive power, but a gain in performance.
 - Fundamentum does not provide special syntax support for working with implicit type class instances of odd shapes. Scalaz provides the `Unapply` type for dealing with odd shapes.

In summary, Fundamentum models a very small subset of what is provided by Scalaz, and does so in a different way, with a different goal.

### Why is a minimizing API footprint desirable? Can't I just ignore stuff I don't use?

One very real cost of a large API is that there is more stuff to change over time. These changes tend to be organized in to versioned releases, with some claim of binary compatibility or incompatibility. Tooling may enforce the binary compatibility claim. However, tooling typically operates on versioned releases rather than individual artifacts like types and methods. As a result, even if you use the stablest part of a large library, you have to ensure the version of the library you use is binary compatible with the version used by all other libraries in your application, or otherwise risk linkage errors resulting from no tool checked binary compatibility.

Fundamentum aims to be a small, stable library, with binary incompatible releases occurring at the same frequency as major Scala releases.

### Will this force library authors to choose between Scalaz and Fundamentum?

Fundamentum is an experiment. It is focused on minimizing the number of type classes and integrating them closely with Scala language features and the standard library. It is entirely possible that this experiment fails in the sense that it no longer should exist as a library. In which case, the overall effort would still be valuable by demonstrating why a smaller subset of type classes using strict evaluation is not useful.

To be clear, in its current form, Fundamentum is not intended to be used in libraries.

## Acknowledgements

This project is influenced by a number of other projects. Notably:

 - [Scalaz](http://github.com/scalaz/scalaz)
 - [Functional Programming in Scala](http://www.manning.com/bjarnason/)
 - [Ed Kmett's semigroupoids](https://hackage.haskell.org/package/semigroupoids)

This project is also influenced by a number of individuals, who will be added here with their agreement.
