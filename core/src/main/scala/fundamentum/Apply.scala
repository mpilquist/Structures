package fundamentum

import simulacrum.{ typeclass, noop }

/**
 * Describes type constructors that are functors and that support the `apply` method such that:
 *  - given `fa: F[A]`, `fab: F[A => B]`, and `fac: F[A => C]`,
 *    `apply(apply(fa)(fab))(fbc) == apply(fa)(apply(fab)(map(fbc)(bc => ab => ab andThen bc)))`
 *
 * This property states that the result of first applying a `F[A => B]` followed by applying a
 * `F[B => C]` must yield the same result as combining the `F[A => B]` and `F[B => C]` in to a
 * single `F[A => C]` and then applying that to `F[A]`.
 *
 * This type class models a more general version of an [[Applicative]] -- specifically, there's
 * no requirement for the `insert` method to exist.
 */
@typeclass trait Apply[F[_]] extends Functor[F] {

  @noop def apply[A, B](fa: F[A])(f: F[A => B]): F[B]


  @noop def apply2[A, B, X](fa: F[A], fb: F[B])(f: F[(A, B) => X]): F[X] =
    apply(fa)(apply(fb)(map(f)(ff => (b: B) => (a: A) => ff(a, b))))

  @noop def apply3[A, B, C, X](fa: F[A], fb: F[B], fc: F[C])(f: F[(A, B, C) => X]): F[X] =
    apply(fa)(apply2(fb, fc)(map(f)(ff => (b: B, c: C) => (a: A) => ff(a, b, c))))

  @noop def apply4[A, B, C, D, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: F[(A, B, C, D) => X]): F[X] =
    apply2(fa, fb)(apply2(fc, fd)(map(f)(ff => (c: C, d: D) => (a: A, b: B) => ff(a, b, c, d))))

  @noop def apply5[A, B, C, D, E, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E])(f: F[(A, B, C, D, E) => X]): F[X] =
    apply2(fa, fb)(apply3(fc, fd, fe)(map(f)(ff => (c: C, d: D, e: E) => (a: A, b: B) => ff(a, b, c, d, e))))

  @noop def apply6[A, B, C, D, E, G, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G])(f: F[(A, B, C, D, E, G) => X]): F[X] =
    apply3(fa, fb, fc)(apply3(fd, fe, fg)(map(f)(ff => (d: D, e: E, g: G) => (a: A, b: B, c: C) => ff(a, b, c, d, e, g))))

  @noop def apply7[A, B, C, D, E, G, H, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H])(f: F[(A, B, C, D, E, G, H) => X]): F[X] =
    apply3(fa, fb, fc)(apply4(fd, fe, fg, fh)(map(f)(ff => (d: D, e: E, g: G, h: H) => (a: A, b: B, c: C) => ff(a, b, c, d, e, g, h))))

  @noop def apply8[A, B, C, D, E, G, H, I, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I])(f: F[(A, B, C, D, E, G, H, I) => X]): F[X] =
    apply4(fa, fb, fc, fd)(apply4(fe, fg, fh, fi)(map(f)(ff => (e: E, g: G, h: H, i: I) => (a: A, b: B, c: C, d: D) => ff(a, b, c, d, e, g, h, i))))

  @noop def apply9[A, B, C, D, E, G, H, I, J, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J])(f: F[(A, B, C, D, E, G, H, I, J) => X]): F[X] =
    apply4(fa, fb, fc, fd)(apply5(fe, fg, fh, fi, fj)(map(f)(ff => (e: E, g: G, h: H, i: I, j: J) => (a: A, b: B, c: C, d: D) => ff(a, b, c, d, e, g, h, i, j))))

  @noop def apply10[A, B, C, D, E, G, H, I, J, K, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J], fk: F[K])(f: F[(A, B, C, D, E, G, H, I, J, K) => X]): F[X] =
    apply5(fa, fb, fc, fd, fe)(apply5(fg, fh, fi, fj, fk)(map(f)(ff => (g: G, h: H, i: I, j: J, k: K) => (a: A, b: B, c: C, d: D, e: E) => ff(a, b, c, d, e, g, h, i, j, k))))



  @noop def map2[A, B, X](fa: F[A], fb: F[B])(f: (A, B) => X): F[X] =
    apply(fa)(map(fb)(b => (a: A) => f(a, b)))

  @noop def map3[A, B, C, X](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => X): F[X] =
    apply(fa)(map2(fb, fc)((b, c) => a => f(a, b, c)))

  @noop def map4[A, B, C, D, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => X): F[X] =
    map2(tuple2(fa, fb), tuple2(fc, fd)) { case ((a, b), (c, d)) => f(a, b, c, d) }

  @noop def map5[A, B, C, D, E, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E])(f: (A, B, C, D, E) => X): F[X] =
    map2(tuple2(fa, fb), tuple3(fc, fd, fe)) { case ((a, b), (c, d, e)) => f(a, b, c, d, e) }

  @noop def map6[A, B, C, D, E, G, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G])(f: (A, B, C, D, E, G) => X): F[X] =
    map2(tuple3(fa, fb, fc), tuple3(fd, fe, fg)) { case ((a, b, c), (d, e, g)) => f(a, b, c, d, e, g) }

  @noop def map7[A, B, C, D, E, G, H, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H])(f: (A, B, C, D, E, G, H) => X): F[X] =
    map2(tuple3(fa, fb, fc), tuple4(fd, fe, fg, fh)) { case ((a, b, c), (d, e, g, h)) => f(a, b, c, d, e, g, h) }

  @noop def map8[A, B, C, D, E, G, H, I, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I])(f: (A, B, C, D, E, G, H, I) => X): F[X] =
    map2(tuple4(fa, fb, fc, fd), tuple4(fe, fg, fh, fi)) { case ((a, b, c, d), (e, g, h, i)) => f(a, b, c, d, e, g, h, i) }

  @noop def map9[A, B, C, D, E, G, H, I, J, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J])(f: (A, B, C, D, E, G, H, I, J) => X): F[X] =
    map2(tuple4(fa, fb, fc, fd), tuple5(fe, fg, fh, fi, fj)) { case ((a, b, c, d), (e, g, h, i, j)) => f(a, b, c, d, e, g, h, i, j) }

  @noop def map10[A, B, C, D, E, G, H, I, J, K, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J], fk: F[K])(f: (A, B, C, D, E, G, H, I, J, K) => X): F[X] =
    map2(tuple5(fa, fb, fc, fd, fe), tuple5(fg, fh, fi, fj, fk)) { case ((a, b, c, d, e), (g, h, i, j, k)) => f(a, b, c, d, e, g, h, i, j, k) }



  @noop def tuple2[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  @noop def tuple3[A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] =
    map3(fa, fb, fc)((_, _, _))

  @noop def tuple4[A, B, C, D](fa: F[A], fb: F[B], fc: F[C], fd: F[D]): F[(A, B, C, D)] =
    map4(fa, fb, fc, fd)((_, _, _, _))

  @noop def tuple5[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E]): F[(A, B, C, D, E)] =
    map5(fa, fb, fc, fd, fe)((_, _, _, _, _))

  @noop def tuple6[A, B, C, D, E, G](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G]): F[(A, B, C, D, E, G)] =
    map6(fa, fb, fc, fd, fe, fg)((_, _, _, _, _, _))

  @noop def tuple7[A, B, C, D, E, G, H](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H]): F[(A, B, C, D, E, G, H)] =
    map7(fa, fb, fc, fd, fe, fg, fh)((_, _, _, _, _, _, _))

  @noop def tuple8[A, B, C, D, E, G, H, I](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I]): F[(A, B, C, D, E, G, H, I)] =
    map8(fa, fb, fc, fd, fe, fg, fh, fi)((_, _, _, _, _, _, _, _))

  @noop def tuple9[A, B, C, D, E, G, H, I, J](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J]): F[(A, B, C, D, E, G, H, I, J)] =
    map9(fa, fb, fc, fd, fe, fg, fh, fi, fj)((_, _, _, _, _, _, _, _, _))

  @noop def tuple10[A, B, C, D, E, G, H, I, J, K](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J], fk: F[K]): F[(A, B, C, D, E, G, H, I, J, K)] =
    map10(fa, fb, fc, fd, fe, fg, fh, fi, fj, fk)((_, _, _, _, _, _, _, _, _, _))
}
