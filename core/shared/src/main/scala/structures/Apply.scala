package structures

import simulacrum.{ typeclass, noop }

/**
 * Type class that describes functors that support an `apply` method which adheres
 * to the laws described in `structures.laws.ApplyLaws`.
 *
 * This type class models a more general version of an [[Applicative]] -- specifically, there's
 * no requirement for the `pure` method to exist.
 */
@typeclass trait Apply[F[_]] extends Any with Functor[F] { self =>

  def apply[A, B](fa: F[A])(f: F[A => B]): F[B]

  @noop def flip[A, B](f: F[A => B]): F[A] => F[B] =
    fa => apply(fa)(f)


  @noop def apply2[A, B, X](fa: F[A], fb: F[B])(f: F[(A, B) => X]): F[X] =
    apply(fb)(apply(fa)(map(f)(ff => (a: A) => (b: B) => ff(a, b))))

  @noop def apply3[A, B, C, X](fa: F[A], fb: F[B], fc: F[C])(f: F[(A, B, C) => X]): F[X] =
    apply(fc)(apply2(fa, fb)(map(f)(ff => (a: A, b: B) => (c: C) => ff(a, b, c))))

  @noop def apply4[A, B, C, D, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: F[(A, B, C, D) => X]): F[X] =
    apply2(fc, fd)(apply2(fa, fb)(map(f)(ff => (a: A, b: B) => (c: C, d: D) => ff(a, b, c, d))))

  @noop def apply5[A, B, C, D, E, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E])(f: F[(A, B, C, D, E) => X]): F[X] =
    apply2(fd, fe)(apply3(fa, fb, fc)(map(f)(ff => (a: A, b: B, c: C) => (d: D, e: E) => ff(a, b, c, d, e))))

  @noop def apply6[A, B, C, D, E, G, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G])(f: F[(A, B, C, D, E, G) => X]): F[X] =
    apply3(fd, fe, fg)(apply3(fa, fb, fc)(map(f)(ff => (a: A, b: B, c: C) => (d: D, e: E, g: G) => ff(a, b, c, d, e, g))))

  @noop def apply7[A, B, C, D, E, G, H, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H])(f: F[(A, B, C, D, E, G, H) => X]): F[X] =
    apply3(fe, fg, fh)(apply4(fa, fb, fc, fd)(map(f)(ff => (a: A, b: B, c: C, d: D) => (e: E, g: G, h: H) => ff(a, b, c, d, e, g, h))))

  @noop def apply8[A, B, C, D, E, G, H, I, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I])(f: F[(A, B, C, D, E, G, H, I) => X]): F[X] =
    apply4(fe, fg, fh, fi)(apply4(fa, fb, fc, fd)(map(f)(ff => (a: A, b: B, c: C, d: D) => (e: E, g: G, h: H, i: I) => ff(a, b, c, d, e, g, h, i))))

  @noop def apply9[A, B, C, D, E, G, H, I, J, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J])(f: F[(A, B, C, D, E, G, H, I, J) => X]): F[X] =
    apply4(fg, fh, fi, fj)(apply5(fa, fb, fc, fd, fe)(map(f)(ff => (a: A, b: B, c: C, d: D, e: E) => (g: G, h: H, i: I, j: J) => ff(a, b, c, d, e, g, h, i, j))))

  @noop def apply10[A, B, C, D, E, G, H, I, J, K, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J], fk: F[K])(f: F[(A, B, C, D, E, G, H, I, J, K) => X]): F[X] =
    apply5(fg, fh, fi, fj, fk)(apply5(fa, fb, fc, fd, fe)(map(f)(ff => (a: A, b: B, c: C, d: D, e: E) => (g: G, h: H, i: I, j: J, k: K) => ff(a, b, c, d, e, g, h, i, j, k))))



  @noop def map2[A, B, X](fa: F[A], fb: F[B])(f: (A, B) => X): F[X] =
    apply(fb)(map(fa)(a => (b: B) => f(a, b)))

  @noop def map3[A, B, C, X](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => X): F[X] =
    apply(fc)(map2(fa, fb)((a, b) => c => f(a, b, c)))

  @noop def map4[A, B, C, D, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => X): F[X] =
    map2(tuple2(fc, fd), tuple2(fa, fb)) { case ((c, d), (a, b)) => f(a, b, c, d) }

  @noop def map5[A, B, C, D, E, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E])(f: (A, B, C, D, E) => X): F[X] =
    map2(tuple2(fd, fe), tuple3(fa, fb, fc)) { case ((d, e), (a, b, c)) => f(a, b, c, d, e) }

  @noop def map6[A, B, C, D, E, G, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G])(f: (A, B, C, D, E, G) => X): F[X] =
    map2(tuple3(fd, fe, fg), tuple3(fa, fb, fc)) { case ((d, e, g), (a, b, c)) => f(a, b, c, d, e, g) }

  @noop def map7[A, B, C, D, E, G, H, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H])(f: (A, B, C, D, E, G, H) => X): F[X] =
    map2(tuple3(fe, fg, fh), tuple4(fa, fb, fc, fd)) { case ((e, g, h), (a, b, c, d)) => f(a, b, c, d, e, g, h) }

  @noop def map8[A, B, C, D, E, G, H, I, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I])(f: (A, B, C, D, E, G, H, I) => X): F[X] =
    map2(tuple4(fe, fg, fh, fi), tuple4(fa, fb, fc, fd)) { case ((e, g, h, i), (a, b, c, d)) => f(a, b, c, d, e, g, h, i) }

  @noop def map9[A, B, C, D, E, G, H, I, J, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J])(f: (A, B, C, D, E, G, H, I, J) => X): F[X] =
    map2(tuple4(fg, fh, fi, fj), tuple5(fa, fb, fc, fd, fe)) { case ((g, h, i, j), (a, b, c, d, e)) => f(a, b, c, d, e, g, h, i, j) }

  @noop def map10[A, B, C, D, E, G, H, I, J, K, X](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J], fk: F[K])(f: (A, B, C, D, E, G, H, I, J, K) => X): F[X] =
    map2(tuple5(fg, fh, fi, fj, fk), tuple5(fa, fb, fc, fd, fe)) { case ((g, h, i, j, k), (a, b, c, d, e)) => f(a, b, c, d, e, g, h, i, j, k) }



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

  def compose[G[_]: Apply]: Apply[Lambda[X => F[G[X]]]] =
    new Apply.Composite[F, G] {
      def F = self
      def G = Apply[G]
    }
}

object Apply {

  trait Composite[F[_], G[_]] extends Any with Apply[Lambda[X => F[G[X]]]] with Functor.Composite[F, G] {
    def F: Apply[F]
    def G: Apply[G]
    override def apply[A, B](fa: F[G[A]])(f: F[G[A => B]]): F[G[B]] = {
      val flipped: F[G[A] => G[B]] = F.map(f)(G.flip)
      F.apply(fa)(flipped)
    }
  }
}
