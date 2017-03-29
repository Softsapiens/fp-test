package com.agilogy.fptest

import scala.language.higherKinds

trait Monad[M[_]] {
  def map[T, T2](m: M[T])(f: T => T2): M[T2]

  def flatMap[T, T2](m: M[T])(f: T => M[T2]): M[T2]

  def pure[T](v: T): M[T]
}

object Monad {

  implicit class MonadOps[M[_] : Monad, T](self: M[T]) {
    private val monad = implicitly[Monad[M]]

    def map[T2](f: T => T2): M[T2] = monad.map(self)(f)

    def flatMap[T2](f: T => M[T2]): M[T2] = monad.flatMap(self)(f)
  }

  implicit class MonadCoops[T](self: T) {
    def pure[M[_] : Monad]: M[T] = implicitly[Monad[M]].pure(self)
  }

  implicit def eitherMonad[L]: Monad[({type λ[α] = Either[L, α]})#λ] = new Monad[({type λ[α] = Either[L, α]})#λ] {

    override def map[T, T2](m: Either[L, T])(f: (T) => T2): Either[L, T2] = m match {
      case Right(r) => Right(f(r))
      case Left(l) => Left(l)
    }

    override def flatMap[T, T2](m: Either[L, T])(f: (T) => Either[L, T2]): Either[L, T2] = m match {
      case Right(r) => f(r)
      case Left(l) => Left(l)
    }

    override def pure[T](v: T): Either[L, T] = Right(v)
  }

}