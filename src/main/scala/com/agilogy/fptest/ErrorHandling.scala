package com.agilogy.fptest

import scala.language.higherKinds

trait ErrorHandling[M[_]] {

  def leftMap[L, R, LL](m: M[Either[L, R]])(f: L => LL): M[Either[LL, R]]

  def rightMap[L, R, RR](m: M[Either[L, R]])(f: R => RR): M[Either[L, RR]]

  def rightFlatMap[L, R, LL >: L, RR](m: M[Either[L, R]])(f: R => M[Either[LL, RR]]): M[Either[LL, RR]]

}

object ErrorHandling {

  //  implicit def toErrorHandlingOps[M[_]:ErrorHandling,L,R](m:M[Either[L,R]]):ErrorHandlingOps[M,L,R] = new ErrorHandlingOps(m)

  implicit class ErrorHandlingOps[M[_] : ErrorHandling, L, R](m: M[Either[L, R]]) {

    private val errorHandling = implicitly[ErrorHandling[M]]

    def rightMap[RR](f: R => RR): M[Either[L, RR]] = errorHandling.rightMap(m)(f)

    def leftMap[LL](f: L => LL): M[Either[LL, R]] = errorHandling.leftMap(m)(f)

    def rightFlatMap[LL >: L, RR](f: R => M[Either[LL, RR]]): M[Either[LL, RR]] = errorHandling.rightFlatMap[L, R, LL, RR](m)(f)

  }

  object monad {

    implicit class ErrorHandlingOps[M[_] : ErrorHandling, L, R](m: M[Either[L, R]]) {

      private val errorHandling = implicitly[ErrorHandling[M]]

      def flatMap[LL >: L, RR](f: R => M[Either[LL, RR]]): M[Either[LL, RR]] = errorHandling.rightFlatMap[L, R, LL, RR](m)(f)

      def map[RR](f: R => RR): M[Either[L, RR]] = errorHandling.rightMap[L, R, RR](m)(f)

    }

  }

}
