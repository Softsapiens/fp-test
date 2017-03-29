package com.agilogy.fptest

import com.agilogy.fptest.RecordValue.Id

import scala.language.higherKinds

case class RecordAlreadyExists(ref:Id) extends Exception
case class RecordNotFound(ref:Id) extends Exception

trait RepoAlg[F[_]]{

  def select(ref: Id): F[Option[IdentifiedRecord]]

  def insert(record: IdentifiedRecord): F[Either[RecordAlreadyExists,Unit]]

  def save(record: IdentifiedRecord): F[Either[RecordNotFound,Unit]]

}

object RepoAlg{
  object syntax {
    def select[F[_]](ref: Id)(implicit alg:RepoAlg[F]): F[Option[IdentifiedRecord]] = alg.select(ref)
    def insert[F[_]](record: IdentifiedRecord)(implicit alg:RepoAlg[F]): F[Either[RecordAlreadyExists,Unit]] = alg.insert(record)
    def save[F[_]](record: IdentifiedRecord)(implicit alg:RepoAlg[F]): F[Either[RecordNotFound,Unit]] = alg.save(record)
  }
}

