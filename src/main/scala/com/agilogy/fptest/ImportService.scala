package com.agilogy.fptest

import com.agilogy.fptest.RecordValue.Id

import scala.language.higherKinds

object ImportService {

  def saveReferencedIfNotFound[M[_] : RepoAlg : Monad](ref: Id): M[Unit] = {
    import Monad._
    import RepoAlg.syntax._
    for{
      r <- select(ref)
      // We can ignore the result of insert, since either way it finally guarantees the record will exist
      _ <- r.map(_ => Right(()).pure[M]).getOrElse {
        insert(IdentifiedRecord(ref, Record()))
      }
    } yield ().pure //res.left.map(_ => UnexpectedImportError("The record already exists but it was just found to not exist"))
  }

  case class UnexpectedImportError(msg:String)

  def importRecord[M[_] : RepoAlg : Monad: ErrorHandling](record: IdentifiedRecord): M[Either[UnexpectedImportError, Unit]] = {
    import Monad._
    import RepoAlg.syntax._

    // An example of a method without error handling:
    def saveReferences: M[Record] = record.body.properties.foldLeft(Record().pure) {
      case (acc, (n, ref: Id)) =>
        for {
          r <- acc
          _ <- saveReferencedIfNotFound(ref)
        } yield r.set(n, ref)
        case (acc, (n,v)) => acc.map(_.set(n,v))
    }

    import ErrorHandling._
    // An example for comprehension over M (no error handling) with ad-hoc error handling inside
    for {
      resolvedRefsRecord <- saveReferences
      currentRecord <- select(record.self)
      res <- currentRecord match{
        case Some(cr) => save(cr.merge(resolvedRefsRecord)).leftMap(_ => UnexpectedImportError(s"Unexpected object not found"))
        case None => insert(IdentifiedRecord(record.self,resolvedRefsRecord)).leftMap(_ => UnexpectedImportError(s"Unexpected idInSource already exists"))
      }
    } yield res
  }

  // An example full error handling method (including error handling monad)
  def insertTwice[M[_]:RepoAlg:Monad:ErrorHandling](r: Record, id1:Id, id2:Id): M[Either[RecordAlreadyExists, Unit]] = {
    import RepoAlg.syntax._
    import ErrorHandling.monad._
//    insert[M](IdentifiedRecord(id1, r)).rightFlatMap(_ => insert(IdentifiedRecord(id2,r)))
    for{
      _ <- insert(IdentifiedRecord(id1,r))
      i2 <- insert(IdentifiedRecord(id2,r))
    } yield i2
  }

  def insertN[M[_]:RepoAlg:Monad](r: Record, times:Int): M[Either[RecordAlreadyExists, Unit]] = ???

}

