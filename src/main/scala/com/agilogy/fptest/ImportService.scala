package com.agilogy.fptest

import com.agilogy.fptest.RecordValue.Id

import scala.language.higherKinds

object ImportService {
  import Monad._
  import RepoAlg.syntax._

  def saveReferencedIfNotFound[M[_] : RepoAlg : Monad](ref: Id): M[Unit] = {
    for{
      r <- select(ref)
      // We can ignore the result of insert, since either way it finally guarantees the record will exist
      _ <- r.map(_ => Right(()).pure[M]).getOrElse {
        insert(IdentifiedRecord(ref, Record()))
      }
    } yield ().pure //res.left.map(_ => UnexpectedImportError("The record already exists but it was just found to not exist"))
  }

  case class UnexpectedImportError(msg:String)

  def importRecord[M[_] : RepoAlg : Monad](record: IdentifiedRecord): M[Either[UnexpectedImportError, Unit]] = {

    def saveReferences = record.body.properties.foldLeft(Record().pure) {
      case (acc, (n, ref: Id)) =>
        for {
          r <- acc
          _ <- saveReferencedIfNotFound(ref)
        } yield r.set(n, ref)
        case (acc, (n,v)) => acc.map(_.set(n,v))
    }

    for {
      resolvedRefsRecord <- saveReferences
      currentRecord <- select(record.self)
      res <- currentRecord match{
        case Some(cr) =>
          for{
            res <- save(cr.merge(resolvedRefsRecord))
          } yield res.left.map{
            case RecordNotFound(o) => UnexpectedImportError(s"Unexpected object not found $o")
          }
        case None =>
          for{
            res <- insert(IdentifiedRecord(record.self,resolvedRefsRecord))
          } yield res.left.map{
            case RecordAlreadyExists(e) => UnexpectedImportError(s"Unexpected idInSource already exists: $e")
          }

      }
    } yield res
  }

  def insertTwice[M[_]:RepoAlg:Monad](r: Record, id1:Id, id2:Id): M[Either[RecordAlreadyExists, Unit]] = {
    import RepoAlg.syntax._
    import Monad._
    for {
      i1 <- insert(IdentifiedRecord(id1,r))
      res <- i1 match{
        case Left(l) => Left(l).pure
        case Right(_) => insert(IdentifiedRecord(id2,r))
      }
    } yield res
  }

  def insertN[M[_]:RepoAlg:Monad](r: Record, times:Int): M[Either[RecordAlreadyExists, Unit]] = ???

}

