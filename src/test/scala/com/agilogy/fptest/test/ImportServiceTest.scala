package com.agilogy.fptest.test

import java.time.Instant

import com.agilogy.fptest._
import com.agilogy.fptest.RecordValue.Id
import org.scalatest.FunSpec

import scala.language.higherKinds

// https://github.com/hablapps/gist/blob/master/src/test/scala/objectalgebras-vs-free-vs-eff/ObjectAlgebras.scala


class ImportServiceTest extends FunSpec {

  import com.agilogy.fptest.State._

  case class TestRepoState(records: Map[Id, IdentifiedRecord]) {
  }

  type TestAction[T] = State[TestRepoState, T]

  //TODO: Are we throwing the lefts away here?
  implicit object TestActionMonad extends Monad[TestAction] {
    override def map[T, T2](m: TestAction[T])(f: (T) => T2): TestAction[T2] = {
      s =>
        val (s2, res) = m(s)
        (s2, f(res))
    }

    override def flatMap[T, T2](m: TestAction[T])(f: (T) => TestAction[T2]): TestAction[T2] = {
      s =>
        val (s2, ires) = m(s)
        f(ires)(s2)
    }

    override def pure[T](v: T): TestAction[T] = {
      s =>
        (s, v)
    }

  }

  implicit object TestRepoAlg extends RepoAlg[TestAction] {

    override def select(ref: Id): TestAction[Option[IdentifiedRecord]] = {
      s =>
        (s, s.records.get(ref))
    }

    override def insert(record: IdentifiedRecord): TestAction[Either[RecordAlreadyExists, Unit]] = {
      s =>
        if (s.records.contains(record.self)) {
          (s, Left(RecordAlreadyExists(record.self)))
        } else {
          val s2 = s.copy(records = s.records ++ Map(record.self -> record))
          (s2, Right(()))
        }
    }

    override def save(record: IdentifiedRecord): TestAction[Either[RecordNotFound, Unit]] = {
      s =>
        if (!s.records.contains(record.self)) {
          (s, Left(RecordNotFound(record.self)))
        } else {
          val s2 = s.copy(records = s.records ++ Map(record.self -> record))
          (s2, Right(()))
        }
    }
  }

  private val ref1 = Id("1")
  private val name = "name" -> RecordValue.Text("Kleisli")
  private val r1 = IdentifiedRecord(ref1, Record(name))

  describe("importRecord") {
    import com.agilogy.fptest.ImportService._
    import Monad._

    it("should insert a record when there was no record with such idInSource") {
      val p = importRecord[TestAction](r1)
      val (s2, res) = p(TestRepoState(Map.empty))
      val r = s2.records(ref1)
      assert(r === r1)
      assert(res.isRight)
    }

    it("should update the record when a record is found with such idInSource") {
      val hobby = "hobby" -> RecordValue.Text("maths")
      val s: TestAction[Either[UnexpectedImportError, Unit]] = for {
        _ <- importRecord[TestAction](r1)
        res <- importRecord[TestAction](IdentifiedRecord(ref1, Record(hobby)))
      } yield res
      val (s2, res) = s(TestRepoState(Map.empty))
      assert(res.isRight, res)
      val r = s2.records(ref1)
      assert(r == IdentifiedRecord(ref1, Record(name, hobby)))
      assert(res.isRight)
    }
  }
}
