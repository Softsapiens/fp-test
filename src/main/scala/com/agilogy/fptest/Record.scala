package com.agilogy.fptest

sealed trait RecordValue

object RecordValue {

  case class Text(v: String) extends RecordValue

  case class Id(value: String) extends RecordValue

}

case class Record(properties: Map[String, RecordValue]) {

  def set(name: String, value: RecordValue): Record = this.copy(properties ++ Map(name -> value))

  def merge(record: Record): Record = this.copy(properties = this.properties ++ record.properties)
}

object Record {
  def apply(properties: (String, RecordValue)*): Record = Record(properties.toMap)
}


case class IdentifiedRecord(self: RecordValue.Id, body: Record) {
  def set(name: String, value: RecordValue): IdentifiedRecord = this.copy(body = body.set(name, value))

  def merge(record: Record): IdentifiedRecord = this.copy(body = body.merge(record))

}