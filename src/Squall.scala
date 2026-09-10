package net.ivoah.squall

import java.sql.*
import java.time.LocalDate
import scala.util.Using

type QueryParam = Int | Double | String | Boolean | scala.Array[Byte] | LocalDate | DbEnum

class Connector(url: String, credentials: Option[(String, String)] = None) {
  def this(url: String, credentials: (String, String)) = this(url, Some(credentials))

  private var _connection: Connection = credentials.map(c => DriverManager.getConnection(url, c._1, c._2)).getOrElse(DriverManager.getConnection(url))

  def connection: Connection = {
    if (!_connection.isValid(5)) _connection = credentials.map(c => DriverManager.getConnection(url, c._1, c._2)).getOrElse(DriverManager.getConnection(url))
    _connection
  }

  def close(): Unit = _connection.close()
}

trait DbEnum(val dbType: String) {
  override def toString: String
}

// case class RawQuery(query: String) {
//   override def toString: String = query
//   def +(other: RawQuery): RawQuery = RawQuery(query + other.query)
//   def +(other: String): RawQuery = RawQuery(query + other)
// }

case class Query(sql: String, params: Seq[QueryParam]) {
  private def buildStatement(using db: Connector): PreparedStatement = {
    val stmt = db.connection.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)
    
    for ((param, i) <- params.zipWithIndex) param match {
      case int: Int                 => stmt.setInt(i + 1, int)
      case dbl: Double              => stmt.setDouble(i + 1, dbl)
      case str: String              => stmt.setString(i + 1, str)
      case bool: Boolean            => stmt.setBoolean(i + 1, bool)
      case date: LocalDate          => stmt.setDate(i + 1, java.sql.Date.valueOf(date))
      case _enum: DbEnum            => stmt.setString(i + 1, _enum.toString)
      case bytes: scala.Array[Byte] => stmt.setBytes(i + 1, bytes)
    }
    
    stmt
  }

  def query[T](fn: ResultSet => T)(using Connector): Seq[T] = {
    Using.resource(buildStatement) { stmt =>
      Iterator.unfold(stmt.executeQuery()) { results =>
        if (results.next()) Some(fn(results), results)
        else None
      }.toSeq
    }
  }

  def execute()(using Connector): Unit = Using.resource(buildStatement)(_.execute())

  def update()(using Connector): Int = Using.resource(buildStatement)(_.executeUpdate())
  def updateGetKey()(using Connector): Int = Using.resource(buildStatement) { stmt =>
    stmt.executeUpdate()
    val results = stmt.getGeneratedKeys
    results.next()
    results.getInt(1)
  }

  def +(o: Query): Query = Query(sql + o.sql, params ++ o.params)
}

extension (sc: StringContext) {
  def sql(params: (QueryParam | Query)*): Query = {
    Query(
      sc.parts.zipAll(params, "", "".sql).map {
        case (part, param) if param == null => s"${part}NULL"
        case (part, _enum: DbEnum)          => s"$part?::${_enum.dbType}"
        case (part, _: QueryParam)          => s"$part?"
        case (part, q: Query)               => s"$part${q.sql}"
      }.mkString,
      params.flatMap {
        case p: QueryParam if p != null => Some(p)
        case q: Query => q.params
        case _ => None
      }
    )
  }
}

extension (str: String) {
	def sql: Query = Query(str, Seq())
}

extension (rs: ResultSet) {
  def getIntOption(col: String): Option[Int] = {
    val v = rs.getInt(col)
    if (rs.wasNull) None else Some(v)
  }

  def getDoubleOption(col: String): Option[Double] = {
    val v = rs.getDouble(col)
    if (rs.wasNull) None else Some(v)
  }

  def getStringOption(col: String): Option[String] = {
    val v = rs.getString(col)
    if (rs.wasNull) None else Some(v)
  }
}
