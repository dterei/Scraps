/**
 * Playing with DB access in Scala. Try out the following:
 *  - Java DataManager (JDBC)
 *  - Java DataSource (JDBC)
 *  - Scala Slick3 ORM
 *
 *  References:
 *   - http://slick.lightbend.com/doc/3.0.0/gettingstarted.html
 *   - http://slick.lightbend.com/doc/3.2.1/database.html
 *   - http://slick.lightbend.com/doc/3.2.1/api/index.html#slick.jdbc.JdbcBackend$DatabaseDef
 *   - https://dev.mysql.com/doc/connector-j/5.1/en/connector-j-reference-configuration-properties.html
 *   - https://medium.com/@takezoe/database-access-libraries-in-scala-7aa7590aa3db
 *   - https://www.playframework.com/documentation/2.0.2/api/scala/play/api/db/DB$.html
 *   - https://www.playframework.com/documentation/2.5.x/PlaySlick
 */
package example

import com.mysql.cj.jdbc.Driver
import com.mysql.cj.jdbc.MysqlConnectionPoolDataSource
import com.mysql.cj.jdbc.MysqlDataSource
import java.sql.Connection
import java.sql.DriverManager
import javax.sql.DataSource
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import slick.driver.MySQLDriver.api._

// Simple table for playing with
class T1(tag: Tag) extends Table[(Int, String, Int)](tag, "t1") {
  def id = column[Int]("id", O.PrimaryKey)
  def name = column[String]("name")
  def age = column[Int]("age")

  def * = (id, name, age)
}

class Test {
  val driver = "com.mysql.jdbc.Driver"
  // val url = "jdbc:mysql://address=(protocol=tcp)(host=localhost)" ++
  //           "(user=root)(password=spark)/rubrik_dev?useSSL=false"
  // val url = "jdbc:mysql://localhost/rubrik_dev?useSSL=false"
  val url = "jdbc:mysql://root:spark@localhost:3306/rubrik_dev?useSSL=false"
  val user = "root"
  val pass = "spark"

  val t1s = TableQuery[T1]

  def test() {
    testDriverManager()
    testDataSource()
    testSlick()
  }

  // DataManager is the standard and oldest way to access databases in Java.
  def testDriverManager() {
    println("Testing DataManager...")
    var conn:Connection = null
    try {
        Class.forName(driver)
        conn = DriverManager.getConnection(url, user, pass)
        testConnection(conn)    
    } catch {
      case e : Throwable => e.printStackTrace
    } finally {
      if (conn != null) {
        conn.close()
      }
    }
    println("")
  }

  // DataSource is the newer and preferred way to create DB connections in
  // Java. It improves over DataManager in providing a factory object for
  // creating connections that supports both pooling and distributed
  // transactions, but requires driver support for both.
  def testDataSource() {
    println("Testing DataSource...")
    var conn:Connection = null
    try {
      // val ds = new MysqlDataSource()
      val ds = new MysqlConnectionPoolDataSource()
      ds.setServerName("test-mysql")
      ds.setURL(url)
      ds.setUser(user)
      ds.setPassword(pass)

      conn = ds.getConnection()
      testConnection(conn)    
    } catch {
      case e : Throwable => e.printStackTrace
    } finally {
      if (conn != null) {
        conn.close()
      }
    }
    println("")
  }

  def testConnection(conn: Connection) {
    val stmt = conn.createStatement()
    val rs = stmt.executeQuery("SELECT name from t1")
    while (rs.next()) {
      println(rs.getString("name"))
    }
  }

  def testSlick() {
    // create from JDBC URL - Uses DriverManager
    println("Testing Slick-JDBC...")
    val db1 = Database.forURL(url, driver=driver)

    try {
      val q = db1.run(t1s.result).map(_.foreach {
        case (id, name, age) =>
          println(name)
      })
      Await.result(q, 10 seconds)
    } catch {
      case e : Throwable => e.printStackTrace
    } finally {
      if (db1 != null) {
        db1.close()
      }
    }
    println("")

    // create from DataSource
    println("Testing Slick-DataSource...")
    val dsMySQL = new MysqlDataSource()
    dsMySQL.setURL(url)
    val ds: DataSource = dsMySQL
    val db2 = Database.forDataSource(ds, None)
    try {
      val q = db2.run(t1s.result).map(_.foreach {
        case (id, name, age) =>
          println(name)
      })
      Await.result(q, 10 seconds)
    } catch {
      case e : Throwable => e.printStackTrace
    } finally {
      if (db1 != null) {
        db1.close()
      }
    }
    println("")
  }
}
