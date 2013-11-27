import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import nodescala.FutureCompanionOps

val elem = Future.delay(5 seconds)

val r = Await.result(elem, 3 seconds)









































println(r)
println("done")
Thread.sleep((10 seconds).toMillis)


