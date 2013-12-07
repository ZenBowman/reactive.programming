import rx.lang.scala.Observable

val f = Observable("foo", "bar", "foo bar")
f.toBlockingObservable.foreach(println(_))


val fil = f.map(_.replace(' ', '_'))

fil.toBlockingObservable.foreach(println(_))



