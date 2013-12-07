import rx.lang.scala.Observable
import rx.lang.scala.subjects.PublishSubject
import rx.subjects.PublishSubject
import scala.concurrent.Future
import suggestions.gui.WikipediaApi
val f = Observable("foo", "bar", "foo bar")
f.toBlockingObservable.foreach(println(_))




val fil = f.map(_.replace(' ', '_'))

fil.toBlockingObservable.foreach(println(_))





object mockApi extends WikipediaApi {
  def wikipediaSuggestion(term: String) = Future {
    if (term.head.isLetter) {
      for (suffix <- List(" (Computer Scientist)", " (Footballer)")) yield term + suffix
    } else {
      List(term)
    }
  }
  def wikipediaPage(term: String) = Future {
    "Title: " + term
  }
}








import mockApi._


val req = new PublishSubject[Int]()






val response = req.recovered



req.onNext(2)


req.onNext(3)




req.onError(4)





response.subscribe(x => println(x))




