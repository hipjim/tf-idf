object TfIdf {

  def tfidf(word: String, document: String, documentList: Seq[String]) =
    tf(word, document) * idf(word, documentList)

  private[this] def tf(word: String, document: String) = {
    val documentTokens = document.toLowerCase.split(" +")
    val freq = (word: String, document: String) => {
      val documentWordFrequency = documentTokens.groupBy(e => e).map(e => e._1 -> e._2.length)
      documentWordFrequency.getOrElse(word.toLowerCase(), 0)
    }

    freq(word, document).toDouble / documentTokens.size
  }

  private[this] def idf(word: String, documentList: Seq[String]) = {
    val numDocsContaining = (word: String, documentList: Seq[String]) => documentList.foldLeft(0) {
      (acc, e) => if (e.toLowerCase.contains(word)) acc + 1 else acc
    }

    import java.lang.Math
    Math.log(documentList.length) / numDocsContaining(word, documentList)
  }

}

object Runner extends App {

  import TfIdf._

  val documentList = "some text" :: "some other text" :: "dummy test text" :: Nil

  for (document <- documentList) {
    println(tfidf("text", document, documentList))
  }
}