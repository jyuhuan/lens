import me.yuhuan.feature._

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
object FeatureExtractionTest extends App {
  type Sentence = Seq[String]

  object WordsBetween extends Featurizer[(Sentence, Int, Int), String] {
    def featurize(v: (Sentence, Int, Int)): FeatureVector[String] = {
      val (s, i, j) = v
      FeatureVector(s.slice(i, j).map(w ⇒ Feature.Cat("WordsBetween", w)))
    }
  }

  object PathBetween extends Featurizer[(Sentence, Int, Int), String] {
    def featurize(v: (Sentence, Int, Int)): FeatureVector[String] = {
      val (s, i, j) = v
      val path = if (j - i > 4) "NN↑NP↑SBAR↑S↓VP↓VBN" else "VBN↑VP↑S↓NP↓DT"
      FeatureVector(Seq(Feature.Cat("PathBetween", path)))
    }
  }


  val sentence1 = "John hates that Mary killed Taro 's dog .".split(' ')
  val sentence2 = "Bill likes that Mary might have accidentally killed Taro 's dog .".split(' ')

  val f = WordsBetween ++ PathBetween ++ PathBetween.catToNum("PathLengthBetween")(_.split("""(↑|↓)""").length)

  val fv1 = f((sentence1, 3, 5))
  val fv2 = f((sentence1, 3, 8))

  val realizer = new FeatureRealizer()

  val sv1 = realizer.realize(fv1)
  val sv2 = realizer.realize(fv2)

  val bp = 0

}
