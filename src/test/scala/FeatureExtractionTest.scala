import me.yuhuan.lens._

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
object FeatureExtractionTest extends App {
  type Sentence = Seq[String]

  object WordsBetween extends Featurizer[(Sentence, Int, Int), String] {
    def name = "Words"
    def featurize(v: (Sentence, Int, Int)): FeatureGroup[String] = {
      val (s, i, j) = v
      FeatureGroup("Words")(s.slice(i, j).map(w ⇒ (w, 1.0)))
    }
  }

  object PathBetween extends Featurizer[(Sentence, Int, Int), String] {
    def name = "Path"
    def featurize(v: (Sentence, Int, Int)): FeatureGroup[String] = {
      val (s, i, j) = v
      val path = if (j - i > 4) "NN↑NP↑SBAR↑S↓VP↓VBN" else "VBN↑VP↑S↓NP↓DT"
      FeatureGroup("Path")(Seq((path, 1.0)))
    }
  }

  object Length extends Featurizer[String, Unit] {
    def name = "Length"
    def featurize(x: String): FeatureGroup[Unit] = FeatureGroup("Length")(Seq(((), x.length.toDouble)))
  }

  val sentence1 = "John hates that Mary killed Taro 's dog .".split(' ')
  val sentence2 = "Bill likes that Mary might have accidentally killed Taro 's dog .".split(' ')
  val sentence3 = "Yoko likes that Alice might kill Bob 's dog .".split(' ')

  val f = FeatureExtractor(WordsBetween, PathBetween, PathBetween.catToNum("PathLengthBetween")(_.split("""(↑|↓)""").length))
//  val f = FeatureExtractor(WordsBetween, (WordsBetween andThen Length))

  val fv1 = f((sentence1, 3, 5))
  val fv2 = f((sentence2, 3, 8))
  val fv3 = f((sentence3, 3, 6))

  val realizer = new FeatureRealizer()
  val sv1 = realizer.realize(fv1)
  val sv2 = realizer.realize(fv2)

  realizer.freeze()
  val sv3 = realizer.realize(fv3)

  val bp = 0
}
