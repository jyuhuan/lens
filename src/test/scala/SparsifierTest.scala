import me.yuhuan.lens._

/**
 * Created by Yuhuan Jiang (jyuhuan@gmail.com) on 11/21/15.
 */
object SparsifierTest extends App {

  val trainFvs = Seq[Seq[Feature[String]]](
    Seq[Feature[String]](
      Feature.Cat("word", "John"),
      Feature.Cat("word", "Mary"),
      Feature.Cat("word", "killed"),
      Feature.Cat("pos", "NN"),
      Feature.Cat("pos", "JJ"),
      Feature.Cat("pos", "VBN"),
      Feature("dist", "()", 6)
    )
  )

  val testFvs = Seq[Seq[Feature[String]]](
    Seq[Feature[String]](
      Feature.Cat("word", "John"),
      Feature.Cat("word", "Bill"),
      Feature.Cat("word", "loved"),
      Feature.Cat("pos", "VBZ"),
      Feature.Cat("pos", "JJ"),
      Feature.Cat("pos", "NN"),
      Feature("dist", "()", 5)
    )
  )

  val sparsifier = new Sparsifier

  val trainSvs = trainFvs.map(x => sparsifier(x))

  sparsifier.freeze()

  val testSvs = testFvs.map(x => sparsifier(x))

  val bp = 0

  //val sparse

}
