# Lens

A feature extraction tool.

Add to your project by:

```scala
resolvers += Resolver.sonatypeRepo("snapshots")
libraryDependencies += "me.yuhuan" %% "lense" % "0.0.0-SNAPSHOT"
```

## Example: Extracting Features

```scala
object Words extends Featurizer[Sentence, String] {
  def featurize(s: Sentence): FeatureVector[String] = {
    FeatureVector(s.map(w ⇒ Feature.Cat("word", w))
  }
}

object Length extends Featurizer[Sentence, Int] {
  def featurize(s: Sentence): FeatureVector[Int] = {
    FeatureVector(Seq(s.length))
  }
}

val f = Words + Length

// Training sentences
val sentence1 = "John loves Mary".split(' ')
val sentence2 = "Bill hates Mary so so much".split(' ')

// Testing sentences
val sentence3 = "John died".split(' ')

// Get feature vectors for training sentences
val fv1 = f(sentence1) 
val fv2 = f(sentence2) 
// fv1 = word=John:1; word=loves:1; word=Mary:1; length=():3
// fv2 = word=Bill:1; word=hates:1; word=Mary:1; word=so:2; word=much:1; length=():6



// Get sparse vectors for training sentences
val realizer = new FeatureRealizer()
val sv1 = realizer(fv1)
val sv2 = realizer(fv2)

// Get sparse vectors for testing sentences
realizer.freeze()
val fv3 = f(sentence3)
val sv3 = realizer(fv3)

```
