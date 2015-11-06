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

val f = Words + Words.map(_.length)

// Training sentences
val sentence1 = "John loves Mary".split(' ')
val sentence2 = "Bill hates Mary so much".split(' ')

// Testing sentences
val sentence3 = "John died".split(' ')

// Get feature vectors for training sentences
val fv1 = f(sentence1)
val fv2 = f(sentence2)

// Get sparse vectors for training sentences
val realizer = new FeatureRealizer()
val sv1 = realizer(fv1)
val sv2 = realizer(fv2)

// Get sparse vectors for testing sentences
realizer.freeze()
val fv3 = f(sentence3)
val sv3 = realizer(fv3)

```
