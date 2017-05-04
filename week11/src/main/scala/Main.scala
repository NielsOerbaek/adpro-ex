// Advanced Programming. Andrzej Wasowski. IT University
// To execute this example, run "sbt run" or "sbt test" in the root dir of the project
// Spark needs not to be installed (sbt takes care of it)

import org.apache.spark.ml.feature.Tokenizer
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types._
import org.apache.spark.sql.Row
import org.apache.spark.ml.linalg.Vector
import org.apache.spark.ml.linalg.Vectors
import org.apache.spark.ml.classification.MultilayerPerceptronClassifier
import org.apache.spark.ml.evaluation.MulticlassClassificationEvaluator


object Main {

	type Embedding       = (String, List[Double])
	type ParsedReview    = (Integer, String, Double)

	org.apache.log4j.Logger getLogger "org"  setLevel (org.apache.log4j.Level.WARN)
	org.apache.log4j.Logger getLogger "akka" setLevel (org.apache.log4j.Level.WARN)
	val spark =  SparkSession.builder
		.appName ("Sentiment")
		.master  ("local[9]")
		.getOrCreate

  	import spark.implicits._

	val reviewSchema = StructType(Array(
			StructField ("reviewText", StringType, nullable=false),
			StructField ("overall",    DoubleType, nullable=false),
			StructField ("summary",    StringType, nullable=false)))

	// Read file and merge the text abd summary into a single text column

	def loadReviews (path: String): Dataset[ParsedReview] =
		spark
			.read
			.schema (reviewSchema)
			.json (path)
			.rdd
			.zipWithUniqueId
			.map[(Integer,String,Double)] { case (row,id) => (id.toInt, s"${row getString 2} ${row getString 0}", row getDouble 1) }
			.toDS
			.withColumnRenamed ("_1", "id" )
			.withColumnRenamed ("_2", "text")
			.withColumnRenamed ("_3", "overall")
			.as[ParsedReview]

  	// Load the GLoVe embeddings file
	def loadGlove (path: String): Dataset[Embedding] =
		spark
			.read
			.text (path)
			.map  { _ getString 0 split " " }
			.map  (r => (r.head, r.tail.toList.map (_.toDouble))) // yuck!
			.withColumnRenamed ("_1", "word" )
			.withColumnRenamed ("_2", "vec")
			.as[Embedding]

	def transformRating(outOf5: Double): Double = {
		if(outOf5 > 3.0) 2
		else if (outOf5 < 3.0) 0
		else 1
	}

	def getTestDatasets(n: Int, splits: Array[Dataset[Row]]): (Dataset[Row], Dataset[Row]) = {
		val index = n % 10
		val test = splits(index)
		val train = (splits.take(index-1) ++ splits.drop(index)).reduce(_.union(_))
		(train, test)
	}

	def getTokens(reviews: Dataset[(Integer, String, Double)]) = {
		val tokenizer = new Tokenizer().setInputCol("text").setOutputCol("words")
		val tokenized = tokenizer.transform(reviews)

		tokenized
			.flatMap( row =>
				row
					.getAs[Seq[String]]("words")
					.map( word => (row.getAs[Int]("id"), row.getAs[Double]("overall"), word))
			)
			.toDF("id", "overall", "token")
	}

	def getAverageVectorsForReviews(tokens: Dataset[Row], glove: Dataset[(String, List[Double])]) = {
		tokens
			.join(glove, tokens.col("token") === glove.col("word"))
			.drop("token")
			.groupByKey( row => row.getAs[Int]("id") )
			.mapGroups( (key, valueIterator) => {
				val values = valueIterator.toList
				val sumVector = values
									.map( row => row.getAs[Seq[Double]]("vec") )
									.reduce( (_, _).zipped.map(_ + _) )				// #functionalprogramming #blackmagic
				val avgVector = sumVector
									.map(_/values.length)
									.toArray
				val transformedRating = transformRating(values.head.getAs[Double]("overall"))
				(key, transformedRating, Vectors.dense(avgVector))
			})
			.toDF("id", "label", "features")
	}

	def nFoldCrossValidation(numSplits: Int, layers: Array[Int], iterations: Int, averageVectors: Dataset[Row]) = {
		val splits = averageVectors.randomSplit(Array.fill(numSplits)(1.0 / numSplits), seed = 1234L)

		val trainer = new MultilayerPerceptronClassifier()
			.setLayers(layers)
			.setBlockSize(128)
			.setSeed(1234L)
			.setMaxIter(iterations)



		def go(n: Int): Unit = {
			if(n>=0) {
				val (train, test) = getTestDatasets(n, splits)
				val result = trainer.fit(train).transform(test)
				val predictionAndLabels = result.select("prediction", "label")
				val evaluator = new MulticlassClassificationEvaluator()
					.setMetricName("accuracy")

				result.show
				println("Accuracy: " + evaluator.evaluate(predictionAndLabels))
				go(n-1)
			}
		}

		go(numSplits - 1);
	}

  	def main(args: Array[String]) = {
		val glove = loadGlove ("C:\\adpro-bigthings\\glove.6B.300d.txt")

		val reviews = loadReviews ("C:\\adpro-bigthings\\Pet_Supplies_5.json")
		val tokens = getTokens(reviews)
		val averageVectors = getAverageVectorsForReviews(tokens, glove)
		nFoldCrossValidation(10, Array[Int](300, 50, 5, 3), 100, averageVectors)

		spark.stop
  	}
}
