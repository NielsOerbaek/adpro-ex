import org.scalatest.{FreeSpec, Matchers, BeforeAndAfterAll}
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.Dataset

class SentimentSpec extends FreeSpec with Matchers with BeforeAndAfterAll {

  org.apache.log4j.Logger getLogger "org"  setLevel (org.apache.log4j.Level.WARN)
  org.apache.log4j.Logger getLogger "akka" setLevel (org.apache.log4j.Level.WARN)

  val spark =  SparkSession.builder
    .appName ("Sentiment")
    .master  ("local[12]")
    .getOrCreate

  override def afterAll = spark.stop

  import spark.implicits._

  val glove = Main.loadGlove("path/to/glove/file/in/your/filesystem")
  val reviews = Main.loadReviews("path/to/reviews/file/in/your/filesystem")

  "something to do with sentiments in texts or some such" - {
    "tokenization" - {
      "correctly splits a short sentence into meaningful tokens" in {
        ??? // Main.getTokens()
      }
    }

    "transformRating" - {
      "transforms a low rating to 0" in {
        Main.transformRating(1.2).should.equal(0)
      }

      "transforms a mediocre rating to 1" in {
        Main.transformRating(3.0).should.equal(1)
      }

      "transforms a good rating to 2" in {
        Main.transformRating(3.1).should.equal(2)
        Main.transformRating(4.0).should.equal(2)
      }
    }
    
    "splitting data sets" - {
      "pick the nth set and concat the others" in {
        ???
      }
    }

    "getting average vectors for reviews" - {
      "averages the vectors with a common review id" in {
        ???
      }
    }

    "n-fold cross-validation" - {
      "probably runs a perceptron..." taggedAs Ignore in {
        // this is quite hard to test :-( we have done manual tests of the output
      }
    }
  }

}
