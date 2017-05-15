import org.scalatest.{BeforeAndAfterAll, FreeSpec, Matchers}
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

  val glove = Main.loadGlove("C:\\adpro-bigthings\\glove.6B.300d.txt")
  val reviews = Main.loadReviews("C:\\adpro-bigthings\\Amazon_Instant_Video_5.json")

  "something to do with sentiments in texts or some such" - {
    "tokenization" - {
      "correctly splits a short sentence into meaningful tokens" in {
        val ds = Seq((1,"This Sentence Has Five Words", 0.0))
          .toDS
          .withColumnRenamed ("_1", "id" )
          .withColumnRenamed ("_2", "text")
          .withColumnRenamed ("_3", "overall")
          .as[Main.ParsedReview]

        val tokens = Main.getTokens(ds)
          .select("token")
          .rdd
          .map(r => r(0))
          .collect()
          .toList

        assert(tokens == List("this", "sentence", "has", "five", "words"))
      }
    }

    "transformRating" - {
      "transforms a low rating to 0" in {
        assert(Main.transformRating(1.2) == 0)
      }

      "transforms a mediocre rating to 1" in {
        assert(Main.transformRating(3.0) == 1)
      }

      "transforms a good rating to 2" in {
        assert(Main.transformRating(3.1) == 2)
        assert(Main.transformRating(4.0) == 2)
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
      "probably runs a perceptron..." ignore {
        // this is quite hard to test :-( we have done manual tests of the output
      }
    }
  }

}
