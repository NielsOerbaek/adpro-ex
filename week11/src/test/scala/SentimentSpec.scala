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

  //disabled for now -- we didn't get to write tests for these :-(
  //val glove = Main.loadGlove("path/to/glove/file/in/your/filesystem")
  //val reviews = Main.loadReviews("path/to/reviews/file/in/your/filesystem")

  "something to do with sentiments in texts or some such" - {
    "tokenization" - {
      "correctly splits a short sentence into meaningful tokens" in {
        ??? // Main.getTokens()
      }
    }

    "transformRating" - {
      "transforms a low rating to 0" in {
        assert(Main.transformRating(1.4) == 0)
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
      "pick the nth set and concat the others" in { //this would be good to make a property test
        val ds1 = spark.createDataset(Seq((1,2),(1,2))).toDF("i1","i2")
        val ds2 = spark.createDataset(Seq((2,3),(2,3))).toDF("i1","i2")
        val ds3 = spark.createDataset(Seq((3,4),(3,4))).toDF("i1","i2")
        val (train,test) = Main.getTestDatasets(1, Array(ds1,ds2,ds3))
        val expectedTrain = ds1.union(ds3).collect
        assert(train.collect.map(row => expectedTrain.map(row2 => row2 == row).reduce((x,y) => x || y)).reduce((x,y) => x && y))
        val expectedTest = ds2.collect
        assert(test.collect.map(row => expectedTest.map(row2 => row2 == row).reduce((x,y) => x || y)).reduce((x,y) => x && y))
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
