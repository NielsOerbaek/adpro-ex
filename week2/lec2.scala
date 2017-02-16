object lec2 {
	def f(n: Int) : Int = {
		def go(i: Int) : Unit = {
			if(i>=0) {
				println(i)
				go(i-2)
			}
		}
		go(n*2)
		n
	}
}

/* HERE ARE THE NOTES 

	Kig på slide om traits for mange smarte tips.

	I scala deconstruere man ofte classes til en række traits der indeholder
	de relevante funktioner og data. Composable.

	Man kan give objecter ekstra traits når man instantierer dem. 
	(Injecting traits)

	s-metoden: s"My name is ${f.name}"
	Den er meget smart!

	Når man laver en case class behøver man ikke at bruge "new" når man 
	instansierer et nyt opject, fordi den indbyggede "apply"-metode
*/