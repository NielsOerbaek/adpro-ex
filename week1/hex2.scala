object fib {
	def fibo(n: Int) : Int = {
		@annotation.tailrec
		def go(i: Int, current: Int, last: Int) : Int = {
			if(0 <= i) {
				val next = current + last
				go(i-1, next, current)
			}
			else current
		}
		go(n,1,0)
	}

	def main(args: Array[String]): Unit = 
		println(fibo(100))
}