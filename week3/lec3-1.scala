sealed trait Binary
case class Digit (i: Boolean) extends Binary
case class Cons(head: Digit, tail: Binary) extends Binary

sealed trait Binary2
case object One extends Binary2
case object Zero extends Binary2
case class I (t: Binary2) extends Binary2
case class O (t: Binary2) extends Binary2