package structures

case class Trie[A](value: Option[A], children: List[(Char, Trie[A])])

object Trie {

  def emptyTrie[A] = Trie[A](None, Nil)

  def add[A](s: String, a: A, t: Trie[A]): Trie[A] =
    if (s.isEmpty) Trie(Some(a), Nil)
    else {
      t.children.find(_._1 == s.head) match {
        case None => Trie(t.value, (s.head, add(s.tail, a, emptyTrie[A])) +: t.children)
        case Some(n) => Trie(t.value, (s.head, add(s.tail, a, n._2)) +: t.children.filterNot(_ == n))
      }
    }

  def findIn[A](s: String, t: Trie[A]): Option[A] =
    if (s.isEmpty) t.value
    else t.children.find(_._1 == s.head) match {
      case None => None
      case Some(a) => findIn(s.tail, a._2)
    }

  def main(args: Array[String]) {

    val a1 = add("A", 15, emptyTrie[Int])
    val a2 = add("to", 7, a1)
    val a3 = add("tea", 3, a2)
    val a4 = add("ted", 4, a3)
    val a5 = add("ten", 10, a4)
    val a6 = add("i", 11, a5)
    val a7 = add("in", 5, a6)
    val a8 = add("inn", 9, a7)
    
    println(findIn("ten", a8))

  }

}