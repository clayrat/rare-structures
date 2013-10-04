package structures

import scala.collection.immutable.BitSet
import scala.util.hashing.{ MurmurHash3 => MH }
import scala.util.Random

case class BloomFilter(m: Int, k: Int, bset: BitSet, hashes: List[String => Int]) {
  def runHashes(s: String) = hashes.map { f => f(s) }
}

object BloomFilter {

  def emptyFilter(size: Int, k: Int) = {
    val hashes = List.fill(k)(Random.nextInt) map { seed =>
      { s: String =>
        Math.abs(MH.bytesHash(s.getBytes, seed)) % size
      }
    }
    BloomFilter(size, k, BitSet.empty, hashes)
  }

  def add(s: String, b: BloomFilter): BloomFilter = 
    BloomFilter(b.m, b.k, b.bset ++ BitSet(b.runHashes(s): _*), b.hashes)

  def query(s: String, b: BloomFilter): Boolean =
    b.runHashes(s).map(b.bset.contains).reduce(_ && _)

  def main(args: Array[String]) {

    val e = emptyFilter(100, 5)
    val a1 = add("shit", e)
    val a2 = add("fuck", a1)
    val a3 = add("piss", a2)

    println(query("shit", a3))
    println(query("nice", a3))
  }

}