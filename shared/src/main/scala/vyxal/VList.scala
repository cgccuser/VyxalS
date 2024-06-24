package vyxal

import collection.immutable.SeqOps
import collection.mutable
import scala.collection.SpecificIterableFactory

/** A Vyxal list. It could represent a completely evaluated list, a finite list
  * that is in the process of being evaluated, or an infinite list.
  * @param lst
  *   The Scala list actually holdings this VList's elements
  */
class VList(val lst: Seq[VAny]) extends Seq[VAny], SeqOps[VAny, Seq, VList] {
  def foldl(start: VAny)(op: (VAny, VAny) => VAny): VAny =
    this.fold(start)(op)

  def output()(using ctx: Context): Unit = {
    ctx.print("⟨")
    var first = true
    for (elem <- this) {
      if (first) {
        first = false
      } else {
        print(" | ")
      }
      Helpers.vyPrint(elem)
    }
    ctx.print("⟩")
  }

  def vmap(f: Monad)(using Context): VList = new VList(lst.map(f(_)))

  /** Zip two VLists together with a function. If one is longer than the other,
    * keep the longer one's elements as-is.
    */
  def zipWith(other: VList)(f: (VAny, VAny) => VAny): VList =
    new VList(lst.lazyZip(other.lst).map(f(_, _)))

  /** Get the element at index `ind`
    */
  override def apply(ind: Int): VAny = lst(ind)

  override def iterator: Iterator[VAny] = lst.iterator

  /** Get the length of this `VList`. A word of caution: this fully evaluates
    * the list, meaning that it won't work with infinite lists.
    */
  override def length: Int = lst.length

  override protected def fromSpecific(coll: IterableOnce[VAny]): VList =
    VList.fromSpecific(coll)
  override protected def newSpecificBuilder
      : collection.mutable.Builder[VAny, VList] =
    VList.newBuilder
  override def empty: VList = VList.empty
}

object VList extends SpecificIterableFactory[VAny, VList] {

  /** Zip multiple VLists together with a function.
    */
  def zipMulti(lists: VList*)(f: Seq[VAny] => VAny): VList = {
    // val its = lists.map(_.iterator)

    // VList(
    //   Seq.empty,
    //   new scala.collection.Iterator[VAny] {
    //     def hasNext: Boolean = its.exists(_.hasNext)
    //     def next: VAny = f(its.collect { case it if it.hasNext => it.next })
    //   }
    // )
    ???
  }

  /** This lets us pattern match on `VList`s, silly as the implementation may
    * be.
    */
  def unapplySeq(vlist: VList): Seq[VAny] = vlist.lst

  def empty: VList = new VList(Seq.empty)

  def newBuilder: mutable.Builder[VAny, VList] =
    mutable.ArrayBuffer
      .newBuilder[VAny]
      .mapResult(elems => new VList(elems.toSeq))

  def fromSpecific(it: IterableOnce[VAny]): VList = new VList(it.iterator.toSeq)
}
