package react
import java.util.Arrays

/**
 * A slightly specialized priority queue.
 */
abstract class PriorityQueue[A >: Null: Manifest] {

  private var array = new Array[A](16)
  private var size0 = 1 // array(0) unused

  protected def ensureSize(n: Int) {
    if (n > array.length) {
      var newsize = array.length * 2
      while (n > newsize)
        newsize = newsize * 2

      val newar = new Array[A](newsize)
      compat.Platform.arraycopy(array, 0, newar, 0, size0)
      array = newar
    }
  }

  protected def swap(a: Int, b: Int) {
    val h = array(a)
    array(a) = array(b)
    array(b) = h
  }

  protected def fixUp(m: Int): Unit = {
    val as = array
    var k = m
    while (k > 1 && priority(as(k / 2)) > priority(as(k))) {
      swap(k, k / 2)
      k = k / 2
    }
  }

  protected def fixDown(m: Int, n: Int): Unit = {
    val as = array
    var k = m
    while (n >= 2 * k) {
      var j = 2 * k
      if (j < n && priority(as(j)) > priority(as(j + 1)))
        j += 1
      if (priority(as(k)) <= priority(as(j)))
        return
      else {
        swap(k, j)
        k = j
      }
    }
  }

  def +=(elem: A) {
    ensureSize(size0 * 3/2 + 1)
    array(size0) = elem
    fixUp(size0)
    size0 += 1
  }

  def dequeue(): A = if (size0 > 1) {
    size0 -= 1
    val res = array(1)
    array(1) = array(size0)
    array(size0) = null // need to clear, don't want to keep nodes alive longer than necessary
    fixDown(1, size0 - 1)
    res
  } else
    throw new NoSuchElementException("no element to remove from heap")

  def priority(a: A): Int

  def isEmpty = size0 == 1
  def clear() { size0 = 1 }

  /*private def remove(elem: A) {
    var i = 1
    while(i < size0) {
      if (array(i) == elem) {
        size0 -= 1
        array(i) = null
        if(i == size0) {
          return
        } else {
          array(i) = null
        }
        compat.Platform.arraycopy(array, i+1, array, i, size0-i)
      }
      return
    }
  }*/

  private def indexOf(elem: A): Int = {
    var i = 1
    while(i < size0) {
      if (array(i) eq elem) return i
      i += 1
    }
    -1
  }

  def reinsert(elem: A) {
    //remove(elem)
    val idx = indexOf(elem)
    if(idx == -1) this += elem
    else {
      fixDown(idx, size0-1)
    }

  }

  override def toString() =
    "PrioQueue" + array.mkString("[", ",", "]")
}

