package local.nodens.linkmodel

/**
 *SequenceStuff is a trait for things that need to perform various manipulations on Seq objects.
 */
trait SequenceStuff {
  /**
   * Removes an item from a sequence.
   * @tparam A The type of elements in the seq.
   * @param i The index to remove
   * @param inSeq the sequence to work from
   * @return A new sequence with the object at i in inSeq removed
   */
  def removeFromSeq[A](i:Int, inSeq:Seq[A]):Seq[A] = if (i < 0) (inSeq) //jackassery
  else if (i == 0) (inSeq.tail) //removing first link
  else if (i >= inSeq.length - 1) (inSeq take i)//removing last link (or beyond)
  else {
    val (before, having) = inSeq.splitAt(i-1)
    before ++ having.tail
  }

  /**
   * Adds an element into a seq.
   * @tparam A The type of the element and the seq.
   * @param i Index to insert at.
   * @param obj Item to be inserted
   * @param inSeq Seq to insert into
   * @return New seq with obj at index i
   */
  def insertToSeq[A](i:Int, obj:A, inSeq:Seq[A]):Seq[A] = if (i <= 0) obj +: inSeq
  else if (i >= inSeq.length) inSeq :+ obj
  else {
    val (before, after) = inSeq.splitAt(i-1)
    before ++ (obj +: after)
  }
  
  /**
   * Moves an item around in a sequence.
   * @tparam A type of sequence
   * @param i Index of object to move
   * @param j Index to move object to
   * @param inSeq Sequence to start on.
   * @return A sequence such that oldSeq(i) = newSeq(j)
   */
  def moveInSeq[A](i:Int, j:Int, inSeq:Seq[A]):Seq[A] = if (i < 0 || i >= inSeq.length) inSeq
  else {
    val obj = inSeq(i)
    val newSeq = removeFromSeq(i, inSeq)
    if (j <= 0) obj +: newSeq
    else if (j >= newSeq.length) newSeq :+ obj
    else if (i >= j) insertToSeq(j, obj, newSeq) //i.e. it's moving forward
      else /*if (i < j)*/ insertToSeq(j-1, obj, newSeq) //moving backwards
  }

  /**
   * Allows a function to be applied to a specific item in a Seq.
   * @tparam A Type of items in the list, and the type the function works on.
   * @param i The index to operate on
   * @param inSeq Sequence to be updated
   * @param f The function to apply.
   * @return Updated sequence st. newSeq(i) = oldSeq(f(i))
   * @todo Fix type params to allow subtypes of A to be returned from f.
   */
  def updateSequenceItem[A](i:Int, inSeq:Seq[A])(f: A => A):Seq[A] = if (i < 0 || i >= inSeq.length) inSeq
  else inSeq.updated(i, f(inSeq(i)))

}
