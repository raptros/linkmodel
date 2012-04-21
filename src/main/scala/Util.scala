package local.nodens.linkmodel

trait SequenceStuff {
  def removeFromSeq[A](i:Int, inSeq:Seq[A]):Seq[A] = if (i < 0) (inSeq) //jackassery
  else if (i == 0) (inSeq.tail) //removing first link
  else if (i >= inSeq.length - 1) (inSeq take i)//removing last link (or beyond)
  else {
    val (before, having) = inSeq.splitAt(i-1)
    before ++ having.tail
  }

  def insertToSeq[A](i:Int, obj:A, inSeq:Seq[A]):Seq[A] = if (i <= 0) obj +: inSeq
  else if (i >= inSeq.length) inSeq :+ obj
  else {
    val (before, after) = inSeq.splitAt(i-1)
    before ++ (obj +: after)
  }
  
  def moveInSeq[A](i:Int, j:Int, inSeq:Seq[A]):Seq[A] = if (i < 0 || i >= inSeq.length) inSeq
  else {
    val obj = inSeq(i)
    val newSeq = removeFromSeq(i, inSeq)
    if (j <= 0) obj +: newSeq
    else if (j >= newSeq.length) newSeq :+ obj
    else if (i >= j) insertToSeq(j, obj, newSeq) //i.e. it's moving forward
      else /*if (i < j)*/ insertToSeq(j-1, obj, newSeq) //moving backwards
  }

  def updateSequenceItem[A](i:Int, inSeq:Seq[A])(f: A => A):Seq[A] = if (i < 0 || i >= inSeq.length) inSeq
  else inSeq.updated(i, f(inSeq(i)))

}
