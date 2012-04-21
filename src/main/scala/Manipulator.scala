package local.nodens.linkmodel

/**
 * The base Manipulator class. It defines all the manipulation operations.
 * @param current The section-bearing component in the model heirarchy that operations will apply to.
 * @param parents The heirarchy of Manipulators all the way to the root Document.
 * @note In many cases, failure isn't a result of bad parameters, but rather operation on an invalid path.
 */
abstract class Manipulator(val current:ContainsSections, val parents:List[Manipulator]) {
  /**
   * Accesses a subsection. Think unix filesystem heirarchy.
   * @param The name of the subsection to access.
   * @return Either a SectionManipulator for that subsection, if it exists, or a NoneManipulator if it doesn't.
   */
  def /(secName:String):Manipulator = current.sections.get(secName).map {
    c => new SectionManipulator(c, this::parents)
  }.getOrElse(new NoneManipulator(secName, this::parents))
  
  /**
   * Descend along a sections path.
   * @param A sequence of section names
   * @return Appropriate manipulator.
   */
  def /~(path:Seq[String]):Manipulator = path.foldLeft(this) (_ / _)
  
  /**
   * Get the base Document.
   */
  def getDoc:ContainsSections

  /**
   * Renders XML from the document.
   */
  def renderXML = getDoc.renderXML

  /**
   * Get a link from the section
   * @param i The index of the link.
   * @return Attempt to retrieve the link.
   */
  def #@(i:Int):Option[Link] = links.lift(i)

  /**
   * Get a link sequence.
   * @param i Which link sequence to get
   * @return Attempt to get link sequence
   */
  def #@@(i:Int):Option[LinkSeq] = linkSeqs.lift(i)

  /**
   * Get a link from a link sequence.
   * @param l Index of link.
   * @param ls Index of link sequence.
   * @return Attempt to get link from link sequence
   */
  def #@#@@(l:Int, ls:Int):Option[Link] = linkSeqs.lift(ls).flatMap(_.links.lift(l))

  /**
   * Attempt to update the structure (into a new structure).
   * @param newChild The child being updated.
   * @return The resulting Document.
   * @note Presence of Option notes the handling of failures.
   */
  def renewChild(newChild:Option[Section]):Option[Document]

  /**
   * Get whatever links are present.
   * @return A sequence of all present links.
   */
  def links:Seq[Link]

  /**
   * Get whatever link sequences are present.
   * @return A sequence of all present link sequences.
   */
  def linkSeqs:Seq[LinkSeq]

  /**
   * Add a link.
   * @param link A link object to append.
   * @return The resulting Document.
   */
  def +#@(link:Link):Option[Document]

  /**
   * Add a link sequence.
   * @param link A link sequence to append.
   * @return The resulting Document.
   */
  def +#@@(links:Seq[Link]):Option[Document]

  /** 
   * Add a link to a link sequence.
   * @param i The index of the link sequence to append to.
   * @param link A link object to append.
   * @return The resulting Document.
   */
  def +#@#@@(i:Int, link:Link):Option[Document]

  /**
   * Add a section.
   * @param name The name of a section to add.
   * @return The resulting Document.
   * @note Use a new name unless you want to overwrite a section!
   */
  def +/(name:String):Option[Document] = renewChild(Some(Section(name)))

  /**
   * Remove a section.
   * @param name Name of section to remove.
   * @return Resulting Document.
   */
  def -/(name:String):Option[Document] 

  /**
   * Remove a link.
   * @param i Index of link to remove.
   * @return Resulting Document.
   */
  def -#@(i:Int):Option[Document]

  /** 
   * Remove a linkSeq.
   * @param i Index of link sequence to remove.
   * @return Resulting Document.
   */
  def -#@@(i:Int):Option[Document]

  /** 
   * Remove link l from linkseq ls
   * @param l Index of link to remove.
   * @param ls Index of link sequence to remove from.
   * @return Resulting Document.
   */
  def -#@#@@(l:Int, ls:Int):Option[Document]

  /**
   * Move link at position iOld to position iNew.
   * @param iOld Index of link to be moved.
   * @param iNew The destination index.
   * @return Resulting Document.
   */
  def ~#@(iOld:Int, iNew:Int):Option[Document]

  /**
   * Move link sequence at position iOld to position iNew.
   * @param iOld Index of link sequence to be moved.
   * @param iNew The destination index.
   * @return Resulting Document.
   */
  def ~#@@(iOld:Int, iNew:Int):Option[Document]

  /** 
   * Move link at position iOld in linksequence ls to position iNew
   * @param iOld Index of link to be moved.
   * @param iNew Resulting index of link in link sequence.
   * @param ls Index of link sequence that link is being moved in.
   * @return Resulting Document.
   */
  def ~#@#@@(iOld:Int, iNew:Int, ls:Int):Option[Document]
  
  /**
   * Obtain the path with root first.
   * @param A list of things that are below
   * @return The full path from root to initial caller.
   */
  def prependPath(below:List[String]):List[String]

  /**
   * The path with root first, and this last.
   */
  val path:List[String] = prependPath(List[String]())

  /**
   * The path as a string.
   */
  val pathStr = path reduceLeft (_ +"/"+ _)
}


