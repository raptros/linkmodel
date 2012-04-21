import org.specs2._
import local.nodens.linkmodel._

class ManipulationSpec extends Specification {
  def is = "Testing the manipulations library" ^
  p^
  "ex1" ^
  "should get first section" ! ex1GetsTheRightThing ^
  p^
  "ex2" ^
  "should be changed" ! ex2Changes ^
  "should become the same as ex1" ! ex2BecomesEx1 ^
  p^
  "exampleBig" ^
  "should be constructed from scratch" ! constructExBig ^
  "should be constructed from scratch again" ! constructExBig2 ^
  "should be constructed from scratch w/ in place syntax" ! constructExBig3 ^
  p^
  "with removals" ^
  "ex1 should become ex2" ! convertEx1ToEx2 ^
  "exampleBig should become ex1" ! convertEx1ToEx2 ^
  p^
  "with reordering" ^
  "ex3 should become ex4" ! convertEx3ToEx4 ^
  "ex5 should become ex6" ! convertEx5ToEx6 ^
  end

  def ex1GetsTheRightThing = (ex1 / "one").current must_== ex1.sections.getOrElse("one", "fail")

  def ex2Changes = {
    val ex2mod = (ex2 / "one" +#@ "title0" #@# "url0")
    ex2mod must beSome.which(_ must beAnInstanceOf[Document])
  }

  def ex2BecomesEx1 = {
    val ex2mod:Document = (ex2 / "one" +#@ "title0" #@# "url0").get
    println(ex1 == ex2mod)
    println(ex1)
    println(ex2mod)
    ex1 must be_==(ex2mod)
  }
  
  def constructExBig = {
      val finalDoc = for {
        sectionOne <- Document() +/ "one" 
        sectionAgain <- sectionOne / "one" +/ "again"
        againLinkOne <- sectionAgain / "one" / "again" +#@ ("A Link" #@# "http://example.com/")
        againLinkTwo <- againLinkOne / "one" / "again" +#@  ("Another Link" #@# "http://www.example.com/")
        sectionTwo <- againLinkTwo +/ "two"
        buildSeq <- sectionTwo  / "two" +#@@ Seq(
          "p1" #@# "http://example.com/1",
          "p2" #@# "http://example.com/2")
        addP3 <- buildSeq / "two" +#@#@@(0, "p3" #@# "http://example.com/3")
        addP4 <- addP3 / "two" +#@#@@(0, "p4" #@# "http://example.com/4")
    } yield addP4
    finalDoc must beSome.which(_ must be_==(exampleBig))
  }

  def constructExBig2 = {
    val docsecs = (Document() +/ "one") / "one" +/ "again"
    val links = (docsecs / "one" / "again" +#@ ("A Link" #@# "http://example.com/")) / 
      "one" / "again" +#@ ("Another Link" #@# "http://www.example.com/")
    val two = (links +/ "two") / "two" +#@@ Seq(
      "p1" #@# "http://example.com/1",
      "p2" #@# "http://example.com/2")
    val finalDoc = (two / "two" +#@#@@(0, "p3" #@# "http://example.com/3")) /
      "two" +#@#@@(0, "p4" #@# "http://example.com/4")
    finalDoc must beSome.which(_ must be_==(exampleBig))
  }

  def constructExBig3 = {
    val finalDoc = ((Document() +/> "one" +/> "again" +#@>("A Link" #@# "http://example.com/") +#@("Another Link" #@# "http://www.example.com/"))
    +/> "two" +#@@>(Seq("p1" #@# "http://example.com/1", "p2" #@# "http://example.com/2")) 
    +#@#@@>(0, "p3" #@# "http://example.com/3")
    +#@#@@(0, "p4" #@# "http://example.com/4"))
    finalDoc must beSome.which(_ must be_==(exampleBig))
  }

  def convertEx1ToEx2 = {
    val finalDoc = ex1 / "one" -#@ 0
    finalDoc must beSome.which(_ must be_==(ex2))
  }
  def convertExBigToEx1 = {
    val finalDoc = (exampleBig -/ "two") / "one" -/> "again" +#@ ("title0" #@# "url0")
    finalDoc must beSome.which(_ must be_==(ex1))
  }

  def convertEx3ToEx4 = {
    val finalDoc = (ex3 / "one") ~#@#@@(0, 1, 0)
    finalDoc must beSome.which(_ must be_==(ex4))
  }

  def convertEx5ToEx6 = {
    val finalDoc = (ex5 / "one") ~#@(1, 0)
    finalDoc must beSome.which(_ must be_==(ex6))
  }



  val ex1 = Document(Section("one", "title0" #@# "url0"))
  val ex2 = Document(Section("one"))

  val ex3 = Document(
    Section("one",
      LinkSeq(
        Seq("title0" #@# "url0",
          "title1" #@# "url1"))))

  val ex4 = Document(Section("one", LinkSeq(Seq("title1" #@# "url1", "title0" #@# "url0"))))

  val ex5 = Document(Section("one", "title1" #@# "url1", "title0" #@# "url0"))
  val ex6 = Document(Section("one", "title0" #@# "url0", "title1" #@# "url1"))

      

  val exampleBig = Document(
    Section("one",
      Section("again",
        "A Link" #@# "http://example.com/",
        "Another Link" #@# "http://www.example.com/"
      )),
    Section("two",
      LinkSeq(
        Seq("p1" #@# "http://example.com/1",
        "p2" #@# "http://example.com/2",
        "p3" #@# "http://example.com/3",
        "p4" #@# "http://example.com/4")
      )))
}
