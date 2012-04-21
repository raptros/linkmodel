import org.specs2._
import local.nodens.linkmodel._

class ModelSpec extends Specification {


  def is = "This specifies how the model system should work" ^
  p^
  "notation" ^
  "isAt must produce proper links" ! isAtNotation1 ^
  "isAt must produce proper links" ! isAtNotation2 ^
  p^
  "ex1 should render as expected" ! ex1ExpectedRender ^
  p^
  "exampleBig " ^
  "should be the correct render" ! exBigRenderCorrectly ^
  "should be parsable" ! exBigParsed ^
  "should be parsed correctly" ! exBigParsedCorrectly ^
  "should be reformable " ! exBigReform ^
  end


  def isAtNotation1 = ("Example 1" #@# "http://example.com/example1").renderXML must be_==/(Link("Example 1", "http://example.com/example1").renderXML)
  def isAtNotation2 = ("Example 1" #@# "http://example.com/example1") must beEqualTo (Link("Example 1", "http://example.com/example1"))

  val ex1 = Document(Section("first", "title0" #@# "url0"))
  val ex1render = <document><section name="first"><link title="title0" url="url0" /></section></document>

  def ex1ExpectedRender = ex1.renderXML must ==/(ex1render)

  def exBigRenderCorrectly = exampleBigParsed.renderXML must ==/(exampleBig)
  def exBigParsed = Document.parse(exampleBig) must beSome
  def exBigParsedCorrectly = Document.parse(exampleBig) must beSome.which(_ must be_==(exampleBigParsed))
  def exBigReform = exampleBig must ==/((Document.parse(exampleBig).get.renderXML))

  val exampleBig = 
  <document>
    <section name="one">
      <section name="again">
        <link title="A Link" url="http://example.com/"/>
        <link title="Another Link" url="http://www.example.com/"/>
      </section>
    </section>
    <section name="two">
      <sequence>
        <link title="p1" url="http://example.com/1"/>
        <link title="p2" url="http://example.com/2"/>
        <link title="p3" url="http://example.com/3"/>
        <link title="p4" url="http://example.com/4"/>
      </sequence>
    </section>
  </document>

  val exampleBigParsed = Document(
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
