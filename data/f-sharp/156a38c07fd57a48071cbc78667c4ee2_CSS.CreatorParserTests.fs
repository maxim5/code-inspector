module WingBeats.CSS.CreatorTests

open System
open Xunit
open WingBeats.Xml.CreatorParser

type TestAttribute = FactAttribute
type cpe = CreatorParseException

let isEqualTo expected actual =
    Assert.Equal(expected, actual)

let throws<'a when 'a :> exn> f =
    Assert.Throws<'a>(Assert.ThrowsDelegate(f))

[<Test>]
let ``<Parse a tag name>`` () =
    parseCreator "div"
    |> isEqualTo [Creator[Tag "div"]]

[<Test>]
let ``<Parse a class name>`` () =
    parseCreator ".megaClass"
    |> isEqualTo [Creator[Classes["megaClass"]]]

[<Test>]
let ``<Parse an ID>`` () =
    parseCreator "#thing"
    |> isEqualTo [Creator[Id "thing"]]

[<Test>]
let ``<Parse an attribute>`` () =
    parseCreator "[href='http://wingbeats.codeplex.com/']"
    |> isEqualTo [Creator[Attributes[("href", "http://wingbeats.codeplex.com/")]]]

[<Test>]
let ``<Parse a tag with two class names>`` () =
    parseCreator "div.shocking.amazing"
    |> isEqualTo [Creator[Tag "div"; Classes["shocking";"amazing"]]]

[<Test>]
let ``<Parse a tag with an ID>`` () =
    parseCreator "div#thrilling"
    |> isEqualTo [Creator[Tag "div"; Id "thrilling"]]

[<Test>]
let ``<Parse a tag with two attributes>`` () =
    parseCreator "img[src='foo.png';alt='a photo']"
    |> isEqualTo [Creator[
                    Tag "img"; 
                    Attributes[("src", "foo.png");
                             ("alt", "a photo");] ]]

[<Test>]
let ``<Parse a tag with an ID and two classes>`` () =
    parseCreator "img#foo.class1.class2"
    |> isEqualTo [Creator[ 
                    Tag "img"; Id "foo";
                    Classes["class1"; "class2"] ]]

[<Test>]
let ``<Parse_an_ID_and_a_class>`` () =
    parseCreator "#id.class"
    |> isEqualTo [Creator[ Id "id"; Classes["class"] ]]

[<Test>]
let ``<Parse two classes and two attributes>`` () =
    parseCreator ".class1.class2[attr1='foo';attr2='bar']"
    |> isEqualTo [Creator[ 
                    Classes["class1"; "class2"];
                    Attributes[("attr1", "foo"); ("attr2", "bar")] ]]

[<Test>]
let ``<Parse a tag id classes and attributes>`` () =
    parseCreator "div#thrilling.shocking.amazing[align='center';title=quite the title]"
    |> isEqualTo [Creator[
                    Tag "div"; Id "thrilling";
                    Classes["shocking"; "amazing"];
                    Attributes[("align", "center"); ("title", "quite the title")] ]]
[<Test>]
let ``<Parse a complex out of order creator>`` () =
    parseCreator "div.class[attr='val']#id.class2"
    |> isEqualTo [Creator[ 
                    Tag "div"; Classes["class"];
                    Attributes[("attr", "val")]; Id "id";
                    Classes["class2"] ]]

[<Test>]
let ``<Parse a creator with an invalid space in it>`` () =
    let ex = throws<cpe> (fun () -> parseCreator "div#id .class" |> ignore)
    Assert.Contains("Error in Ln: 1 Col: 8", ex.Message)

[<Test>]
let ``<Parse a creator with an out of order tag name>`` () =
    let ex = throws<cpe> (fun () -> parseCreator "#id.class[attr='val']div" |> ignore)
    Assert.Contains("Error in Ln: 1 Col: 22", ex.Message)

[<Test>]
let ``<Parse unquoted attribute>`` () =
    parseCreator "[attr=val]"
    |> isEqualTo [Creator [Attributes [("attr", "val")]]]

[<Test>]
let ``<Parse unquoted attribute with semicolon>`` () =
    let ex = throws<cpe> (fun () -> parseCreator "[attr=val;style=display:none;visibility:hidden]" |> ignore)
    Assert.Contains("Error in Ln: 1 Col: 40", ex.Message)    

[<Test>]
let ``<Parse quoted attribute with semicolon>`` () =
    parseCreator "[attr=val;style='display:none;visibility:hidden']"
    |> isEqualTo [Creator
                    [Attributes
                        [("attr", "val"); ("style", "display:none;visibility:hidden")]]]

[<Test>]
let ``<Parse an empty id>`` () =
    let ex = throws<cpe> (fun () -> parseCreator ".class#" |> ignore)
    Assert.Contains("Error in Ln: 1 Col: 8", ex.Message)

[<Test>]
let ``<Parse an empty class>`` () =
    let ex = throws<cpe> (fun () -> parseCreator "#id." |> ignore)
    Assert.Contains("Error in Ln: 1 Col: 5", ex.Message)

[<Test>]
let ``<Parse an empty attributes list>`` () =
    let ex = throws<cpe> (fun () -> parseCreator ".class[]" |> ignore)
    Assert.Contains("Error in Ln: 1 Col: 8", ex.Message)

[<Test>]
let ``<Parse an attribute with no value>`` () =
    let ex = throws<cpe> (fun () -> parseCreator "[attr]" |> ignore)
    Assert.Contains("Error in Ln: 1 Col: 6", ex.Message)

[<Test>]
let ``<Parse siblings with commas>`` () =
    parseCreator "div, div,div"
    |> isEqualTo [ Creator[Tag "div"]; Connector(Sibling);
                   Creator[Tag "div"]; Connector(Sibling); 
                   Creator[Tag "div"] ]

[<Test>]
let ``<Parse siblings with plus signs>`` () =
    parseCreator "div + div+div"
    |> isEqualTo [ Creator[Tag "div"]; Connector(Sibling);
                   Creator[Tag "div"]; Connector(Sibling); 
                   Creator[Tag "div"] ]

[<Test>]
let ``<Parse parent child tags>`` () =
    parseCreator "div > h1"
    |> isEqualTo [ Creator[Tag "div"]; Connector(Child);
                  Creator[Tag "h1"] ]

[<Test>]
let ``<Parse child and sibling tags>`` () =
    parseCreator "div > h1 + p"
    |> isEqualTo [ Creator[Tag "div"]; Connector(Child);
                   Creator[Tag "h1"]; Connector(Sibling);
                   Creator[Tag "p"] ]

[<Test>]
let ``<Parse child and sibling tags with classes and IDs>`` () =
    parseCreator "div#item > h1.title + p.body"
    |> isEqualTo [ Creator[Tag "div"; Id "item"]; Connector(Child);
                   Creator[Tag "h1"; Classes["title"]]; Connector(Sibling);
                   Creator[Tag "p"; Classes["body"]] ]

[<Test>]
let ``<Parse tag with placeholder>`` () =
    parseCreator "div$ph"
    |> isEqualTo [Creator[Tag "div"; Placeholder "ph"]]

[<Test>]
let ``<Parse two tags with classes and placeholders>`` () =
    parseCreator "div$ph1, div$ph2"
    |> isEqualTo [Creator[Tag "div"; Placeholder "ph1"]; Connector(Sibling);
              Creator[Tag "div"; Placeholder "ph2"]]

[<Test>]
let ``<Parse invalid adjacent combinators>`` () =
    let ex = throws<cpe> (fun () -> parseCreator "+>+" |> ignore)
    Assert.Contains("Error in Ln: 1 Col: 2", ex.Message)

//[<Test>]
//let ``<Parse an entire HTML document>`` () =
//    parseCreator "html > head > title$title < body > h1.header$head + p.story$story"
//    |> isEqualTo [ Creator [Tag "html"]; Connector Child; Creator [Tag "head"];
//                   Connector Child; Creator [Tag "title"; Placeholder "title"];
//                   Connector ParentSibling; Creator [Tag "body"]; Connector Child;
//                   Creator [Tag "h1"; Classes ["header"]; Placeholder "head"];
//                   Connector Sibling;
//                   Creator [Tag "p"; Classes ["story"]; Placeholder "story"]]

[<Test>]
let ``<Parse a repeated element>`` () =
    parseCreator "div*3"
    |> isEqualTo [Creator[Tag "div"; Repeat 3]]

[<Test>]
let ``<Parse two repeated elements>`` () =
    parseCreator "div*2 + p*3"
    |> isEqualTo [ Creator[Tag "div"; Repeat 2]; Connector(Sibling);
                   Creator[Tag "p"; Repeat 3] ]

[<Test>]
let ``<Parse complex repeated element>`` () =
    parseCreator "div#thrilling.shocking.amazing[align='center';title='quite the title']*5"
    |> isEqualTo [Creator[
                    Tag "div"; Id "thrilling";
                    Classes["shocking"; "amazing"];
                    Attributes[("align", "center"); ("title", "quite the title")];
                    Repeat 5 ]]
