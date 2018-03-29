namespace CodeBetter.Extensions.Tests
{
   #region
   using System;
   using NUnit.Framework;
   #endregion

   [TestFixture]
   public class StringTests
   {
      [Test]
      public void CreateType_ReturnsInstanceWithNoParameters()
      {
         FakeClass type = "CodeBetter.Extensions.Tests.FakeClass, CodeBetter.Extensions.Tests".CreateType<FakeClass>();
         Assert.AreEqual(0, type.Id);
      }
      [Test]
      public void CreateType_ReturnsInstanceWithParameters()
      {
         FakeClass type = "CodeBetter.Extensions.Tests.FakeClass, CodeBetter.Extensions.Tests".CreateType<FakeClass>(1);
         Assert.AreEqual(1, type.Id);
      }
      [Test, ExpectedException("System.TypeLoadException")]
      public void CreateType_ThrowsExceptionForInvalidType()
      {
         "unknownType".CreateType<string>();
         Assert.Fail();
      }
      
      [Test]
      public void Is_ReturnsFalseForMatches()
      {
         Assert.IsFalse("abc1".Is("abc"));
         Assert.IsFalse("z".Is("123"));
      }
      [Test]
      public void Is_ReturnsTrueForMatches()
      {
         Assert.IsTrue("abc".Is("abc"));
         Assert.IsTrue("123".Is("123"));
         Assert.IsTrue("AAA".Is("aaa"));
         Assert.IsTrue("AAA jka 099al!!l".Is("AAA JKA 099AL!!L"));
      }

      [Test]
      public void Proper_AddsSpaceAfterPunctuation()
      {
         Assert.AreEqual("Hello world, what a wonderful day! I agree.", "hello world,   what a wonderful day!I agree.".Proper());
      }
      [Test]
      public void Proper_CapitalizesFirstLetterOfSentence()
      {
         Assert.AreEqual("Wow. That's amazing.", "Wow. that's amazing.".Proper());
      }
      [Test]
      public void Proper_CapitalizesI()
      {
         Assert.AreEqual("Wow. \"I'm amazing\" I said", "Wow. \"i'm amazing\" i said".Proper());
      }
      [Test]
      public void Proper_RemovesExtraDoubleSpaces()
      {
         Assert.AreEqual("Hello world, what a wonderful day!", "hello   world, what a    wonderful day!".Proper());
      }

      [Test]
      public void Reverse()
      {
         Assert.AreEqual("68840aabc", "cbaa04886".Reverse());
      }

      [Test]
      public void NewlineToBr()
      {
         string input = "Hello {0} world!{0}{0}How Goes?".FormatWith(Environment.NewLine);
         Assert.AreEqual("Hello <br /> world!<br /><br />How Goes?", input.NewlineToBr());
      }

      [Test]
      public void FormatWith()
      {
         Assert.AreEqual("1 - bob", "{0} - {1}".FormatWith(1, "bob"));
      }
      
      [Test]
      public void StripHtml()
      {
         Assert.AreEqual("It's over 9000", "<i>It's</i> <b><a href=\"#\">over</a> 9000</b>".StripHtml());
      }

      [Test]
      public void Capitalize_DoesSingleLetter()
      {
         Assert.AreEqual("Z", "z".Capitalize());
         Assert.AreEqual("W", "W".Capitalize());
      }
      [Test]
      public void Capitalize_DoesMultiLetter()
      {
         Assert.AreEqual("Zebra", "zebra".Capitalize());
         Assert.AreEqual("Wicked", "Wicked".Capitalize());
      }
      [Test]
      public void Capitalize_ReturnsEmptyOnEmpty()
      {
         Assert.AreEqual(string.Empty, "".Capitalize());
      }

      [Test]
      public void Left_ReturnsEmptyForEmptyStringOrZeroLength()
      {
         Assert.AreEqual(string.Empty, "".Left(100));
         Assert.AreEqual(string.Empty, "abc".Left(0));
      }
      [Test]
      public void Left_ReturnsStringIfLengthTooBig()
      {
         Assert.AreEqual("12345", "12345".Left(10));
      }
      [Test]
      public void Left_ReturnsLeftSubstring()
      {
         Assert.AreEqual("12", "12345".Left(2));
      }

      [Test]
      public void Right_ReturnsEmptyForEmptyStringOrZeroLength()
      {
         Assert.AreEqual(string.Empty, "".Right(100));
         Assert.AreEqual(string.Empty, "abc".Right(0));
      }
      [Test]
      public void Right_ReturnsStringIfLengthTooBig()
      {
         Assert.AreEqual("12345", "12345".Right(10));
      }
      [Test]
      public void Right_ReturnsRightSubstring()
      {
         Assert.AreEqual("45", "12345".Right(2));
      }
   }
   [TestFixture]
   public class StringRightOfTests
   {
     [Test]
     public void RightOf_ReturnsFromSubstring()
     {
       Assert.AreEqual("45", "12345".RightOf("3"));
     }

     [Test]
     public void RightOf_HandlesNullCorrectly()
     {
       // calling RightOf on null should throw ArgumentNullException with ParamName = "value"
       bool exceptionThrown = false;
       string test = null;
       try
       {
         test.RightOf("3");
       }
       catch (Exception e)
       {
         exceptionThrown = true;
         Assert.IsInstanceOfType(typeof(ArgumentNullException), e);
         Assert.AreEqual("source", (e as ArgumentNullException).ParamName);
       }
       Assert.AreEqual(true, exceptionThrown);
       // calling with null as value should throw ArgumentNullException with ParamName = "value"
       test = "12345";
       exceptionThrown = false;
       try
       {
         test.RightOf(null);
       }
       catch (Exception e)
       {
         exceptionThrown = true;
         Assert.IsInstanceOfType(typeof(ArgumentNullException), e);
         Assert.AreEqual("value", (e as ArgumentNullException).ParamName);
       }
       Assert.AreEqual(true, exceptionThrown);
     }

     [Test]
     public void RightOf_HandlesEmptyStringCorrectly()
     {
       string test = "";
       Assert.AreEqual(null, test.RightOf("3"));
       Assert.AreEqual("", "123".RightOf("3"));
       Assert.AreEqual("12345", "12345".RightOf(""));
     }

     [Test]
     public void RightOf_StartIndex()
     {
       bool exceptionThrown = false;
       string test = "12345";
       try
       {
         Assert.AreEqual(null, test.RightOf("3", 8));
       }
       catch (Exception e)
       {
         exceptionThrown = true;
         Assert.IsInstanceOfType(typeof(ArgumentOutOfRangeException), e);
         Assert.AreEqual("startIndex", (e as ArgumentOutOfRangeException).ParamName);
       }
       Assert.AreEqual(true, exceptionThrown);
     }
   }
}