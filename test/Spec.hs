import Text.Trifecta
import Text.ICal
import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "Parsing lines" $ do

    it "parses a single line" $
      successfulParse contentLineTest `shouldBe` True

    it "parses multiple lines" $
      successfulParse contentLinesTest `shouldBe` True

    it "unfolds lines" $
      successfulParse unfoldingExample `shouldBe` True

  describe "Parameter parsing" $ do

    it "parses dquoted params" $
      successfulParse quotedPropertyParams `shouldBe` True


    it "parses ALTREP" $
      successfulParse altrep `shouldBe` True

    it "parses CN" $
      successfulParse commonNameTest `shouldBe` True

    it "parses CUTYPE" $
      successfulParse cutypeTest `shouldBe` True





isFailure :: Result a -> Bool
isFailure (Failure _) = True
isFailure (Success _) = False


isSuccess :: Result a -> Bool
isSuccess = not . isFailure


scheduleParser :: String -> Result Schedule
scheduleParser = parseString schedule mempty


successfulParse :: String -> Bool
successfulParse = isSuccess . scheduleParser


{-
 - Example strings
 -}

contentLineTest :: String
contentLineTest =
  "RDATE;VALUE=DATE:19970304,19970504,19970704,19970904"


contentLinesTest:: String
contentLinesTest =
  "Here:is line one\r\nAnd: here is line two"


unfoldingExample :: String
unfoldingExample =
  "ATTENDEE;RSVP=TRUE;ROLE=REQ-PARTICIPANT:mailto:\r\n\tjsmith@example.com"


quotedPropertyParams :: String
quotedPropertyParams =
  "DESCRIPTION;ALTREP=\"cid:part1.0001@example.org\":The Fall'98 Wild\r\n  Wizards Conference - - Las Vegas\\, NV\\, USA"


altrep :: String
altrep =
  "DESCRIPTION;ALTREP=\"CID:part3.msg.970415T083000@example.com\":\r\n\t Project XYZ Review Meeting will include the following agenda\r\n\t items: (a) Market Overview\\, (b) Finances\\, (c) Project Man\r\n\tagement"


commonNameTest :: String
commonNameTest =
  "ORGANIZER;CN=\"John Smith\":mailto:jsmith@example.com"


cutypeTest :: String
cutypeTest =
  "ATTENDEE;CUTYPE=GROUP:mailto:ietf-calsch@example.org"

