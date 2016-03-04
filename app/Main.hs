module Main where

import Text.Trifecta
import Text.ICal

main :: IO ()
main = runTest


runTest :: IO ()
runTest = do
  print $ parseString schedule mempty contentLineTest
  print $ parseString schedule mempty contentLinesTest
  print $ parseString schedule mempty foldExample
  print $ parseString schedule mempty quotedPropertyParams
  print $ parseString schedule mempty altrep
  print $ parseString schedule mempty commonNameTest
  print $ parseString schedule mempty cutypeTest
  print $ parseString schedule mempty delegateFrom
  print $ parseString schedule mempty delegateTo
  print $ parseString schedule mempty dir
  print $ parseString schedule mempty attachExample
  print $ parseString schedule mempty freeBusy
  print $ parseString schedule mempty languageTag
  print $ parseString schedule mempty memberList
  print $ parseString schedule mempty partStat
  print $ parseString schedule mempty thisAndFuture
  print $ parseString schedule mempty trigger
  print $ parseString schedule mempty relationship
  print $ parseString schedule mempty attendee
  print $ parseString schedule mempty timezone


contentLineTest :: String
contentLineTest =
  "RDATE;VALUE=DATE:19970304,19970504,19970704,19970904"


contentLinesTest :: String
contentLinesTest =
  "Here:is line one\r\nAnd: here is line two"


foldExample :: String
foldExample =
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


delegateFrom :: String
delegateFrom =
  "ATTENDEE;DELEGATED-FROM=\"mailto:jsmith@example.com\":mailto:\r\n jdoe@example.com"


delegateTo :: String
delegateTo =
  "ATTENDEE;DELEGATED-TO=\"mailto:jdoe@example.com\",\"mailto:jqpublic\r\n @example.com\":mailto:jsmith@example.com"


dir :: String
dir =
  "ORGANIZER;DIR=\"ldap://example.com:6666/o=ABC%20Industries,\r\n c=US???(cn=Jim%20Dolittle)\":mailto:jimdo@example.com"


attachExample :: String
attachExample =
  "ATTACH;FMTTYPE=text/plain;ENCODING=BASE64;VALUE=BINARY:TG9yZW\r\n 0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2ljaW\r\n 5nIGVsaXQsIHNlZCBkbyBlaXVzbW9kIHRlbXBvciBpbmNpZGlkdW50IHV0IG\r\n xhYm9yZSBldCBkb2xvcmUgbWFnbmEgYWxpcXVhLiBVdCBlbmltIGFkIG1pbm\r\n ltIHZlbmlhbSwgcXVpcyBub3N0cnVkIGV4ZXJjaXRhdGlvbiB1bGxhbWNvIG\r\n xhYm9yaXMgbmlzaSB1dCBhbGlxdWlwIGV4IGVhIGNvbW1vZG8gY29uc2VxdW\r\n F0LiBEdWlzIGF1dGUgaXJ1cmUgZG9sb3IgaW4gcmVwcmVoZW5kZXJpdCBpbi\r\n B2b2x1cHRhdGUgdmVsaXQgZXNzZSBjaWxsdW0gZG9sb3JlIGV1IGZ1Z2lhdC\r\n BudWxsYSBwYXJpYXR1ci4gRXhjZXB0ZXVyIHNpbnQgb2NjYWVjYXQgY3VwaW\r\n RhdGF0IG5vbiBwcm9pZGVudCwgc3VudCBpbiBjdWxwYSBxdWkgb2ZmaWNpYS\r\n BkZXNlcnVudCBtb2xsaXQgYW5pbSBpZCBlc3QgbGFib3J1bS4="


freeBusy :: String
freeBusy =
  "FREEBUSY;FBTYPE=BUSY:19980415T133000Z/19980415T170000Z"


languageTag :: String
languageTag =
  "SUMMARY;LANGUAGE=en-US:Company Holiday Party"

memberList :: String
memberList =
  "ATTENDEE;MEMBER=\"mailto:projectA@example.com\",\"mailto:pr\r\n ojectB@example.com\":mailto:janedoe@example.com"

partStat :: String
partStat =
  "ATTENDEE;PARTSTAT=DECLINED:mailto:jsmith@example.com"

thisAndFuture :: String
thisAndFuture =
  "RECURRENCE-ID;RANGE=THISANDFUTURE:19980401T133000Z"

trigger :: String
trigger =
  "TRIGGER;RELATED=END:PT5M"

relationship :: String
relationship =
  "RELATED-TO;RELTYPE=SIBLING:19960401-080045-4000F192713@\r\n example.com"

attendee :: String
attendee =
  "ATTENDEE;RSVP=TRUE:mailto:jsmith@example.com"

timezone :: String
timezone =
  "DTSTART;TZID=America/New_York:19980119T020000"
