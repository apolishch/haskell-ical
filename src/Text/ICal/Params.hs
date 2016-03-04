module Text.ICal.Params where


import Control.Applicative
import Text.Trifecta
import Text.ICal.Combinators


data Param
  = Altrep String
  | CommonName String
  | CalendarUserType CalendarUser
  | DelegatedFrom [String]
  | DelegatedTo [String]
  | Dir String
  | EncodingType Encoding
  | FormatType String
  | FreeBusyType FreeBusy
  | Language String
  | Member [String]
  | PartStat ParticipationStatus
  | Range String
  | TriggerRelation TriggerState
  | RelType Relationship
  | Role RoleType
  | RSVP Bool
  | SentBy String
  | TimezoneID String
  | Value ValueType
  | OtherParam String [String]
  deriving (Show, Eq, Ord)


data CalendarUser
  = Individual
  | Group
  | Resource
  | Room
  | Unknown (Maybe String)
  deriving (Show, Eq, Ord)


data Encoding
  = EightBit
  | Base64
  deriving (Show, Eq, Ord)


data FreeBusy
  = Free
  | Busy (Maybe String)
  | BusyUnavailable
  | BusyTentative
  deriving (Show, Eq, Ord)

data ParticipationStatus
  = NeedsAction (Maybe String)
  | Accepted
  | Declined
  | Tentative
  | Delegated
  | InProcess
  | Completed
  deriving (Show, Eq, Ord)

data TriggerState
  = Start
  | End
  deriving (Show, Eq, Ord)

data Relationship
  = Parent (Maybe String)
  | Child
  | Sibling
  deriving (Show, Eq, Ord)

data RoleType
  = Chair
  | RequiredParticipant (Maybe String)
  | OptionalParticipant
  | NonParticipant
  deriving (Show, Eq, Ord)

data ValueType
  = BinaryValue
  | BooleanValue
  | CalendarAddress
  | DateValue
  | DateTimeValue
  | TimeValue
  | DurationValue
  | FloatValue
  | IntegerValue
  | PeriodValue
  | RecurValue
  | TextValue
  | UriValue
  | UtcOffsetValue
  | OtherValue String
  deriving (Show, Eq, Ord)


{-
 - Based on param name, parses the values and returns an appropriate type.
 -}
paramValue :: String -> Parser Param

-- https://tools.ietf.org/html/rfc5545#section-3.2.1
paramValue "ALTREP" = do
  -- eventually we'll want to parse this as a uri:
  value <- betweenDquotes quoteSafeChars
  return $ Altrep value

-- https://tools.ietf.org/html/rfc5545#section-3.2.2
paramValue "CN" = do
  value <- paramValueChars
  return $ CommonName value

-- https://tools.ietf.org/html/rfc5545#section-3.2.3
paramValue "CUTYPE" = do
  value <- paramValueChars
  return $
    case value of
      "INDIVIDUAL" ->
        CalendarUserType Individual
      "GROUP" ->
        CalendarUserType Group
      "RESOURCE" ->
        CalendarUserType Resource
      "ROOM" ->
        CalendarUserType Room
      "UNKNOWN" ->
        CalendarUserType (Unknown Nothing)
      _ ->
        CalendarUserType (Unknown $ Just value)

-- https://tools.ietf.org/html/rfc5545#section-3.2.4
paramValue "DELEGATED-FROM" = do
  values <- betweenDquotes quoteSafeChars `sepBy` char ','
  return $ DelegatedFrom values

-- https://tools.ietf.org/html/rfc5545#section-3.2.5
paramValue "DELEGATED-TO" = do
  values <- betweenDquotes quoteSafeChars `sepBy` char ','
  return $ DelegatedTo values

-- https://tools.ietf.org/html/rfc5545#section-3.2.6
paramValue "DIR" = do
  value <- betweenDquotes quoteSafeChars
  return $ Dir value

-- https://tools.ietf.org/html/rfc5545#section-3.2.7
paramValue "ENCODING" = do
  value <- string "8BIT" <|> string "BASE64"
  return $
    case value of
      "8BIT" ->
        EncodingType EightBit
      "BASE64" ->
        EncodingType Base64

-- https://tools.ietf.org/html/rfc5545#section-3.2.8
paramValue "FMTTYPE" = do
  -- TODO: Properly parse type
  --   https://tools.ietf.org/html/rfc4288#section-4.2
  value <- paramValueChars
  return $ FormatType value

-- https://tools.ietf.org/html/rfc5545#section-3.2.9
paramValue "FBTYPE" = do
  value <- nameParser
  return $
    case value of
      "FREE" ->
        FreeBusyType Free
      "BUSY" ->
        FreeBusyType (Busy Nothing)
      "BUSY-TENTATIVE" ->
        FreeBusyType BusyTentative
      "BUSY-UNAVAILABLE" ->
        FreeBusyType BusyUnavailable
      _ ->
        FreeBusyType (Busy (Just value))

-- https://tools.ietf.org/html/rfc5545#section-3.2.10
paramValue "LANGUAGE" = do
  -- TODO: Parase language tag: https://tools.ietf.org/html/rfc5646
  value <- safeChars
  return $ Language value

-- https://tools.ietf.org/html/rfc5545#section-3.2.11
paramValue "MEMBER" = do
  values <- betweenDquotes quoteSafeChars `sepBy` char ','
  return $ Member values

-- https://tools.ietf.org/html/rfc5545#section-3.2.12
paramValue "PARTSTAT" = do
  value <- nameParser
  return $
    case value of
      "NEEDS-ACTION" ->
        PartStat (NeedsAction Nothing)
      "ACCEPTED" ->
        PartStat Accepted
      "DECLINED" ->
        PartStat Declined
      "TENTATIVE" ->
        PartStat Tentative
      "DELEGATED" ->
        PartStat Delegated
      "IN-PROCESS" ->
        PartStat InProcess
      "COMPLETED" ->
        PartStat Completed
      _ ->
        PartStat (NeedsAction (Just value))

-- https://tools.ietf.org/html/rfc5545#section-3.2.13
paramValue "RANGE" = do
  value <- string "THISANDFUTURE"
  return $ Range value

-- https://tools.ietf.org/html/rfc5545#section-3.2.14
paramValue "RELATED" = do
  value <- string "START" <|> string "END"
  return $
    case value of
      "START" ->
        TriggerRelation Start
      "END" ->
        TriggerRelation End
      _ ->
        TriggerRelation Start

-- https://tools.ietf.org/html/rfc5545#section-3.2.15
paramValue "RELTYPE" = do
  value <- nameParser
  return $ RelType $
    case value of
      "Parent" ->
        Parent Nothing
      "CHILD" ->
        Child
      "SIBLING" ->
        Sibling
      _ ->
        Parent (Just value)

-- https://tools.ietf.org/html/rfc5545#section-3.2.16
paramValue "ROLE" = do
  value <- nameParser
  return $ Role $
    case value of
      "CHAIR" ->
        Chair
      "REQ-PARTICIPANT" ->
        RequiredParticipant Nothing
      "OPT-PARTICIPANT" ->
        OptionalParticipant
      "NON-PARTICIPANT" ->
        NonParticipant
      _ ->
        RequiredParticipant $ Just value

-- https://tools.ietf.org/html/rfc5545#section-3.2.17
paramValue "RSVP" = do
  value <- string "TRUE" <|> string "FALSE"
  return $ RSVP $
    case value of
      "TRUE" -> True
      "FALSE" -> False
      _ -> False

-- https://tools.ietf.org/html/rfc5545#section-3.2.18
paramValue "SENT-BY" = do
  value <- betweenDquotes quoteSafeChars
  return $ SentBy value

-- https://tools.ietf.org/html/rfc5545#section-3.2.19
paramValue "TZID" = do
  optional (char '/')
  value <- paramValueChars
  return $ TimezoneID value

-- https://tools.ietf.org/html/rfc5545#section-3.2.20
paramValue "VALUE" = do
  value <- nameParser
  return $ Value $
    case value of
      "BINARY" ->
        BinaryValue
      "BOOLEAN" ->
        BooleanValue
      "CAL-ADDRESS" ->
        CalendarAddress
      "DATE" ->
        DateValue
      "DATE-TIME" ->
        DateTimeValue
      "DURATION" ->
        DurationValue
      "FLOAT" ->
        FloatValue
      "INTEGER" ->
        IntegerValue
      "PERIOD" ->
        PeriodValue
      "RECUR" ->
        RecurValue
      "TEXT" ->
        TextValue
      "TIME" ->
        TimeValue
      "URI" ->
        UriValue
      "UTC-OFFSET" ->
        UtcOffsetValue
      _ ->
        OtherValue value

paramValue name = do
  values <- paramValueChars `sepBy` char ','
  return $ OtherParam name values

paramValueChars :: Parser String
paramValueChars =
  betweenDquotes quoteSafeChars <|> safeChars
