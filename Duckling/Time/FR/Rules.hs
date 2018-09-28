-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.FR.Rules
  ( rules
  ) where

import Data.Text (Text)
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Regex.Types
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
import Duckling.Types
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

ruleAujourdhui :: Rule
ruleAujourdhui = Rule
  { name = "aujourd'hui"
  , pattern =
    [ regex "(aujourd'? ?hui)|(ce jour)|(dans la journ(é|e)e?)|(en ce moment)"
    ]
  , prod = \_ -> tt today
  }

ruleDayofmonthNamedmonth :: Rule
ruleDayofmonthNamedmonth = Rule
  { name = "<day-of-month> <named-month>"
  , pattern =
    [ Predicate isDOMInteger
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleHier :: Rule
ruleHier = Rule
  { name = "hier"
  , pattern =
    [ regex "hier|la veille"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 1
  }

ruleLeLendemainDuTime :: Rule
ruleLeLendemainDuTime = Rule
  { name = "le lendemain du <time>"
  , pattern =
    [ regex "(le|au)? ?lendemain du"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ cycleNthAfter True TG.Day 1 td
      _ -> Nothing
  }

ruleCePartofday :: Rule
ruleCePartofday = Rule
  { name = "ce <part-of-day>"
  , pattern =
    [ regex "cet?t?e?"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time . partOfDay . notLatent <$>
        intersect today td
      _ -> Nothing
  }

ruleCeTime :: Rule
ruleCeTime = Rule
  { name = "ce <time>"
  , pattern =
    [ regex "ce"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 0 False td
      _ -> Nothing
  }

ruleDurationAvantTime :: Rule
ruleDurationAvantTime = Rule
  { name = "<duration> avant <time>"
  , pattern =
    [ dimension Duration
    , regex "avant"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_:Token Time td:_) ->
        tt $ durationBefore dd td
      _ -> Nothing
  }

ruleMidi :: Rule
ruleMidi = Rule
  { name = "midi"
  , pattern =
    [ regex "midi"
    ]
  , prod = \_ -> tt $ hour False 12
  }

ruleNProchainsCycle :: Rule
ruleNProchainsCycle = Rule
  { name = "n prochains <cycle>"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , regex "prochaine?s?|suivante?s?|apr(e|è|é)s"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt $ cycleN True grain n
      _ -> Nothing
  }

ruleNDerniersCycle :: Rule
ruleNDerniersCycle = Rule
  { name = "n derniers <cycle>"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , regex "derni(e|è|é)re?s?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt . cycleN True grain $ - n
      _ -> Nothing
  }

ruleAvantTimeofday :: Rule
ruleAvantTimeofday = Rule
  { name = "avant <time-of-day>"
  , pattern =
    [ regex "(n[ ']importe quand )?(avant|jusqu'(a|à))"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ withDirection TTime.Before td
      _ -> Nothing
  }

ruleHhhmmTimeofday :: Rule
ruleHhhmmTimeofday = Rule
  { name = "hh(:|h)mm (time-of-day)"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))[:h]([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        h <- parseInt m1
        m <- parseInt m2
        tt $ hourMinute (h < 12) h m
      _ -> Nothing
  }

ruleDdMm :: Rule
ruleDdMm = Rule
  { name = "dd mm"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9]) (1[0-2]|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:_)):_) -> do
        m <- parseInt mm
        d <- parseInt dd
        tt $ monthDay m d
      _ -> Nothing
  }

ruleDdMmYyyy :: Rule
ruleDdMmYyyy = Rule
  { name = "dd mm yyyy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9]) (1[0-2]|0?[1-9]) (\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:yy:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleNamedmonthProchain :: Rule
ruleNamedmonthProchain = Rule
  { name = "<named-month> prochain"
  , pattern =
    [ dimension Time
    , regex "prochain"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth 1 False td
      _ -> Nothing
  }

ruleToussaint :: Rule
ruleToussaint = Rule
  { name = "toussaint"
  , pattern =
    [ regex "((la |la journ(é|e)e de la |jour de la )?toussaint|jour des morts)"
    ]
  , prod = \_ -> tt $ monthDay 11 1
  }

ruleDernierCycleDeTimeLatent :: Rule
ruleDernierCycleDeTimeLatent = Rule
  { name = "dernier <cycle> de <time> (latent)"
  , pattern =
    [ regex "derni(e|é|è)re?"
    , dimension TimeGrain
    , regex "d['e]"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleLastOf grain td
      _ -> Nothing
  }

ruleDurationApresTime :: Rule
ruleDurationApresTime = Rule
  { name = "<duration> apres <time>"
  , pattern =
    [ dimension Duration
    , regex "apr(e|è)s"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_:Token Time td:_) ->
        tt $ durationAfter dd td
      _ -> Nothing
  }

ruleNCycleAprs :: Rule
ruleNCycleAprs = Rule
  { name = "n <cycle> après"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    , regex "(d')? ?apr(e|è|é)s|qui sui(t|ves?)|plus tard"
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt $ cycleNth grain n
      _ -> Nothing
  }

ruleYearLatent2 :: Rule
ruleYearLatent2 = Rule
  { name = "year (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 2101 10000
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ year n
      _ -> Nothing
  }

ruleNCycleAvant :: Rule
ruleNCycleAvant = Rule
  { name = "n <cycle> avant"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    , regex "(d')? ?avant|plus t(o|ô)t"
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt . cycleNth grain $ - n
      _ -> Nothing
  }

ruleHourofdayEtQuart :: Rule
ruleHourofdayEtQuart = Rule
  { name = "<hour-of-day> et quart"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "(et )?quart"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 15
      _ -> Nothing
  }

ruleHourofdayEtDemi :: Rule
ruleHourofdayEtDemi = Rule
  { name = "<hour-of-day> et demi"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "et demie?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 30
      _ -> Nothing
  }

ruleHourofdayEtTroisQuart :: Rule
ruleHourofdayEtTroisQuart = Rule
  { name = "<hour-of-day> et trois quarts"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "(et )?(3|trois) quarts?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _) -> tt $ hourMinute is12H hours 45
      _ -> Nothing
  }

ruleHourofdayEtpassDeNumeral :: Rule
ruleHourofdayEtpassDeNumeral = Rule
  { name = "<hour-of-day> et|passé de <number>"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "et|(pass(é|e)e? de)"
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _:
       token:
       _) -> do
        n <- getIntValue token
        tt $ hourMinute is12H hours n
      _ -> Nothing
  }

ruleHourofdayEtpassDeNumeralMinutes :: Rule
ruleHourofdayEtpassDeNumeralMinutes = Rule
  { name = "<hour-of-day> et|passé de <number> minutes"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "et|(pass(é|e)e? de)"
    , Predicate $ isIntegerBetween 1 59
    , regex "min\\.?(ute)?s?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       _:
       token:
       _) -> do
        n <- getIntValue token
        tt $ hourMinute is12H hours n
      _ -> Nothing
  }

ruleHourofdayInteger :: Rule
ruleHourofdayInteger = Rule
  { name = "<hour-of-day> <integer> (as relative minutes)"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isAnHourOfDay, hasNoDirection]
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       token:
       _) -> do
        n <- getIntValue token
        tt $ hourMinute is12H hours n
      _ -> Nothing
  }

ruleHourofdayIntegerMinutes :: Rule
ruleHourofdayIntegerMinutes = Rule
  { name = "<hour-of-day> <integer> minutes"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isAnHourOfDay]
    , Predicate $ isIntegerBetween 1 59
    , regex "min\\.?(ute)?s?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData {TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}:
       token:
       _) -> do
        n <- getIntValue token
        tt $ hourMinute is12H hours n
      _ -> Nothing
  }

ruleHourofdayMoinsIntegerAsRelativeMinutes :: Rule
ruleHourofdayMoinsIntegerAsRelativeMinutes = Rule
  { name = "<hour-of-day> moins <integer> (as relative minutes)"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "moins( le)?"
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:token:_) -> do
        n <- getIntValue token
        Token Time <$> minutesBefore n td
      _ -> Nothing
  }

ruleHourofdayMoinsQuart :: Rule
ruleHourofdayMoinsQuart = Rule
  { name = "<hour-of-day> moins quart"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "moins( le)? quart"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> Token Time <$> minutesBefore 15 td
      _ -> Nothing
  }

ruleAprsLeDayofmonth :: Rule
ruleAprsLeDayofmonth = Rule
  { name = "après le <day-of-month>"
  , pattern =
    [ regex "(apr(e|è)s le|(a|à) partir du)"
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        n <- getIntValue token
        tt . withDirection TTime.After $ dayOfMonth n
      _ -> Nothing
  }

ruleCycleDernier :: Rule
ruleCycleDernier = Rule
  { name = "<cycle> dernier"
  , pattern =
    [ dimension TimeGrain
    , regex "derni(è|e)re?|pass(é|e)e?|pr(e|é)c(e|é)dente?|(d')? ?avant"
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_) ->
        tt . cycleNth grain $ - 1
      _ -> Nothing
  }

ruleLeOrdinalCycleDeTime :: Rule
ruleLeOrdinalCycleDeTime = Rule
  { name = "le <ordinal> <cycle> de <time>"
  , pattern =
    [ regex "l[ea]"
    , dimension Ordinal
    , dimension TimeGrain
    , regex "d['eu]|en"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter True grain (n - 1) td
      _ -> Nothing
  }

ruleDdmm :: Rule
ruleDdmm = Rule
  { name = "dd/-mm"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[/-](1[0-2]|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:_)):_) -> do
        m <- parseInt mm
        d <- parseInt dd
        tt $ monthDay m d
      _ -> Nothing
  }

ruleNamedmonthnameddayDernierpass :: Rule
ruleNamedmonthnameddayDernierpass = Rule
  { name = "<named-month|named-day> dernier|passé"
  , pattern =
    [ dimension Time
    , regex "derni(e|é|è)re?|pass(é|e)e?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth (-1) False td
      _ -> Nothing
  }

ruleLeCycleDernier :: Rule
ruleLeCycleDernier = Rule
  { name = "le <cycle> dernier"
  , pattern =
    [ regex "l[ae']? ?"
    , dimension TimeGrain
    , regex "derni(è|e)re?|pass(é|e)e?"
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt . cycleNth grain $ - 1
      _ -> Nothing
  }

ruleNCyclePassesprecedents :: Rule
ruleNCyclePassesprecedents = Rule
  { name = "n <cycle> passes|precedents"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    , regex "pass(e|è|é)(e|è|é)?s?|pr(e|é)c(e|é)dente?s?|(d')? ?avant|plus t(o|ô)t"
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt . cycleN True grain $ - n
      _ -> Nothing
  }

ruleTimeofdayLatent :: Rule
ruleTimeofdayLatent = Rule
  { name = "time-of-day (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 0 23
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ hour (n < 12) n
      _ -> Nothing
  }

ruleLeCycleDeTime :: Rule
ruleLeCycleDeTime = Rule
  { name = "le <cycle> de <time>"
  , pattern =
    [ regex "l[ea]"
    , dimension TimeGrain
    , regex "d['eu]|en"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain 0 td
      _ -> Nothing
  }

ruleNoel :: Rule
ruleNoel = Rule
  { name = "noel"
  , pattern =
    [ regex "(jour de )?no(e|ë)l"
    ]
  , prod = \_ -> tt $ monthDay 12 25
  }

ruleDayofweekProchain :: Rule
ruleDayofweekProchain = Rule
  { name = "<day-of-week> prochain"
  , pattern =
    [ Predicate isADayOfWeek
    , regex "prochain"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth 0 True td
      _ -> Nothing
  }

ruleDemain :: Rule
ruleDemain = Rule
  { name = "demain"
  , pattern =
    [ regex "(demain)|(le lendemain)"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 1
  }

ruleNamedmonthnameddaySuivantdaprs :: Rule
ruleNamedmonthnameddaySuivantdaprs = Rule
  { name = "<named-month|named-day> suivant|d'après"
  , pattern =
    [ dimension Time
    , regex "suivante?s?|d'apr(e|é|è)s"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth 1 False td
      _ -> Nothing
  }

ruleNameddayEnHuit :: Rule
ruleNameddayEnHuit = Rule
  { name = "<named-day> en huit"
  , pattern =
    [ Predicate isADayOfWeek
    , regex "en (huit|8)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth 1 False td
      _ -> Nothing
  }

ruleHhmmMilitaryTimeofday :: Rule
ruleHhmmMilitaryTimeofday = Rule
  { name = "hhmm (military time-of-day)"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        h <- parseInt m1
        m <- parseInt m2
        tt . mkLatent $ hourMinute False h m
      _ -> Nothing
  }

ruleAprsTimeofday :: Rule
ruleAprsTimeofday = Rule
  { name = "après <time-of-day>"
  , pattern =
    [ regex "(apr(e|è)s|(a|à) partir de|(un peu )?plus tard que)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ withDirection TTime.After td
      _ -> Nothing
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ Predicate isNotLatent
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleErMai :: Rule
ruleErMai = Rule
  { name = "1er mai"
  , pattern =
    [ regex "f(e|ê)te du travail"
    ]
  , prod = \_ -> tt $ monthDay 5 1
  }

ruleAvanthier :: Rule
ruleAvanthier = Rule
  { name = "avant-hier"
  , pattern =
    [ regex "avant[- ]?hier"
    ]
  , prod = \_ -> tt . cycleNth TG.Day $ - 2
  }

ruleCycleProchainsuivantdaprs :: Rule
ruleCycleProchainsuivantdaprs = Rule
  { name = "<cycle> prochain|suivant|d'après"
  , pattern =
    [ dimension TimeGrain
    , regex "prochaine?|suivante?|qui suit|(d')? ?apr(e|è|é)s"
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:_) ->
        tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleLeDayofmonthDatetime :: Rule
ruleLeDayofmonthDatetime = Rule
  { name = "le <day-of-month> à <datetime>"
  , pattern =
    [ regex "le"
    , Predicate isDOMInteger
    , regex "(a|à)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:token:_:Token Time td:_) -> do
        n <- getIntValue token
        Token Time <$> intersect (dayOfMonth n) td
      _ -> Nothing
  }

ruleCedansLeCycle :: Rule
ruleCedansLeCycle = Rule
  { name = "ce|dans le <cycle>"
  , pattern =
    [ regex "(cet?t?e?s?)|(dans l[ae']? ?)"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 0
      _ -> Nothing
  }

ruleDansDuration :: Rule
ruleDansDuration = Rule
  { name = "dans <duration>"
  , pattern =
    [ regex "dans"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) ->
        tt $ inDuration dd
      _ -> Nothing
  }

ruleOrdinalCycleDeTime :: Rule
ruleOrdinalCycleDeTime = Rule
  { name = "<ordinal> <cycle> de <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , regex "d['eu]|en"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter True grain (n - 1) td
      _ -> Nothing
  }

ruleLeCycleProchainsuivantdaprs :: Rule
ruleLeCycleProchainsuivantdaprs = Rule
  { name = "le <cycle> prochain|suivant|d'après"
  , pattern =
    [ regex "l[ae']? ?|une? ?"
    , dimension TimeGrain
    , regex "prochaine?|suivante?|qui suit|(d'? ?)?apr(e|è|é)s"
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 1
      _ -> Nothing
  }

ruleDimTimePartofday :: Rule
ruleDimTimePartofday = Rule
  { name = "<dim time> <part-of-day>"
  , pattern =
    [ dimension Time
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleEnNamedmonth :: Rule
ruleEnNamedmonth = Rule
  { name = "en <named-month>"
  , pattern =
    [ regex "en|au mois de?'?"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

rulePartofdayDuDimTime :: Rule
rulePartofdayDuDimTime = Rule
  { name = "<part-of-day> du <dim time>"
  , pattern =
    [ Predicate isAPartOfDay
    , regex "du"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleYyyymmdd :: Rule
ruleYyyymmdd = Rule
  { name = "yyyy-mm-dd"
  , pattern =
    [ regex "(\\d{2,4})-(1[0-2]|0?[1-9])-(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:m3:_)):_) -> do
        y <- parseInt m1
        m <- parseInt m2
        d <- parseInt m3
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleTimeofdayHeures :: Rule
ruleTimeofdayHeures = Rule
  { name = "<time-of-day> heures"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "h\\.?(eure)?s?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ notLatent td
      _ -> Nothing
  }

ruleDernierDayofweekDeTimeLatent :: Rule
ruleDernierDayofweekDeTimeLatent = Rule
  { name = "dernier <day-of-week> de <time> (latent)"
  , pattern =
    [ regex "derni(e|é|è)re?"
    , Predicate isADayOfWeek
    , regex "d['e]"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        tt $ predLastOf td1 td2
      _ -> Nothing
  }

ruleCeDayofweek :: Rule
ruleCeDayofweek = Rule
  { name = "ce <day-of-week>"
  , pattern =
    [ regex "ce"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ predNth 0 True td
      _ -> Nothing
  }

ruleYearLatent :: Rule
ruleYearLatent = Rule
  { name = "year (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween (- 10000) 999
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ year n
      _ -> Nothing
  }

ruleVersTimeofday :: Rule
ruleVersTimeofday = Rule
  { name = "à|vers <time-of-day>"
  , pattern =
    [ regex "(vers|autour de|(a|à) environ|aux alentours de|(a|à))"
    , Predicate $ and . sequence [isATimeOfDay, isNotLatent]
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleDayofweekDayofmonth :: Rule
ruleDayofweekDayofmonth = Rule
  { name = "<day-of-week> <day-of-month>"
  , pattern =
    [ Predicate isADayOfWeek
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleNameddayEnQuinze :: Rule
ruleNameddayEnQuinze = Rule
  { name = "<named-day> en quinze"
  , pattern =
    [ Predicate isADayOfWeek
    , regex "en (quinze|15)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ predNth 2 False td
      _ -> Nothing
  }

ruleLeDayofmonthNonOrdinal :: Rule
ruleLeDayofmonthNonOrdinal = Rule
  { name = "le <day-of-month> (non ordinal)"
  , pattern =
    [ regex "le"
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        n <- getIntValue token
        tt $ dayOfMonth n
      _ -> Nothing
  }

ruleIntersectByMaisparExempleplutt :: Rule
ruleIntersectByMaisparExempleplutt = Rule
  { name = "intersect by 'mais/par exemple/plutôt'"
  , pattern =
    [ Predicate isNotLatent
    , regex "mais|par exemple|plut(ô|o)t"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleLeCycleAprssuivantTime :: Rule
ruleLeCycleAprssuivantTime = Rule
  { name = "le <cycle> après|suivant <time>"
  , pattern =
    [ regex "l[ea']?"
    , dimension TimeGrain
    , regex "suivante?|apr(e|è|é)s"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain 1 td
      _ -> Nothing
  }

ruleAprsdemain :: Rule
ruleAprsdemain = Rule
  { name = "après-demain"
  , pattern =
    [ regex "apr(e|è)s[- ]?demain"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 2
  }

ruleIlYADuration :: Rule
ruleIlYADuration = Rule
  { name = "il y a <duration>"
  , pattern =
    [ regex "il y a"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) ->
        tt $ durationAgo dd
      _ -> Nothing
  }

ruleDudansLePartofday :: Rule
ruleDudansLePartofday = Rule
  { name = "du|dans le <part-of-day>"
  , pattern =
    [ regex "du|dans l[ae']? ?|au|en|l[ae' ]|d(è|e)s l?[ae']?"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ notLatent td
      _ -> Nothing
  }

ruleYear :: Rule
ruleYear = Rule
  { name = "year"
  , pattern =
    [ Predicate $ isIntegerBetween 1000 2100
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt $ year n
      _ -> Nothing
  }

ruleMaintenant :: Rule
ruleMaintenant = Rule
  { name = "maintenant"
  , pattern =
    [ regex "maintenant|tout de suite"
    ]
  , prod = \_ -> tt now
  }

ruleDdmmyyyy :: Rule
ruleDdmmyyyy = Rule
  { name = "dd/-mm/-yyyy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[/-](1[0-2]|0?[1-9])[-/](\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:m3:_)):_) -> do
        y <- parseInt m3
        m <- parseInt m2
        d <- parseInt m1
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleLeCycleAvantprcdentTime :: Rule
ruleLeCycleAvantprcdentTime = Rule
  { name = "le <cycle> avant|précédent <time>"
  , pattern =
    [ regex "l[ea']?"
    , dimension TimeGrain
    , regex "avant|pr(é|e)c(é|e)dent"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter False grain (-1) td
      _ -> Nothing
  }

ruleDayOfMonthPremier :: Rule
ruleDayOfMonthPremier = Rule
  { name = "day of month (premier)"
  , pattern =
    [ regex "premier|prem\\.?|1 ?er"
    ]
  , prod = \_ -> tt $ dayOfMonth 1
  }

ruleLeVeilleDuTime :: Rule
ruleLeVeilleDuTime = Rule
  { name = "le veille du <time>"
  , pattern =
    [ regex "(la )?veille du"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tt $ cycleNthAfter True TG.Day (-1) td
      _ -> Nothing
  }

ruleLeTime :: Rule
ruleLeTime = Rule
  { name = "le <time>"
  , pattern =
    [ regex "l[ea]"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleDayofweekDayofmonthTimeofday :: Rule
ruleDayofweekDayofmonthTimeofday = Rule
  { name = "<day-of-week> <day-of-month> à <time-of-day>)"
  , pattern =
    [ Predicate isADayOfWeek
    , Predicate isDOMInteger
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:token:Token Time td2:_) -> do
        dom <- intersectDOM td1 token
        Token Time <$> intersect dom td2
      _ -> Nothing
  }

ruleJourDeLan :: Rule
ruleJourDeLan = Rule
  { name = "jour de l'an"
  , pattern =
    [ regex "(jour de l'|nouvel )an"
    ]
  , prod = \_ -> tt $ monthDay 1 1
  }

ruleMinuit :: Rule
ruleMinuit = Rule
  { name = "minuit"
  , pattern =
    [ regex "minuit"
    ]
  , prod = \_ -> tt $ hour False 0
  }

ruleNCycleSuivants :: Rule
ruleNCycleSuivants = Rule
  { name = "n <cycle> suivants"
  , pattern =
    [ Predicate $ isIntegerBetween 2 9999
    , dimension TimeGrain
    , regex "prochaine?s?|suivante?s?|apr(e|è|é)s|qui sui(t|ves?)|plus tard"
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        tt $ cycleN True grain n
      _ -> Nothing
  }

rulePlusTard :: Rule
rulePlusTard = Rule
  { name = "plus tard"
  , pattern =
    [ regex "(un peu )?plus tard"
    ]
  , prod = \_ -> tt $ withDirection TTime.After $ cycleNth TG.Minute 10

  }

rulePlusTardPartofday :: Rule
rulePlusTardPartofday = Rule
  { name = "plus tard <part-of-day>"
  , pattern =
    [ regex "(un peu )?plus tard"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleTimezone :: Rule
ruleTimezone = Rule
  { name = "<time> timezone"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isATimeOfDay]
    , regex "\\b(YEKT|YEKST|YAKT|YAKST|WITA|WIT|WIB|WGT|WGST|WFT|WET|WEST|WAT|WAST|VUT|VLAT|VLAST|VET|UZT|UYT|UYST|UTC|ULAT|TVT|TMT|TLT|TKT|TJT|TFT|TAHT|SST|SRT|SGT|SCT|SBT|SAST|SAMT|RET|PYT|PYST|PWT|PST|PONT|PMST|PMDT|PKT|PHT|PHOT|PGT|PETT|PETST|PET|PDT|OMST|OMSST|NZST|NZDT|NUT|NST|NPT|NOVT|NOVST|NFT|NDT|NCT|MYT|MVT|MUT|MST|MSK|MSD|MMT|MHT|MDT|MAWT|MART|MAGT|MAGST|LINT|LHST|LHDT|KUYT|KST|KRAT|KRAST|KGT|JST|IST|IRST|IRKT|IRKST|IRDT|IOT|IDT|ICT|HOVT|HKT|GYT|GST|GMT|GILT|GFT|GET|GAMT|GALT|FNT|FKT|FKST|FJT|FJST|EST|EGT|EGST|EET|EEST|EDT|ECT|EAT|EAST|EASST|DAVT|ChST|CXT|CVT|CST|COT|CLT|CLST|CKT|CHAST|CHADT|CET|CEST|CDT|CCT|CAT|CAST|BTT|BST|BRT|BRST|BOT|BNT|AZT|AZST|AZOT|AZOST|AWST|AWDT|AST|ART|AQTT|ANAT|ANAST|AMT|AMST|ALMT|AKST|AKDT|AFT|AEST|AEDT|ADT|ACST|ACDT)\\b"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token RegexMatch (GroupMatch (tz:_)):
       _) -> Token Time <$> inTimezone (Text.toUpper tz) td
      _ -> Nothing
  }

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "Janvier"   , "janvier|janv\\.?"                    )
  , ( "Fevrier"   , "f(é|e)vrier|f(é|e)v\\.?"   )
  , ( "Mars"      , "mars|mar\\.?"                        )
  , ( "Avril"     , "avril|avr\\.?"                       )
  , ( "Mai"       , "mai"                                 )
  , ( "Juin"      , "juin|jun\\.?"                        )
  , ( "Juillet"   , "juillet|juil?\\."                    )
  , ( "Aout"      , "ao(û|u)t|aou\\.?"               )
  , ( "Septembre" , "septembre|sept?\\.?"                 )
  , ( "Octobre"   , "octobre|oct\\.?"                     )
  , ( "Novembre"  , "novembre|nov\\.?"                    )
  , ( "Decembre"  ,  "d(é|e)cembre|d(é|e)c\\.?" )
  ]

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "Lundi"    , "lundi"    )
  , ( "Mardi"    , "mardi"    )
  , ( "Mercredi" , "mercredi" )
  , ( "Jeudi"    , "jeudi"    )
  , ( "Vendredi" , "vendredi" )
  , ( "Samedi"   , "samedi"   )
  , ( "Dimanche" , "dimanche" )
  ]

rules :: [Rule]
rules =
  [ ruleAujourdhui
  , ruleAvantTimeofday
  , ruleAvanthier
  , ruleCeDayofweek
  , ruleCePartofday
  , ruleCeTime
  , ruleCedansLeCycle
  , ruleCycleDernier
  , ruleCycleProchainsuivantdaprs
  , ruleDansDuration
  , ruleDayOfMonthPremier
  , ruleDayofmonthNamedmonth
  , ruleDayofweekDayofmonth
  , ruleDayofweekDayofmonthTimeofday
  , ruleDayofweekProchain
  , ruleDdMm
  , ruleDdMmYyyy
  , ruleDdmm
  , ruleDdmmyyyy
  , ruleDemain
  , ruleDernierCycleDeTimeLatent
  , ruleDernierDayofweekDeTimeLatent
  , ruleDimTimePartofday
  , ruleDudansLePartofday
  , ruleDurationApresTime
  , ruleDurationAvantTime
  , ruleEnNamedmonth
  , ruleErMai
  , ruleHhhmmTimeofday
  , ruleHhmmMilitaryTimeofday
  , ruleHier
  , ruleIlYADuration
  , ruleIntersect
  , ruleIntersectByMaisparExempleplutt
  , ruleJourDeLan
  , ruleLeCycleAprssuivantTime
  , ruleLeCycleAvantprcdentTime
  , ruleLeCycleDeTime
  , ruleLeCycleDernier
  , ruleLeCycleProchainsuivantdaprs
  , ruleLeDayofmonthDatetime
  , ruleLeDayofmonthNonOrdinal
  , ruleLeLendemainDuTime
  , ruleLeOrdinalCycleDeTime
  , ruleLeTime
  , ruleLeVeilleDuTime
  , ruleMaintenant
  , ruleMidi
  , ruleMinuit
  , ruleNCycleAprs
  , ruleNCycleAvant
  , ruleNCyclePassesprecedents
  , ruleNCycleSuivants
  , ruleNDerniersCycle
  , ruleNProchainsCycle
  , ruleNameddayEnHuit
  , ruleNameddayEnQuinze
  , ruleNamedmonthProchain
  , ruleNamedmonthnameddayDernierpass
  , ruleNamedmonthnameddaySuivantdaprs
  , ruleNoel
  , ruleOrdinalCycleDeTime
  , rulePartofdayDuDimTime
  , ruleTimeofdayHeures
  , ruleTimeofdayLatent
  , ruleToussaint
  , ruleVersTimeofday
  , ruleYear
  , ruleYearLatent
  , ruleYearLatent2
  , ruleYyyymmdd
  , ruleHourofdayMoinsQuart
  , ruleHourofdayMoinsIntegerAsRelativeMinutes
  , ruleHourofdayIntegerMinutes
  , ruleHourofdayInteger
  , ruleHourofdayEtpassDeNumeral
  , ruleHourofdayEtpassDeNumeralMinutes
  , ruleHourofdayEtTroisQuart
  , ruleHourofdayEtQuart
  , ruleHourofdayEtDemi
  , ruleTimezone
  , rulePlusTard
  , rulePlusTardPartofday
  ]
  ++ ruleMonths
  ++ ruleDaysOfWeek
