module Util.Time 
  ( auTimeLocale
  ) where

import Data.Time (TimeLocale (..), TimeZone (..), utc, iso8601DateFormat)

auTimeLocale :: TimeLocale
auTimeLocale =  TimeLocale 
  { wDays  = [("Sunday",   "Sun"),  ("Monday",    "Mon"),
                  ("Tuesday",  "Tue"),  ("Wednesday", "Wed"),
                  ("Thursday", "Thu"),  ("Friday",    "Fri"),
                  ("Saturday", "Sat")],

        months = [("January",   "Jan"), ("February",  "Feb"),
                  ("March",     "Mar"), ("April",     "Apr"),
                  ("May",       "May"), ("June",      "Jun"),
                  ("July",      "Jul"), ("August",    "Aug"),
                  ("September", "Sep"), ("October",   "Oct"),
                  ("November",  "Nov"), ("December",  "Dec")],

        amPm = ("AM", "PM"),
        dateTimeFmt = iso8601DateFormat (Just "%H:%M:%S %Z%z"),
        dateFmt = "%d/%m/%y",
        timeFmt = "%H:%M:%S",
        time12Fmt = "%I:%M:%S %p",
        knownTimeZones =
          [ utc
          , TimeZone (10 * 60) False "UTC+10"
          ]
  }

