module Block3.Task1
  (
    WeekDay (..)
  , nextDay
  , afterDays
  , isWeekend
  , daysToParty
  ) where

daysCnt :: Int
daysCnt = 7

data WeekDay
  = Monday | Tuesday | Wednesday | Thursday
  | Friday | Saturday | Sunday
  deriving Show

instance Enum WeekDay where
  toEnum n = case n of
    0 -> Monday
    1 -> Tuesday
    2 -> Wednesday
    3 -> Thursday
    4 -> Friday
    5 -> Saturday
    6 -> Sunday
    _ -> error "Incorrect number of a week day"

  fromEnum day = case day of
    Monday    -> 0
    Tuesday   -> 1
    Wednesday -> 2
    Thursday  -> 3
    Friday    -> 4
    Saturday  -> 5
    Sunday    -> 6

nextDay :: WeekDay -> WeekDay
nextDay = afterDays 1

afterDays :: Int -> WeekDay -> WeekDay
afterDays n day = toEnum day'
  where day' = (fromEnum day + n) `mod` daysCnt

isWeekend :: WeekDay -> Bool
isWeekend day = daysToParty day > 4

daysToParty :: WeekDay -> Int
daysToParty day = (4 - fromEnum day + daysCnt) `mod` daysCnt
