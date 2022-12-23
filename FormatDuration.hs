module FormatDuration where

import Data.List (intercalate)
import Data.Maybe (catMaybes)

formatDuration :: (Integral i, Show i) => i -> String
formatDuration n
  | n < 0 = error "Must be non-negative integer"
  | n == 0 = "now"
  | otherwise = helper' . filter (not . null) $ helper n [(60, "second"), (60, "minute"), (24, "hour"), (365, "day")]

helper n ((up, s) : ups) =
  case divMod n up of
    (0, m) -> [zeroOrOneOrMore m s]
    (n', m) -> zeroOrOneOrMore m s : helper n' ups
helper n [] = [zeroOrOneOrMore n "year"]

zeroOrOneOrMore 0 _ = ""
zeroOrOneOrMore 1 s = "1 " ++ s
zeroOrOneOrMore n s = show n ++ " " ++ s ++ "s"

helper' [s] = s
helper' (s : ss) = intercalate ", " (reverse ss) ++ " and " ++ s



formatDuration' :: (Integral i) => i -> String
formatDuration' n = show (toEnum (fromIntegral n) :: Time)

data Time = Time
  { year :: Int,
    day :: Int,
    hour :: Int,
    minute :: Int,
    second :: Int
  }
  deriving (Eq, Ord)

instance Enum Time where
  fromEnum Time {year = y, day = d, hour = h, minute = m, second = s} =
    60 * (60 * (24 * (365 * y + d) + h) + m) + s
  toEnum ss = Time y d h m s
    where
      (ms, s) = ss `divMod` 60
      (hs, m) = ms `divMod` 60
      (ds, h) = hs `divMod` 24
      (y, d) = ds `divMod` 365

instance Show Time where
  show Time {year = y, day = d, hour = h, minute = m, second = s} =
    (toHumanReadable . formatList) [y, d, h, m, s]

toHumanReadable :: [String] -> String
toHumanReadable [] = ""
toHumanReadable [s] = s
toHumanReadable [s1, s2] = s1 ++ " and " ++ s2
toHumanReadable (str : strs) = str ++ ", " ++ toHumanReadable strs

formatList :: [Int] -> [String]
formatList [0, 0, 0, 0, 0] = ["now"]
formatList values = catMaybes $ zipWith showMaybeTimeUnit units values
  where
    units = ["year", "day", "hour", "minute", "second"]
    showMaybeTimeUnit :: String -> Int -> Maybe String
    showMaybeTimeUnit unit x
      | x <= 0 = Nothing
      | x == 1 = Just (show x ++ " " ++ unit)
      | otherwise = Just (show x ++ " " ++ unit ++ "s")

test1 = formatDuration 0 == "now"

test2 = formatDuration 1 == "1 second"

test3 = formatDuration 3600 == "1 hour"

test4 = formatDuration 120 == "2 minutes"

test5 = formatDuration 62 == "1 minute and 2 seconds"

test6 = formatDuration 3662 == "1 hour, 1 minute and 2 seconds"

testAll = test1 && test2 && test3 && test4 && test5 && test6