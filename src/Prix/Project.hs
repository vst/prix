module Prix.Project where

import Control.Applicative ((<|>))
import qualified Data.Time as Time
import qualified Options.Applicative as OA


data IterationQuery
  = IterationQueryPrevious
  | IterationQueryCurrent
  | IterationQueryNext
  | IterationQueryDate Time.Day
  deriving (Eq, Show)


iterationQueryParser :: OA.Parser IterationQuery
iterationQueryParser =
  previous <|> current <|> next <|> bydate
  where
    previous = IterationQueryPrevious <$ OA.flag' () (OA.long "previous" <> OA.help "Select previous iteration")
    current = IterationQueryCurrent <$ OA.flag' () (OA.long "current" <> OA.help "Select current iteration")
    next = IterationQueryNext <$ OA.flag' () (OA.long "next" <> OA.help "Select next iteration")
    bydate = IterationQueryDate <$> OA.option OA.auto (OA.long "date" <> OA.metavar "YYYY-MM-DD" <> OA.help "Select iteration by date (YYYY-MM-DD format)")


queryIteration :: Integer -> Time.Day -> IterationQuery -> IO Integer
queryIteration days inception query = do
  reference <- case query of
    IterationQueryPrevious -> Time.addDays (-7) . Time.utctDay <$> Time.getCurrentTime
    IterationQueryCurrent -> Time.utctDay <$> Time.getCurrentTime
    IterationQueryNext -> Time.addDays 7 . Time.utctDay <$> Time.getCurrentTime
    IterationQueryDate date -> pure date
  pure $ getIteration days inception reference


-- | Get the iteration number based on the number of days per iteration,
-- the inception date, and the reference date.
--
-- The inception date is considered as the Monday of the week that the inception date falls in.
--
-- >>> getIteration 7 (read "2022-06-06") (read "2025-07-20")
-- 162
-- >>> getIteration 7 (read "2022-06-06") (read "2025-07-21")
-- 163
-- >>> getIteration 7 (read "2022-06-06") (read "2025-07-26")
-- 163
-- >>> getIteration 7 (read "2022-06-06") (read "2025-07-27")
-- 163
-- >>> getIteration 7 (read "2022-06-06") (read "2025-07-28")
-- 164
getIteration :: Integer -> Time.Day -> Time.Day -> Integer
getIteration days inception reference =
  let -- Get the Monday of the week that the inception date falls in:
      d1 = getMondayOfWeek inception
      -- Get the Monday of the week that the reference date falls in:
      d2 = getMondayOfWeek reference
   in -- Calculate the number of iterations based on the difference in days:
      Time.diffDays d2 d1 `div` fromIntegral days


-- | Get the Monday of the week that the given day falls in.
--
-- >>> getMondayOfWeek (read "2025-07-21")
-- 2025-07-21
-- >>> getMondayOfWeek (read "2025-07-22")
-- 2025-07-21
-- >>> getMondayOfWeek (read "2025-07-26")
-- 2025-07-21
-- >>> getMondayOfWeek (read "2025-07-27")
-- 2025-07-21
-- >>> getMondayOfWeek (read "2025-07-28")
-- 2025-07-28
-- >>> getMondayOfWeek (read "2025-07-29")
-- 2025-07-28
getMondayOfWeek :: Time.Day -> Time.Day
getMondayOfWeek =
  Time.weekFirstDay Time.Monday
