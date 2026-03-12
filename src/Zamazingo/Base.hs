module Zamazingo.Base where


enumerate :: (Bounded a, Enum a) => [a]
enumerate = [minBound .. maxBound]
