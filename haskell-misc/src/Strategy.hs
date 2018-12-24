module Strategy where

data Strategy = Strategy


data Configuration

data Overrides

type Errors = [String]

data Validated a
  = Failure Errors | Success a

data PerformancePerDay

class Constrained a where
  validate :: a -> Validated a

validatePerformanceReport :: PerformancePerDay -> Validated PerformancePerDay
validatePerformanceReport = undefined


