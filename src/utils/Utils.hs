-- | toolchain of utils used throughout the application
module Utils
    ( 
      ApiError (..),
      ValidationResult (..)
    ) where
---
import Control.Exception ( Exception )
---

-- | Generic API error type used for user input validation
data ApiError 
    = ValidationError String String
    | ResourceNotFoundError String
    deriving (Show)

instance Exception ApiError

-- | Validation result used when parsing user login data inputs
data ValidationResult
    = Invalid String 
    | Valid 
    deriving (Show)
