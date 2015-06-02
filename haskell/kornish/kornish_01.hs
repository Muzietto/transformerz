{-# LANGUAGE OverloadedStrings #-}
4
import Data.Text

data LoginError = InvalidEmail
  deriving Show

getDomain :: Text -> Either LoginError Text
getDomain emailAddress = case (splitOn "@" emailAddress) of
                           [name,domain] -> Right domain
                           _ -> Left InvalidEmail
