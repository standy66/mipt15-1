{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Network.HTTP.Conduit
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import Network (withSocketsDo)

(email, name) = ("karn9050@gmail.com", encodeUtf8 "Степанов А.В.") -- адрес почты и фамилия с инициалами

factorial :: Int -> Int
factorial 0 = 1
factorial n = product [1..n]

pascal :: Int -> Int -> Int
pascal m n = (factorial n) `quot` ((factorial $ (n - m)) * (factorial m))
printIt :: Int -> C.ByteString
printIt n = C.pack $ show $ [pascal y x | x <- [0..n], y <- [0..x]]

main :: IO()
main = 
  withSocketsDo $ do
  initReq <- parseUrl "http://mipt.eu01.aws.af.cm/lab0"
  let req = urlEncodedBody [("email", email), ("name", name), ("lang", "haskell"), ("content", printIt 20)] $ initReq { method = "POST" }
  response <- withManager $ httpLbs req
  L.putStr $ responseBody response
