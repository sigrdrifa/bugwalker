{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Test 
    (
        test
    ) where

--
import Test.Hspec
--
import WebApiSpec
--

test :: IO ()
test = hspec webApiSpec
