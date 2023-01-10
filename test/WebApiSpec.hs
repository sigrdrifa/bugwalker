{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module WebApiSpec 
    (
        webApiSpec
    ) where

import BugWalkerServer (server)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON


webApiSpec :: Spec
webApiSpec = undefined
{-
    with (return server) $ do
    describe "GET /fail" $ do
        it "responds with 404" $ do
            get "/fail" `shouldRespondWith` 404
    
    describe "GET /api/builds" $ do
        it "responds with 200" $ do
            get "/api/builds" `shouldRespondWith` 200
            -}