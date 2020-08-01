{-# LANGUAGE OverloadedStrings #-}
module PGConnectInfo where
import Database.Selda.PostgreSQL
pgConnectInfo = "test" `on` "localhost" `auth` ("postgres", "password")
