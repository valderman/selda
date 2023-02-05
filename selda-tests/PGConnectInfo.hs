{-# LANGUAGE OverloadedStrings #-}
module PGConnectInfo where
import Database.Selda.PostgreSQL
pgConnectInfo = "postgres" `on` "localhost" `auth` ("postgres", "password")
