--
-- | Hello world plugin
--
module Plugins.Hello (theModule) where

import Lambdabot

newtype HelloModule = HelloModule ()

theModule :: MODULE
theModule = MODULE $ HelloModule ()

instance Module HelloModule where
    moduleHelp   _ _ = return "hello world plugin"
    commands     _ = return ["hello","goodbye"]
    process      _ _ target _ rest = ircPrivmsg target ("Hello world. " ++ rest)

