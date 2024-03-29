{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Myml.Mymli.Text
  ( mymliGreetingText,
    mymliHelpText,
    mymliByeText,
    mymlVersionText,
    mymlOptionHelpHeaderString,
  )
where

import Data.Text
import Data.Version (showVersion)
import NeatInterpolation
import Paths_myml (version)

mymlVersionText :: Text
mymlVersionText = pack (showVersion version)

mymliGreetingText :: Text
mymliGreetingText =
  [trimming|
  mymli, version ${mymlVersionText}: https://github.com/linyinfeng/myml/  :help for help
  |]

mymlOptionHelpHeaderString :: String
mymlOptionHelpHeaderString =
  unpack
    [trimming|
  mymli ${mymlVersionText}, Lin Yinfeng <lin.yinfeng@outlook.com>
  |]

mymliHelpText :: Text
mymliHelpText =
  [trimming|
  Commands available from the prompt:
    <term> ;;                     Evaluate <term>
    <x> <params> ... = <term> ;;  Evaluate <term> and add binding
    :quit                         Quit myml
    :help                         Show this help message
    :store                        Show current store
    :bindings value               Show value bindings
    :bindings term                Show term bindings
    :bindings type                Show type bindings
  |]

mymliByeText :: Text
mymliByeText = "Leaving mymli."
