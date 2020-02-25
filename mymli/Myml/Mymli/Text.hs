{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Myml.Mymli.Text
  ( mymliGreetingText
  , mymliHelpText
  , mymliByeText
  )
where

import           NeatInterpolation
import           Data.Text
import           Paths_myml                     ( version )
import           Data.Version                   ( showVersion )

mymlVersionText :: Text
mymlVersionText = pack (showVersion version)

mymliGreetingText :: Text
mymliGreetingText = [trimming|
  mymli, version ${mymlVersionText}: https://github.com/linyinfeng/myml/  :help for help
  |]

mymliHelpText :: Text
mymliHelpText = [trimming|
  Commands available from the prompt:
    <term>              Evaluate <term>
    <x> = <term>        Evaluate <term> and add binding
    :exit               Exit
    :quit               Same as :exit
    :help               Show help
    :store              Show current store
    :bindings value     Show value bindings
    :bindings term      Show term bindings
    :bindings type      Show type bindings
    :input              Multi-line input
    :load "<filename>"  Load file
  |]

mymliByeText :: Text
mymliByeText = "Leaving mymli."
