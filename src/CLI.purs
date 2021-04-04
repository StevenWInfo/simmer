module Simmer.CLI where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.Either (Either(..))
import Options.Applicative ((<**>))
import Options.Applicative as O

-- import Repl (runRepl)
import Interpret (Value, eval', Library)

cli :: Array Library -> Effect Unit
cli lib = O.execParser opts >>= (handleCmd lib)
    where
      opts = O.info (cmd <**> O.helper)
          ( O.fullDesc
          <> O.progDesc "A Simmer interpreter"
          <> O.header "Only works with --file command currently" )

-- With spago do `spago run --exec-args "--file FILE"`
runFile :: Array Library -> String -> Effect (Either String Value)
runFile lib filename = do
    text <- readTextFile UTF8 filename
    eval' lib text

handleCmd :: Array Library -> Cmd -> Effect Unit
handleCmd lib (File file) = do
    result <- (runFile lib file)
    case result of
        Left msg -> log msg
        Right val -> log $ "Final value was: " <> show val

data Cmd = File String

cmd :: O.Parser Cmd
cmd = File
    -- Want this to be an argument
    <$> O.strOption
    (O.long "file"
    <> O.metavar "FILE"
    <> O.help "The file to run")
