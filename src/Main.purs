module Main where

import Prelude
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.Either (Either)
import Options.Applicative as O

-- import Repl (runRepl)
import Builtin (builtinLibrary)
import Interpret (Value, eval')

main :: Effect Unit
main = O.execParser opts >>= handleCmd
    where
      opts = O.info (cmd O.<**> O.helper)
          ( O.fullDesc
          <> O.progDesc "A Simmer interpreter"
          <> O.header "Only works with --file command currently" )

runFile :: String -> Effect (Either String Value)
runFile filename = do
    text <- readTextFile UTF8 filename
    eval' [ builtinLibrary ] text

handleCmd :: Cmd -> Effect Unit
handleCmd (File file) = (runFile file) *> pure unit

data Cmd = File String

cmd :: O.Parser Cmd
cmd = File
    -- Want this to be an argument
    <$> O.strOption
    (O.long "file"
    <> O.metavar "FILE"
    <> O.help "The file to run")
