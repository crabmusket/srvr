{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (pack)
import qualified Options.Applicative as O
import System.Exit (exitFailure)
import qualified Web.Scotty as S

main :: IO ()
main = do
    opts <- O.execParser optionParser
    S.scotty (getPort opts) (app opts)
    where
        optionParser = O.info (O.helper <*> getOptions) $
            O.header "server" <>
            O.progDesc "Run a simple web server that displays your chosen message." <>
            O.fullDesc

app :: Options -> S.ScottyM ()
app opts = do
    S.get "/" (S.html $ pack $ getMessage opts)
    when (getKillable opts) $
        S.post "/kill" (liftIO exitFailure)

data Options = Options
    { getPort :: Int
    , getMessage :: String
    , getKillable :: Bool
    } deriving Show

getOptions :: Parser Options
getOptions = Options
    <$> portOption
    <*> messageOption
    <*> killOption
    where
        portOption = O.option O.auto $
            O.metavar "PORT" <>
            O.long "port" <>
            O.short 'p' <> 
            O.value 80 <>
            O.help "Port to bind to"
        messageOption = O.strOption $
            O.metavar "MSG" <>
            O.long "message" <>
            O.short 'm' <>
            O.value "Beam me up, Scotty!" <>
            O.help "Message to display at root route"
        killOption = O.option O.auto $
            O.metavar "KILLABLE" <>
            O.long "killable" <>
            O.short 'k' <> 
            O.value False <>
            O.help "Allow crashing the server using POST /kill"
