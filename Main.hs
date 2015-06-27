{-# LANGUAGE OverloadedStrings #-}

import Data.Text.Lazy
import Options.Applicative
import qualified Web.Scotty as S

main = do
    opts <- execParser optionParser
    S.scotty (port opts) $ app opts
    where
        optionParser = info (helper <*> getOptions) $
            fullDesc <>
            progDesc "Run a simple web server that displays your chosen message." <>
            header "server"

app opts = do
    S.get "/" $ do
        S.html $ pack $ message opts

data Options = Options {
    port :: Int,
    message :: String
 } deriving Show

getOptions :: Parser Options
getOptions = Options
    <$> portOption
    <*> messageOption
    where
        portOption =  option auto $
            metavar "PORT" <>
            long "port" <>
            short 'p' <> 
            value 80 <>
            help "Port to bind to"
        messageOption = strOption $
            metavar "MSG" <>
            long "message" <>
            short 'm' <>
            value "Beam me up, Scotty!" <>
            help "Message to display at root route"
