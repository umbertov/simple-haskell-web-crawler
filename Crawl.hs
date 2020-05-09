{-# LANGUAGE OverloadedStrings #-}

module Main where


import Control.Concurrent
import Control.Monad

import Data.Set (Set)
import qualified Data.Set as Set

import System.Random (randomRIO)

import Data.List
import Text.HTML.Scalpel



-- inizializza canale e insieme di pagine gia' visitate, poi fa partire tutto
initiator :: [URL] -> IO ()
initiator seeds = do urlsC <- newChan -- canale
                     let visited = Set.empty :: Set URL
                     forM_ seeds (writeChan urlsC) -- scrivi i seed url nel canale
                     scheduler visited urlsC

-- legge un URL dal canale, e lo scrapa se non e' gia' visitato.
scheduler :: Set URL -> Chan URL -> IO ()
scheduler visited chan = do 
    url <- readChan chan
    if not (url `Set.member` visited)
       then do putStrLn url
               forkIO (worker chan url) -- parte il thread
               scheduler (Set.union visited (Set.singleton $ url)) chan 
        else
            scheduler visited chan


worker :: Chan URL -> URL -> IO ()
worker chan url = do
    -- estrai una lista di Maybe URL
    urls <- scrapeURL url (chroots (tagSelector "a") getHref)
    -- Se url e' Nothing printErr, else scrivi gli url nel canale
    maybe printErr writeUrls urls 
    return ()
    where
        writeUrls :: [URL] -> IO () -- scrive l'argomento nel canale
        writeUrls urls = do forM urls (writeChan chan)
                            return () -- senno' i tipi si incazzano
        printErr = putStrLn $ "[ERROR] Could not scrape " ++ (show url)
        -- getHref estrae l'attributo href
        getHref = do
            u <- attr "href" anySelector

            if ((not $null u) && head u == '/') then
                return $ (baseUrl url) ++ u
            else
                return u


baseUrl :: URL -> URL
baseUrl u = case findIndices (=='/') u of
              [] -> u
              (_:_:index:xs) -> take (index+1) u

main = do
    initiator ["https://en.wikipedia.org/"]

