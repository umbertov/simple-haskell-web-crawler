{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where


import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.BoundedChan
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.State.Lazy

import Data.Set (Set)
import qualified Data.Set as Set

import System.Random (randomRIO)

import Data.List
import Text.HTML.Scalpel


data SchedulerState = SchedulerState 
    { urlChan      :: BoundedChan URL -- per leggere i nuovi URL
    , visitedSet   :: Set URL         -- contiene URL gia' visitati
    }


-- inizializza canale e insieme di pagine gia' visitate, poi fa partire tutto
initiator :: [URL] -> IO ((), SchedulerState)
initiator seeds = do urlsChan <- newBoundedChan 400 -- canale
                     let visited = Set.empty :: Set URL
                     forM_ seeds (writeChan urlsChan) -- scrivi i seed url nel canale
                     runStateT scheduler (SchedulerState urlsChan visited)

sleep n = threadDelay (n * (10^6))

scheduler :: StateT SchedulerState IO ()
scheduler = do
    -- ottieni lo stato corrente
    s <- get
    let chan = urlChan s
        visited = visitedSet s
    -- ottieni il prossimo URL da (eventualmente) crawlare
    url <- liftIO $! readChan chan
    -- crawla URL se non gia' visitato
    when (not (url `Set.member` visited)) $ do
        -- parte il thread
        liftIO $ runWorker url chan visited 
        -- marca URL come gia' visitato e aggiorna lo stato
        put $ s { visitedSet = Set.union visited (Set.singleton url) }
    scheduler -- repeat
    where 
        runWorker url chan visited = do 
            putStrLn url
            forkIO (worker chan url)

incMVar v = do
    n <- takeMVar v
    putMVar v (n+1)

decMVar v = do
    n <- takeMVar v
    putMVar v (n-1)

worker :: BoundedChan URL -> URL -> IO ()
worker chan url = do
    -- estrai una lista di Maybe URL
    urls <- scrapeURL url (chroots (tagSelector "a") getHref)
    -- Se url e' Nothing printErr, altrimenti scrivi gli url nel canale
    maybe printErr writeUrls urls 
    return ()
    where
        -- scrive nel canale tutti gli URL trovati nella pagina corrente
        writeUrls urls = do forM urls (writeChan chan)
                            return () -- senno' i tipi si incazzano
        printErr = putStrLn $ "[ERROR] Could not scrape " ++ (show url)
        -- getHref estrae l'attributo href da una pagina HTML
        getHref = do
            u <- attr "href" anySelector

            if (isRelativeUrl u) then
                return $ (baseUrl url) ++ u
            else
                return u

isRelativeUrl !u = (not (null u)) && head u == '/'

isIgnoredUrl !u = (null u) || head u == '#'

baseUrl :: URL -> URL
baseUrl !u = case findIndices (=='/') u of
              [] -> u
              (_:_:index:xs) -> take (index+1) u

main = do
    initiator ["https://en.wikipedia.org/"]
    return ()
