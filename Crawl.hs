{-# LANGUAGE OverloadedStrings #-}

module Main where


import Control.Concurrent
import Control.Monad
import Control.Monad.State.Lazy

import Data.Set (Set)
import qualified Data.Set as Set

import System.Random (randomRIO)

import Data.List
import Text.HTML.Scalpel


data SchedulerState = SchedulerState 
    { getChan      :: Chan URL
    , getVisited   :: Set URL
    }


-- inizializza canale e insieme di pagine gia' visitate, poi fa partire tutto
initiator :: [URL] -> IO ((), SchedulerState)
initiator seeds = do urlsChan <- newChan -- canale
                     let visited = Set.empty :: Set URL
                     forM_ seeds (writeChan urlsChan) -- scrivi i seed url nel canale
                     runStateT scheduler (SchedulerState urlsChan visited)

sleep n = threadDelay (n * (10^6))

scheduler :: StateT SchedulerState IO ()
scheduler = do
    s <- get
    let chan = getChan s
        visited = getVisited s
    url <- liftIO $ readChan chan
    when (not (url `Set.member` visited)) $ do
        newVisited <- liftIO $ runWorker url chan visited
        put $ SchedulerState chan newVisited
        scheduler
    scheduler
    where 
        runWorker url chan visited= do 
            putStrLn url
            forkIO (worker chan url)
            let newVisited = Set.union visited (Set.singleton url)
            return newVisited

incMVar v = do
    n <- takeMVar v
    putMVar v (n+1)

decMVar v = do
    n <- takeMVar v
    putMVar v (n-1)

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

            if (isRelativeUrl u) then
                return $ (baseUrl url) ++ u
            else
                return u

isRelativeUrl u = (not (null u)) && head u == '/'

isIgnoredUrl u = (null u) || head u == '#'

baseUrl :: URL -> URL
baseUrl u = case findIndices (=='/') u of
              [] -> u
              (_:_:index:xs) -> take (index+1) u

main = do
    initiator ["https://en.wikipedia.org/"]
    return ()
