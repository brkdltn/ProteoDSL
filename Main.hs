module Main where

import Text.CSV (parseCSV, Record)
import Data.Map (fromListWith)
import Control.Applicative ((<|>))

data MassSpec = MassSpec { 
  proteinRank :: String,
  description :: String,
  logProb :: Maybe Float,
  bestLogProb :: Maybe Float,
  bestScore :: Maybe Float,
  totalIntensity :: Maybe Float,
  spectra :: Maybe Float,
  uniquePeptides :: Maybe Float,
  modPeptides :: Maybe Float,
  coverage :: Maybe Float,
  aa :: Maybe Float,
  proteinDBNumber :: Maybe Float
} deriving Show

-- Hmm v --
findLargestSpectra :: [Record] -> [Record]
findLargestSpectra items = filter (\x -> if s x >= show 20 then True else False) items

s [_,_,_,_,_,_,spectra,_,_,_,_,_] = spectra

-- Hmm ^ --

-- Protein Properties --
printProteins :: IO()
printProteins = do 
      let fileName = "Untreated.csv"
      input <- readFile fileName
      let csv = parseCSV fileName input
      either print listContaminants csv

listProteins :: [Record] -> IO ()
listProteins csv = print $ map parseToProteins csv

parseToProteins :: Record -> (String)
parseToProteins record = (description massSpec) 
  where massSpec = parseMassSpec record

-- Combine Isoforms --
printCombinedIsoform :: IO()
printCombinedIsoform = do 
      let fileName = "Untreated.csv"
      input <- readFile fileName
      let csv = parseCSV fileName input
      either print listCombinedIsoform csv

listCombinedIsoform :: [Record] -> IO ()
listCombinedIsoform csv = print $ fromListWith combineIsoform $ map parseToTuple csv

combineIsoform :: MassSpec -> MassSpec -> MassSpec
combineIsoform massSpec1 massSpec2 = MassSpec { proteinRank = proteinRank massSpec1
  , description = description massSpec2
  , logProb = logProb massSpec1 <|> logProb massSpec2
  , bestLogProb = bestLogProb massSpec1 <|> bestLogProb massSpec2
  , bestScore = bestScore massSpec1 <|> bestScore massSpec2
  , totalIntensity = totalIntensity massSpec1 <|> totalIntensity massSpec2
  , spectra = spectra massSpec1 <|> spectra massSpec2
  , uniquePeptides = uniquePeptides massSpec1 <|> uniquePeptides massSpec2
  , modPeptides = modPeptides massSpec1 <|> modPeptides massSpec2
  , coverage = coverage massSpec1 <|> coverage massSpec2
  , aa = aa massSpec1 <|> aa massSpec2
  , proteinDBNumber = proteinDBNumber massSpec1 <|> proteinDBNumber massSpec2
   }

-- Parse Mass Spec --
parseToTuple :: Record -> (String, MassSpec)
parseToTuple record = (proteinRank massSpec, massSpec) 
  where massSpec = parseMassSpec record

printSpectra :: [Record] -> IO ()
printSpectra csv = print $ map parseToSpectra csv

parseToSpectra:: Record -> (Maybe Float)
parseToSpectra record = (spectra massSpec) 
  where massSpec = parseMassSpec record

parseMassSpec :: Record -> MassSpec
parseMassSpec record = 
  MassSpec { proteinRank = record !! 0
       , description = record !! 1     
       , logProb = case reads (record !! 2)::[(Float,String)] of
          [(x, "")] -> Just x
          _ -> Nothing 
       , bestLogProb = case reads (record !! 3)::[(Float,String)] of
          [(x, "")] -> Just x
          _ -> Nothing 
       , bestScore = case reads (record !! 4)::[(Float,String)] of
          [(x, "")] -> Just x
          _ -> Nothing 
       , totalIntensity = case reads (record !! 5)::[(Float,String)] of
          [(x, "")] -> Just x
          _ -> Nothing 
       , spectra = case reads (record !! 6)::[(Float,String)] of
          [(x, "")] -> Just x
          _ -> Nothing 
       , uniquePeptides = case reads (record !! 7)::[(Float,String)] of
          [(x, "")] -> Just x
          _ -> Nothing 
       , modPeptides = case reads (record !! 8)::[(Float,String)] of
          [(x, "")] -> Just x
          _ -> Nothing 
       , coverage = case reads (record !! 9)::[(Float,String)] of
          [(x, "")] -> Just x
          _ -> Nothing        
       , aa = case reads (record !! 10)::[(Float,String)] of
          [(x, "")] -> Just x
          _ -> Nothing 
       , proteinDBNumber = case reads (record !! 11)::[(Float,String)] of
          [(x, "")] -> Just x
          _ -> Nothing
       }

-- Contaminant Properties --
data Contaminant = Contaminant {
  identity :: String,
  contaminant :: String,
  reason :: String
} deriving Show

printContaminants :: IO()
printContaminants = do 
      let fileName = "cRAP.csv"
      input <- readFile fileName
      let csv = parseCSV fileName input
      either print listContaminants csv

listContaminants :: [Record] -> IO ()
listContaminants csv = print $ map parseToContaminants csv

parseToContaminants :: Record -> (String)
parseToContaminants record = (contaminant contaminants) 
  where contaminants = parseContaminants record

parseContaminants :: Record -> Contaminant
parseContaminants record = 
  Contaminant { identity = record !! 0
       , contaminant = record !! 1   
       , reason = record !! 2
       }

-- Main --------------

main :: IO ()
main = do
  -- printContaminants
  -- printProteins
  -- printCombinedIsoform

   let fileName = "UntreatedTest.csv"
   input <- readFile fileName
   let csv = parseCSV fileName input

  -- either print printSpectra csv
  -- either print printCombinedIsoform csv
  -- either print listCombinedIsoform csv
   either print (print.findLargestSpectra.tail) csv
