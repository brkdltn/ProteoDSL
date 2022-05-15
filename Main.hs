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
-- findLargestSpectra :: [Record] -> Record
-- findLargestSpectra items = foldl1 (\a x -> if s x > s a then x else a) items

-- s [_,_,_,_,_,_,s,_,_,_,_,_] = toInt s

-- toInt :: String -> Int                               
-- toInt = read

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
      let fileName = "UntreatedTest.csv"
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
  printProteins
  -- printCombinedIsoform

  -- let fileName = "UntreatedTest.csv"
  -- input <- readFile fileName
  -- let csv = parseCSV fileName input
  -- either print getBySomething csv
    -- where listOfContaminants = "(P02769) Serum albumin precursor (Allergen Bos d 6) (BSA)","(P00766) Chymotrypsinogen A (EC 3.4.21.1) [Contains: Chymotrypsin A chain A; Chymotrypsin A chain B; Chymotrypsin A chain C]","(P00767) Chymotrypsinogen B (EC 3.4.21.1) [Contains: Chymotrypsin B chain A; Chymotrypsin B chain B; Chymotrypsin B chain C]","(P00711) Alpha-lactalbumin precursor (Lactose synthase B protein) (Allergen Bos d 4)","(Q7M135) Lysyl endopeptidase (EC 3.4.21.50) (Lys-C)","(P00792) Pepsin A precursor (EC 3.4.23.1)","(P00791) Pepsin A precursor (EC 3.4.23.1)","(Q10735) Pepsin B precursor (EC 3.4.23.2) (Parapepsin I) (Fragment)","(P30879) Gastricsin precursor (EC 3.4.23.3) (Pepsinogen C) (Fragment)","(P04188) Glutamyl endopeptidase precursor (EC 3.4.21.19) (Staphylococcal serine proteinase) (V8 protease) (V8 proteinase) (Endoproteinase Glu-C)","(P00760) Cationic trypsin precursor (EC 3.4.21.4) (Beta-trypsin) [Contains: Alpha-trypsin chain 1; Alpha-trypsin chain 2] (Fragment)","(Q29463) Anionic trypsin precursor (EC 3.4.21.4)","(P00761) Trypsin precursor (EC 3.4.21.4)","(P02662) Alpha-S1-casein precursor","(P02663) Alpha-S2-casein precursor [Contains: Casocidin-1 (Casocidin-I)]","(P02666) Beta-casein precursor","(P02668) Kappa-casein precursor [Contains: Casoxin C; Casoxin 6; Casoxin A; Casoxin B; Casoplatelin]","(P42212) Aequorea victoria Green fluorescent protein","(P81054) Peptidyl-Lys metalloendopeptidase precursor - Grifola frondosa (LysN)","(P04745) Salivary alpha-amylase precursor (EC 3.2.1.1) (1,4-alpha-D-glucan glucanohydrolase)","(P13645) Keratin, type I cytoskeletal 10 (Cytokeratin-10) (CK-10) (Keratin-10) (K10)","(P35527) Keratin, type I cytoskeletal 9 (Cytokeratin-9) (CK-9) (Keratin-9) (K9)","(P04264) Keratin, type II cytoskeletal 1 (Cytokeratin-1) (CK-1) (Keratin-1) (K1) (67 kDa cytokeratin) (Hair alpha protein)","(P35908) Keratin, type II cytoskeletal 2 epidermal (Cytokeratin-2e) (K2e) (CK 2e)","(Q15323) Keratin, type I cuticular Ha1 (Hair keratin, type I Ha1)","(Q14532) Keratin, type I cuticular Ha2 (Hair keratin, type I Ha2)","(O76011) Keratin, type I cuticular Ha4 (Hair keratin, type I Ha4)","(Q92764) Keratin, type I cuticular Ha5 (Hair keratin, type I Ha5)","(O76013) Keratin, type I cuticular Ha6 (Hair keratin, type I Ha6)","(O76014) Keratin, type I cuticular Ha7 (Hair keratin, type I Ha7)","(O76015) Keratin, type I cuticular Ha8 (Hair keratin, type I Ha8)","(O76009) Keratin, type I cuticular Ha3-I (Hair keratin, type I Ha3-I)","(Q14525) Keratin, type I cuticular Ha3-II (Hair keratin, type I Ha3-II)","(Q14533) Keratin, type II cuticular Hb1 (Hair keratin, type II Hb1) (ghHKb1) (ghHb1) (MLN 137)","(Q9NSB4) Keratin, type II cuticular Hb2 (Hair keratin, type II Hb2)","(P78385) Keratin, type II cuticular Hb3 (Hair keratin, type II Hb3)","(Q9NSB2) Keratin, type II cuticular Hb4 (Hair keratin, type II Hb4)","(P78386) Keratin, type II cuticular Hb5 (Hair keratin, type II Hb5)","(O43790) Keratin, type II cuticular Hb6 (Hair keratin, type II Hb6) (ghHb6)","(O77727) Keratin, type I cytoskeletal 15 (Cytokeratin-15) (CK-15) (Keratin-15) (K15)","(P02534) Keratin, type I microfibrillar 48 kDa, component 8C-1 (Low-sulfur keratin)","(P25690) Keratin, type I microfibrillar, 47.6 kDa (Low-sulfur keratin)","(P02539) Keratin, type II microfibrillar (Low-sulfur keratin) (Fragment)","(P15241) Keratin, type II microfibrillar, component 7C","(P25691) Keratin, type II microfibrillar, component 5","(P02444) Keratin, high sulfur matrix protein, IIIB3","(P02445) Keratin, high sulfur matrix protein, IIIB4","(P02443) Keratin, high-sulfur matrix protein, IIIA3A","(P02441) Keratin, high-sulfur matrix protein, IIIA3","(Q02958) Keratin, glycine/tyrosine-rich of hair","(P02438) Keratin, high-sulfur matrix protein, B2A","(P02439) Keratin, high-sulfur matrix protein, B2B","(P02440) Keratin, high-sulfur matrix protein, B2C","(P08131) Keratin, high-sulfur matrix protein, B2D","(P26372) Keratin, ultra high-sulfur matrix protein (UHS keratin)","(O82803) Small rubber particle protein - Hevea brasiliensis","(P15252) Rubber elongation factor protein - Hevea brasiliensis","(P00004) Horse heart cytochrome C","(P00921) Bovine carbonic anhydrase 2 (EC 4.2.1.1)","(P00330) Yeast alcohol dehydrogenase 1","(P00883) Rabbit fructose-bisphosphate aldolase A","(P00698) Hen egg lysozyme C precursor","(P68082) Horse heart myoglobin","(P01012) Hen egg ovalbumin","(P00722) E. coli beta-galactosidase (E.C. 3.2.1.23)","(P00366) Bovine glutamate dehydrogenase 1, mitochondrial precursor (E.C. 1.4.1.3)","(P02768) Serum albumin 66,393 Recombinant Pichia pastoris","(P01008) Antithrombin-III 49,033 Plasma Glycosylation","(P08758) Annexin A5 35,782 Placenta Acetylation","(P61769) Beta-2-microglobulin 11,729 Urine","(P55957) BH3 interacting domain death agonist [BID] 21,978 Recombinant E. coli","(P00915) Carbonic anhydrase 1 28,738 Erythrocytes Acetylation","(P00918) Carbonic anhydrase 2 29,095 Erythrocytes Acetylation","(P04040) Catalase 59,583 Erythrocytes","(P07339) Cathepsin D 26,624 Liver Glycosylation","(P08311) Cathepsin G 26,751 Sputum Glycosylation","(P01031) Complement C5 [Complement C5a] 8,266 Recombinant E. coli","(P02741) C-reactive protein 23,030 Plasma","(P00167) Cytochrome b5 16,021 Recombinant E. coli 6-His","(P99999) Cytochrome c [Apocytochrome c] 11,608 Recombinant E. coli","(P01133) Epidermal growth factor 6,211 Recombinant E. coli","(P05413) Fatty acid-binding protein 14,716 Plasma Acetylation,Phosphorylation","(P06396) Gelsolin 82,954 Plasma Phosphorylation","(P08263) Glutathione S-transferase A1 [GST A1-1] 25,482 Recombinant E. coli","(P09211) Glutathione S-transferase P [GST] 23,220 Placenta","(P69905) Hemoglobin alpha chain 15,127 Erythrocytes","(P68871) Hemoglobin beta chain 15,867 Erythrocytes,Acetylation,Nitrosylation,Glycosylation","(P01344) Insulin-like growth factor II 7,464 Recombinant E. coli","(P10145) Interleukin-8 8,381 Recombinant E. coli","(P06732) Creatine kinase M-type [CK-MM] 43,070 Heart","(P00709) Alpha-lactalbumin 14,070 Milk Glycosylation","(P41159) Leptin 16,024 Recombinant E. coli","(P61626) Lysozyme C 14,692 Milk","(P02144) Myoglobin 17,051 Heart","(Q15843) Neddylin [Nedd8] 9,071 Recombinant E. coli","(P15559) NAD(P)H dehydrogenase [quinone] 1[DT Diaphorase] 30,984 Recombinant E. coli","(P16083) Ribosyldihydronicotinamide dehydrogenase(quinone) [Quinone oxidoreductase 2 or NQO2] 25,817 Recombinant E. coli","(P01127) Platelet-derived growth factor B chain 12,286 Recombinant E. coli","(P62937) Peptidyl-prolyl cis-trans isomerase A [Cyclophilin A] 17,947 Recombinant E. coli","(Q06830) Peroxiredoxin 1 22,106 Recombinant E. coli","(P01112) GTPase HRas [Ras protein] 21,292 Recombinant E. coli","(P02753) Retinol-binding protein 21,065 Urine","(P00441) Superoxide dismutase [Cu-Zn] 15,800 Erythrocytes Acetylation","(P63165) Small ubiquitin-related modifier 1 [SUMO-1] 37,420 Recombinant E. coli GST","(P12081) Histidyl-tRNA synthetase [Jo-1] 58,223 Recombinant E. coli","(P10636) Microtubule-associated protein tau [Tau protein] 46,810 Recombinant E. coli 6-His","(P10599) Thioredoxin 12,424 Recombinant E. coli 6-His","(P01375) Tumor necrosis factor [TNF-alpha] 17,350 Recombinant E. coli","(P02787) Serotransferrin [Apotransferrin] 75,143 Plasma Glycosylation","(P02788) Lactotransferrin 78,289 Milk Glycosylation","(P51965) Ubiquitin-conjugating enzyme E2 E1 [UbcH6]","(O00762) Ubiquitin-conjugating enzyme E2 C [UbcH10] 20,473 Recombinant E. coli 6-His","(P63279) Ubiquitin-conjugating enzyme E2 I [UbcH9] 22,907 Recombinant E. coli","(P62979) Ubiquitin-40S ribosomal protein S27a 9,387 Recombinant E. coli 6-His","(P32503) Major coat protein, Saccharomyces cerevisiae virus L-A (ScV-L-A) (ScVL1)"

  -- either print printSpectra csv
  -- either print printCombinedIsoform csv
  -- either print listCombinedIsoform csv
  -- either print (print.findLargestSpectra.tail) csv
