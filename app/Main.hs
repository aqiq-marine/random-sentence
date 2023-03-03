module Main (main) where

-- import Lib
import System.Random
import System.IO
import Numeric

data Noun = N | R2N Noun Noun| R3N Noun Noun Noun
data Clause = R1V Noun | R2V Noun Noun | R3V Noun Noun Noun
data Sentence = S Clause | R2Conj Clause Clause | R3Conj Clause Clause Clause

instance Show Noun where
    show obj = case obj of
        N -> "N"
        R2N n1 n2 -> showWithSpace ["R2", show n1, show n2]
        R3N n1 n2 n3 -> showWithSpace ["R3", show n1, show n2, show n3]

instance Show Clause where
    show clause = case clause of
        R1V n -> "V1" ++ " " ++ show n
        R2V n1 n2 -> showWithSpace ["V2", show n1, show n2]
        R3V n1 n2 n3 -> showWithSpace ["V3", show n1, show n2, show n3]

instance Show Sentence where
    show sentence = case sentence of
        S c -> show c
        R2Conj c1 c2 -> showWithSpace ["Conj2", show c1, show c2]
        R3Conj c1 c2 c3 -> showWithSpace ["Conj3", show c1, show c2, show c3]


showWith :: String -> [String] -> String
showWith word list = case list of
    [] -> ""
    [str] -> str
    str:rest -> str ++ word ++ showWith word rest

showWithSpace :: [String] -> String
showWithSpace = showWith " "

-- どの名詞がどの動詞を扱えるかを決定する
-- 例えば名詞番号nについてn%2=0なら二類名詞n%3=0なら三類n%5なら五類
-- 動詞番号mについてm%8=0ならぜロ類m%8=1なら一類みたいな感じにして
-- 二類名詞は一類動詞を取れるみたいな
-- もちろん1-17類名詞もあるから、そしたらすべての動詞を取れる

nounMaker :: RandomGen g => g -> (Noun, g)
nounMaker gen = (noun, g_next)
    where
        rand :: Float
        (rand, g1) = random gen
        -- 閾値によっては期待値が発散するので注意
        x = 1/6
        y = 1/6
        -- 2 * x + 3 * y < 1
        (noun, g_next)
            | rand > x + y = (N, g1)
            | rand > y =
                let (n1, g2) = nounMaker g1
                    (n2, g3) = nounMaker g2
                in (R2N n1 n2, g3)
            | otherwise =
                let (n1, g2) = nounMaker g1
                    (n2, g3) = nounMaker g2
                    (n3, g4) = nounMaker g3
                in (R3N n1 n2 n3, g4)

nounMakerInf :: RandomGen g => g -> [Noun]
nounMakerInf gen = a_noun : rest_nouns
    where
        (a_noun, g2) = nounMaker gen
        rest_nouns = nounMakerInf g2

clauseMaker :: RandomGen g => g -> (Clause, g)
clauseMaker gen = (clause, g4)
    where
        rand :: Float
        (rand, g1) = random gen
        (n1, g2) = nounMaker g1
        (n2, g3) = nounMaker g2
        (n3, g4) = nounMaker g3
        clause
            | rand > 0.9 = R1V n1
            | rand > 0.3 = R2V n1 n2
            | otherwise = R3V n1 n2 n3

clauseMakerInf :: RandomGen g => g -> [Clause]
clauseMakerInf gen = c:rest
    where
        (c, g1) = clauseMaker gen
        rest = clauseMakerInf g1

sentenceMaker :: RandomGen g => g -> (Sentence, g)
sentenceMaker gen = (sentence, g4)
    where
        rand :: Float
        (rand, g1) = random gen
        (c1, g2) = clauseMaker g1
        (c2, g3) = clauseMaker g2
        (c3, g4) = clauseMaker g3
        sentence
            | rand > 0.4 = S c1
            | rand > 0.1 = R2Conj c1 c2
            | otherwise = R3Conj c1 c2 c3

sentenceMakerInf :: RandomGen g => g -> [Sentence]
sentenceMakerInf gen = s1 : rest
    where
        (s1, g1) = sentenceMaker gen
        rest = sentenceMakerInf g1


data NounTrait = One | Two | Three | Four | Five | Six | Seven | Eight deriving Show

getTrait :: Int -> [NounTrait]
getTrait num = traits
    where
        has_trait (p, t) acc = if mod num p == 0 then t:acc else acc
        traits = foldr has_trait [] [
            (1, One), (2, Two), (3, Three),
            (5, Four), (7, Five), (11, Six),
            (13, Seven), (17, Eight)]

trait2p :: NounTrait -> Int
trait2p trait = case trait of
    One -> 1
    Two -> 2
    Three -> 3
    Four -> 5
    Five -> 7
    Six -> 11
    Seven -> 13
    Eight -> 17

randomRInvProp :: RandomGen g => Int -> g -> (Int, g)
randomRInvProp end gen = (index, g1)
    where
        invprop = map (1.0/) [1..(fromIntegral end :: Float)]
        sum_all = sum invprop
        cum = (++ [100]) . init . scanl (+) 0 . map (/sum_all) $ invprop
        rand :: Float
        (rand, g1) = random gen
        index = fst . head . dropWhile (\(_, c) -> c < rand) $ zip [1..] cum


genNounFromTrait :: RandomGen g => g -> NounTrait -> (Int, g)
genNounFromTrait gen trait = (num, g1)
    where
        word_size = 1024
        p = trait2p trait
        max_k = div word_size p
        (index, g1) = randomRInvProp max_k gen
        num = p * index

num2strNoun :: Int -> String
num2strNoun num = 'n' : showHex num ""

genComplexNoun :: RandomGen g => g -> NounTrait -> Noun -> ([String], g)
genComplexNoun gen trait noun = case noun of
    N -> ([num2strNoun n], g1)
        where
            (n, g1) = genNounFromTrait gen trait
    R2N noun1 noun2 -> ([r2] ++ n1 ++ n2, g3)
        where
            r2n :: Int
            (r2n, g1) = randomR (1, 16) gen
            r2 = "r2" ++ showHex r2n ""
            (n1, g2) = genComplexNoun g1 trait noun1
            n2trait = if mod r2n 4 == 0 then trait else One
            (n2, g3) = genComplexNoun g2 n2trait noun2
    R3N noun1 noun2 noun3 -> ([r3] ++ n1 ++ n2 ++ n3, g4)
        where
            r3n :: Int
            (r3n, g1) = randomR (1, 16) gen
            r3 = "r3" ++ showHex r3n ""
            (n1, g2) = genComplexNoun g1 trait noun1
            n23trait = Two
            (n2, g3) = genComplexNoun g2 n23trait noun2
            (n3, g4) = genComplexNoun g3 n23trait noun3

genClause :: RandomGen g => g -> Clause -> ([String], g)
genClause gen clause = case clause of
    R1V noun1 -> (v1 : n, g2)
        where
            (v1n, g1) = randomRInvProp 100 gen
            v1 = "v1" ++ showHex v1n ""
            trait
                | even v1n = Two
                | mod v1n 3 == 0 = Three
                | mod v1n 5 == 0 = Four
                | otherwise = One
            (n, g2) = genComplexNoun g1 trait noun1
    R2V noun1 noun2 -> (v2 : n1 ++ n2, g3)
        where
            (v2n, g1) = randomRInvProp 600 gen
            v2 = "v2" ++ showHex v2n ""
            (trait1, trait2)
                | even v2n = (Two, One)
                | mod v2n 3 == 0 = (Eight, Two)
                | otherwise = (Six, One)
            (n1, g2) = genComplexNoun g1 trait1 noun1
            (n2, g3) = genComplexNoun g2 trait2 noun2
    R3V noun1 noun2 noun3 -> (v3 : n1 ++ n2 ++ n3, g4)
        where
            (v3n, g1) = randomRInvProp 300 gen
            v3 = "v3" ++ showHex v3n ""
            (trait1, trait2, trait3)
                | even v3n = (Two, Three, Three)
                | mod v3n 3 /= 0 = (One, Two, Five)
                | otherwise = (Three, One, Seven)
            (n1, g2) = genComplexNoun g1 trait1 noun1
            (n2, g3) = genComplexNoun g2 trait2 noun2
            (n3, g4) = genComplexNoun g3 trait3 noun3

genSentence :: RandomGen g => g -> Sentence -> ([String], g)
genSentence gen sentence = case sentence of
    S clause -> genClause gen clause
    R2Conj clause1 clause2 -> (conj2 : c1 ++ c2, g3)
        where
            (conj2n, g1) = randomRInvProp 30 gen
            conj2 = "conj2" ++ showHex conj2n ""
            (c1, g2) = genClause g1 clause1
            (c2, g3) = genClause g2 clause2
    R3Conj clause1 clause2 clause3 -> (conj3 : c1 ++ c2 ++ c3, g4)
        where
            (conj3n, g1) = randomRInvProp 10 gen
            conj3 = "conj3" ++ showHex conj3n ""
            (c1, g2) = genClause g1 clause1
            (c2, g3) = genClause g2 clause2
            (c3, g4) = genClause g3 clause3

genNewSentence :: RandomGen g => g -> ([String], g)
genNewSentence gen = genSentence g1 frame
    where
        (frame, g1) = sentenceMaker gen

genNewSentenceInf :: RandomGen g => g -> [[String]]
genNewSentenceInf gen = s:rest
    where
        (s, g1) = genNewSentence gen
        rest = genNewSentenceInf g1

main :: IO ()
main = do
    let g1 = mkStdGen 546
    let rand_sentences = genNewSentenceInf g1
    let contents = showWith "\n" $ take 1000 . map showWithSpace $ rand_sentences
    handle <- openFile "corpus.txt" WriteMode
    hPutStrLn handle contents
    hClose handle
