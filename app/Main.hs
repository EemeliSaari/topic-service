module Main where
import Control.Monad.State

import Topics.Lda as Lda
import Topics.Vocab

import Debug.Trace


main :: IO ()
main = do
    let corpus = ["eat turkey on turkey day holiday",
                  "i like to eat cake on holiday",
                  "turkey trot race on thanksgiving holiday",
                  "snail race the turtle",
                  "time travel space race",
                  "movie on thanksgiving",
                  "movie at air and space museum is cool movie",
                  "aspiring movie star"]

    let tokens = map words corpus --Should come pre-tokenized
    let vocab = buildVocab tokens
    let docs = prepareTokens vocab tokens
    putStrLn ("nTerms: " ++ show (numTerms vocab))

    let specs = Lda.ldaSpecDefaults { ntopics = 2
                                    , nterms = numTerms vocab
                                    , passes = 10
                                    }

    model <- Lda.initLda specs
    result <- evalStateT (Lda.fit docs) model
    print result
