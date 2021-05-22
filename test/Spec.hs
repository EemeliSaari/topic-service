module Main where

import System.Exit (exitFailure)
import Control.Monad.State

import Topics.Lda as Lda
import Topics.Vocab


main = do
    putStrLn "This test always fails!"
    --exitFailure
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

    let specs = Lda.ldaSpecDefaults { ntopics = 3
                                    , nterms = numTerms vocab
                                    , passes = 10
                                    }

    model <- Lda.initLda specs

    print (evalState (Lda.fit docs) model)
