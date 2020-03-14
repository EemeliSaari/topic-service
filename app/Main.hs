module Main where
import Control.Monad.State

import Topics.Lda as Lda
import Topics.Vocab
import Topics.Sampling


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

    let specs = Lda.LdaSpec { ntopics = 2
                            , alpha = 0.1
                            , eta = 0.5
                            , decay = 0.1
                            , iter = 10
                            , passes = 5
                            , nterms = numTerms vocab
                            }

    let model = Lda.initLda specs in do
        print (evalState (Lda.update docs 0) model)
