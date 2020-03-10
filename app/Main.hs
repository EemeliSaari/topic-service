module Main where

import Topics.LDA as LDA

import Data.Map (fromList)

main :: IO ()
main = do
    let specs = LDA.LDASpec {topic_n = 2, alpha = 0.1, eta = 0.5, decay = 0.1, iter = 10}
    let corpus = ["eat turkey on turkey day holiday",
                  "i like to eat cake on holiday",
                  "turkey trot race on thanksgiving holiday",
                  "snail race the turtle",
                  "time travel space race",
                  "movie on thanksgiving",
                  "movie at air and space museum is cool movie",
                  "aspiring movie star"]
    let docs = map words corpus
    let vocab = fromList [(y, x) |(x, y) <- zip [0..] (concat docs)]
    print vocab
