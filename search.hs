module Main where

import Control.Monad.State
import Data.Tree
import Data.Tree.Pretty

data Dir = L | R | Found deriving (Show, Eq)

tree :: Tree String
tree = Node "1" [ Node "2" [ Node "4" []
                           , Node "5" [ Node "8" []
                                      , Node "9" []
                                      ]
                           ]
                , Node "3" [ Node "6" [ Node "10" []
                                     , Node "11" []
                                     ]
                           , Node "7" [ Node "12" []
                                       , Node "13" []
                                       ]
                           ]
                ]

depthFirst :: String -> Tree String -> State [Dir] ()
depthFirst n (Node x []) = do
    s <- get
    put (if n == x then s ++ [Found] else s)

depthFirst n (Node x (l:r:_))
    | n == x = do
        s <- get
        put (s ++ [Found])
    | otherwise = do
        s <- get
        when (Found `notElem` s) $ do
            put (s ++ [L])
            depthFirst n l
        s' <- get
        when (Found `notElem` s') $ do
            put (s ++ [R])
            depthFirst n r

main :: IO ()
main = do
    putStrLn $ drawVerticalTree tree
    print "Which number? "
    n <- getLine
    print $ execState (depthFirst n tree) []
    main
