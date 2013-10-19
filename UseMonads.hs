module UseMonads where

import Control.Monad.Identity

instance Show a => Show (Identity a) where
 show (Identity x) = show x

example1 = do { x <- [1,2]; y<-[3,4]; return (x+y)}

example2 = do { x <- Just 1; y<- Just 2; return (x+y)}

example3 = do { x <- Just 1; y<- Nothing; return (x+y)}

example4 = do { x <- Nothing ; y<- Just 2; return (x+y)}

example5 = do {x<-return 'c':: Identity Char; y<-return 'd';return [x,y]}

example6 = do {x<-return 'c':: Maybe Char; y<-return 'd';return [x,y]}

example7 = do {x<-return 'c':: IO Char; y<-return 'd';return [x,y]}

example8 = do {x<-return 'c':: [Char]; y<-return 'd';return [x,y]}



