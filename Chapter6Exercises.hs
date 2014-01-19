import Test.HUnit
import Test.QuickCheck
import Pictures (Picture)
import Chapter5Exercises (onSeparateLines)

-------------------------------------------------------------------------------
-- Exercise 6.1
-------------------------------------------------------------------------------

-- snd :: (a,y) -> y
-- snd (x,y) = y

-- sing :: x -> [x]
-- sing x = [x]


-------------------------------------------------------------------------------
-- Exercise 6.2
-------------------------------------------------------------------------------

-- There are types for id which are not intances of [[a]]->[[a]]

-------------------------------------------------------------------------------
-- Exercise 6.3
-------------------------------------------------------------------------------

-- shift :: ((x,y),z) -> (x,(y,z))

-------------------------------------------------------------------------------
-- Exercise 6.4
-------------------------------------------------------------------------------

superimposeChar :: Char -> Char -> Char
superimposeChar '.' '.' = '.'
superimposeChar _   _   = '#'

superimposeCharTest = TestList
    [ TestCase (assertEqual "" '.' (superimposeChar '.' '.'))
    , TestCase (assertEqual "" '#' (superimposeChar '.' '#'))
    , TestCase (assertEqual "" '#' (superimposeChar '#' '.'))
    , TestCase (assertEqual "" '#' (superimposeChar '#' '#'))
    ]


-------------------------------------------------------------------------------
-- Exercise 6.5
-------------------------------------------------------------------------------

superimposeLine :: [Char] -> [Char] -> [Char]
superimposeLine a b = [ superimposeChar a b | (a,b) <- zip a b ]

superimposeLineTest
    = TestCase (assertEqual "" ".###" (superimposeLine ".##." ".#.#"))

-------------------------------------------------------------------------------
-- Exercise 6.6
-------------------------------------------------------------------------------

superimpose :: Picture -> Picture -> Picture
superimpose a b = [ superimposeLine a b | (a,b) <- zip a b  ]

superimposeTest =
    TestCase (assertEqual "" expected (superimpose a b) )
        where a =        [ ".##.",
                           ".##.",
                           ".##." ]
              b =        [ ".#.#",
                           ".#.#",
                           ".#.#" ]
              expected = [ ".###",
                           ".###",
                           ".###" ]

-------------------------------------------------------------------------------
-- Exercise 6.7
-------------------------------------------------------------------------------

printPicture :: Picture -> IO ()
printPicture picture = putStr (onSeparateLines picture)

-------------------------------------------------------------------------------
-- Exercise 6.8
-------------------------------------------------------------------------------

rotate90 :: Picture -> Picture
rotate90 p = [ line n p | n <- [0..((length p)-1)] ]
    where line n p = reverse [ l!!n | l <- p]

testRotate90 = TestCase (assertEqual "" expected (rotate90 input))
    where input    = [  ".##.",
                        ".#.#",
                        ".###",
                        "####" ]
          expected = [  "#...",
                        "####",
                        "##.#",
                        "###." ]
