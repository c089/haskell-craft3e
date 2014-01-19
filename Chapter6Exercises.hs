import Test.HUnit
import Test.QuickCheck

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
