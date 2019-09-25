-- | Types and functions for shapes. The list of all tetris pieces.
module Shapes where
import Data.List(transpose)
import Data.Maybe(isNothing)
import Test.QuickCheck

-- * Shapes

type Square = Maybe Colour

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
              deriving (Eq,Bounded,Enum,Show)

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.

data Shape = S [Row] deriving (Eq)
type Row = [Square]

rows :: Shape -> [Row]
rows (S rs) = rs


-- * Showing shapes

showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
  where
    showRow :: Row -> String
    showRow r = [showSquare s | s <- r]
    
    showSquare Nothing = '.'
    showSquare (Just Black) = '#' -- can change to '█' on linux/mac
    showSquare (Just Grey)  = 'g' -- can change to '▓'
    showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss)++r


-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of connected 4 blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [S (makeSquares s) | s <- shapes] 
   where
      makeSquares = map (map colour)
      colour c    = lookup c [('I',Red),('J',Grey),('T',Blue),('O',Yellow),
                              ('Z',Cyan),('L',Green),('S',Purple)]
      shapes = 
              [["I",
               "I",
               "I",
               "I"],
              [" J",
               " J",
               "JJ"],
              [" T",
               "TT",
               " T"],
              ["OO",
               "OO"],
              [" Z",
               "ZZ",
               "Z "],
              ["LL",
               " L",
               " L"],
              ["S ",
               "SS",
               " S"]]

-- * Some simple functions

-- ** A01
emptyShape :: (Int,Int) -> Shape
emptyShape (n,m) = S (replicate m emptyRow') 
  where emptyRow' = emptyRow n (Nothing)

emptyRow :: Int -> Square -> Row
emptyRow n square = replicate n square

-- ** A02

-- | The size (width and height) of a shape
shapeSize :: Shape -> (Int,Int)
shapeSize x = (shapeCol x, shapeRow x) 

shapeRow :: Shape -> Int
shapeRow a = length(rows a) 

shapeCol :: Shape -> Int
shapeCol a = length $ head (rows a)
-- ** A03

-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount a = length $ filter (/= Nothing) (concat (rows a))

{-OBS!!!!!!!!!! EXPLAIN!!!!!!!!!!
blockCount :: Shape -> Int
blockCount a = area b - (counterRow (rows a))
   where b = shapeSize a

counterRow :: [Row] -> Int
counterRow [] = 0
counterRow (x:xs) = counterCol x + counterRow xs

counterCol :: Row -> Int
counterCol (x:xs) | xs == [] = 0
                  | x == Nothing = 1 + counterCol xs
                  | x /= Nothing = 0 + counterCol xs 

area :: (Int,Int) -> Int
area (x,y) = x*y
-}

-- * The Shape invariant

-- ** A04
-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)
prop_Shape :: Shape -> Bool
prop_Shape a = (shapeRow a >= 1) && (shapeCol a >= 1) && shapeCheck a 

shapeCheck :: Shape -> Bool
shapeCheck a = (shapeCol a) * shapeRow a == length (concat (rows a)) 

-- * Test data generators

-- ** A05
-- | A random generator for colours
rColour :: Gen Colour
rColour = elements [Black, Red, Green, Yellow, Blue, Purple, Cyan, Grey]

instance Arbitrary Colour where
  arbitrary = rColour

-- ** A06
-- | A random generator for shapes
rShape :: Gen Shape
rShape = elements allShapes


instance Arbitrary Shape where
  arbitrary = rShape

-- * Transforming shapes

-- ** A07
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape a = S (transpose(rows a))

rotateShape' :: Shape -> Shape
rotateShape' a = S (transpose(reverse(rows a)))
-- ** A08
-- | shiftShape adds empty squares above and to the left of the shape
shiftShape :: (Int,Int) -> Shape -> Shape
shiftShape (x,y) shape = addCol x (addRow y shape)

addCol :: Int -> Shape -> Shape 
addCol col shape = rotateShape (addRow col (rotateShape shape))

addRow :: Int -> Shape -> Shape
addRow row shape = S $ rows(empty) ++ rows shape
      where empty = emptyShape (shapeCol shape,row)
{-
shiftShape :: (Int,Int) -> Shape -> Shape
shiftShape (x,y) a = S (addRow y (addCol x (rows a)))

-- Ta addRow först då addCol lägger till i varje lista 
-- och den nya tillagda listan kommer sakna den

addCol :: Int -> [Row] -> [Row]
addCol x [] = undefined
addCol x (y:ys)= replicate x Nothing ++ y : addCol x ys

addRow :: Int -> [Row] -> [Row]
addRow x (y:ys) = replicate (addCol (S y)) [Nothing] ++ y ++ addRow ys
-}
-- ** A09
-- | padShape adds empty sqaure below and to the right of the shape

padShape :: (Int,Int) -> Shape -> Shape
padShape (col,row) shape | col <0 || row <0 = error "Error in padShape, negative values"
                         | otherwise = addRight col (addBottom row shape)

addRight :: Int -> Shape -> Shape 
addRight col shape = rotateShape (addBottom col (rotateShape shape))

addBottom :: Int -> Shape -> Shape
addBottom row shape = S (rows shape ++ rows(emptyShape (shapeCol(shape),row)))



-- ** A10
-- | pad a shape to a given size
padShapeTo :: (Int,Int) -> Shape -> Shape
padShapeTo (x,y) shape = padShape ((x-a),(y-b)) shape
     where (a,b) = shapeSize shape

{-
-- * Comparing and combining shapes

-- ** B01

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
s1 `overlaps` s2 = error "A11 overlaps undefined"

-- ** B02
-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square->Square->Square) -> Shape -> Shape -> Shape
zipShapeWith = error "A12 zipShapeWith undefined"

-- ** B03
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
s1 `combine` s2 = error "A13 zipShapeWith undefined"
-}
