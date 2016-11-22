{-# LANGUAGE OverloadedStrings #-}
module Shapes(
  Shape, Point, Vector, Transform, Drawing,
  point, getX, getY, convert,
  empty, circle, square,
  identity, translate, rotate, scale, (<+>))  where

import Text.Blaze.Svg11 ((!), mkPath, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

-- I created an image datatype, a stlye typeclass and modified the drawing definition to include a list of styles
data Img = Img {  shape :: Shape
                , position :: Vector
                , size :: Double
                , rotation :: Transform
                , strokeColor :: String
                , strokeWidth :: Integer
                , fillColor :: String

                } deriving (Show)


modifyShape :: Shape -> Img -> Img
modifyShape s (Img _ a b c e f g) = Img s a b c e f g

modifyTrans :: Transform -> Img -> Img
modifyTrans (Translate (Vector x y)) (Img s (Vector a b) n m e f g) =
  Img s (Vector (a + x) (b + y)) n m e f g


convertScale :: Transform -> Double
convertScale (Scale (Vector x y)) = x + y

modifyScale :: Double -> Img -> Img
modifyScale scale (Img s p size r e f g) = Img s p (size*scale) r e f g

modifyRotation :: Transform -> Img -> Img
modifyRotation (Rotate r) x = x
modifyRotation _ x = x

applyStyle :: [Style] -> Img -> Img
applyStyle (StrokeColour x : xs) (Img a b c d e f g) = applyStyle xs (Img a b c d x f g)
applyStyle (StrokeWidth x:xs) (Img a b c d e f g) = applyStyle xs (Img a b c d e x g)
applyStyle (FillColour x : xs) (Img a b c d e f g) =  applyStyle xs (Img a b c d e f x)
applyStyle (None : xs) image = applyStyle xs image
-- applyStyle [(StrokeColour x)] (Img a b c d e f g) = Img a b c d x f g
-- applyStyle [(StrokeWidth x)] (Img a b c d e f g) = Img a b c d e x g
-- applyStyle [(FillColour x)] (Img a b c d e f g) =  Img a b c d e f x
-- applyStyle [(None)] image = image
applyStyle _ image = image

pickTransform :: Transform -> Img -> Img
pickTransform (Translate v1) image = modifyTrans (Translate v1) image
pickTransform (Scale v1) image = modifyScale (convertScale (Scale v1)) image
pickTransform (Rotate m) image = modifyRotation (Rotate m) image

applyTransforms :: Transform -> Img -> Img
applyTransforms (Compose t0 t1) image =  applyTransforms t0 (pickTransform t1 image)
applyTransforms t image = pickTransform t image


-- This takes a tuple froma  drawing and converts it to an image
conv :: (Transform, Shape, [Style]) -> Img
conv (t, shape, styles) =  applyStyle styles $ modifyShape shape $ applyTransforms t blank
  where blank = Img{shape = circle, position = vector 1 1, size = 10,
  rotation = Translate (Vector 0 0), strokeColor = "black", strokeWidth = 0, fillColor = "black"}

convertDrawings :: Drawing -> [Img]
convertDrawings d = map conv d

-- This takes an image and converts it to an svg
-- long and unwieldly sticking together of svg attributes easy to extend though
imageToBlaze :: Img -> S.Svg
imageToBlaze (Img Circle (Vector px py) si r e f g) = S.circle ! A.cx  (S.stringValue (show px)) ! A.cy (S.stringValue (show py)) ! A.r (S.stringValue (show ( si/2))) ! A.strokeWidth (S.stringValue (show f)) ! A.stroke (S.stringValue e) ! A.fill (S.stringValue g)
imageToBlaze (Img Square (Vector px py) si r e f g) = S.rect ! A.x (S.stringValue (show px)) ! A.y (S.stringValue (show py)) ! A.width (S.stringValue (show si)) ! A.height (S.stringValue (show si)) ! A.strokeWidth (S.stringValue (show f)) ! A.stroke (S.stringValue e) ! A.fill (S.stringValue g)

drawingToSvgArray :: Drawing -> [S.Svg]
drawingToSvgArray ds = map imageToBlaze $ convertDrawings ds

-- this converts a drawing to a string representation of a blaze svg,
-- ready to be used to the server with scotty and blaze
convert :: Drawing -> String
convert ds = renderSvg $ S.docTypeSvg ! A.viewbox "0 0 400 400" $ foldr1 (>>) (drawingToSvgArray ds)
--
data Vector = Vector Double Double
              deriving (Show, Read)
vector = Vector

cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

mult :: Matrix -> Vector -> Vector
mult (Matrix r0 r1) v = Vector (cross r0 v) (cross r1 v)

invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c

-- 2x2 square matrices are all we need.
data Matrix = Matrix Vector Vector
              deriving Show

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

getX (Vector x y) = x
getY (Vector x y) = y

-- Shapes

type Point  = Vector

point :: Double -> Double -> Point
point = vector

data Style = None
  | StrokeWidth Integer
  | StrokeColour String
  | FillColour String
  deriving (Show, Read)

strokewidth = StrokeWidth
strokecolour = StrokeColour
fillcolor = FillColour


data Shape = Empty
           | Circle
           | Square
             deriving (Show, Read)

empty, circle, square :: Shape

empty = Empty
circle = Circle
square = Square

-- Transformations

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Compose Transform Transform
           | Rotate Double
             deriving (Show, Read)

identity = Identity
translate = Translate
scale = Scale
rotate = Rotate
t0 <+> t1 = Compose t0 t1

transform :: Transform -> Point -> Point
transform Identity                   x = id x
transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty))     (Vector px py)  = Vector (px / tx)  (py / ty)
transform (Rotate n) p = p
type Drawing = [(Transform,Shape, [Style])]

-- interpretation function for drawings

-- inside :: Point -> Drawing -> Bool
-- inside p d = or $ map (inside1 p) d

-- inside1 :: Point -> (Transform, Shape) -> Bool
-- inside1 p (t,s) = insides (transform t p) s

-- insides :: Point -> Shape -> Bool
-- p `insides` Empty = False
-- p `insides` Circle = distance p <= 1
-- p `insides` Square = maxnorm  p <= 1


-- distance :: Point -> Double
-- distance (Vector x y ) = sqrt ( x**2 + y**2 )

-- maxnorm :: Point -> Double
-- maxnorm (Vector x y ) = max (abs x) (abs y)

s1 = (translate (vector 20 20) <+> scale (point 2 2) <+> (rotate 20), square, [StrokeWidth 2, StrokeColour "blue", FillColour "red"])
s2 = (translate (point 10 10) <+> translate (vector 20 20) <+> scale (vector 6 2), square, [StrokeWidth 3, FillColour "green"])
s3 = (translate (vector 77 77), circle, [StrokeWidth 0, StrokeColour "yellow", FillColour "purple"])
s4 = (translate (vector 30 30) <+> scale (point 10 10), square, [FillColour "orange"])
s5 = (scale (point 10 10), circle, [StrokeWidth 6, FillColour "red"])
s6 = (scale (point 3 3) <+> translate (vector 40 40), square, [StrokeWidth 6, FillColour "red"])

d1 = [s1, s2, s3, s4, s5]
d2 = [s2, s3]
d3 = [s1]
d4 = [s6]
