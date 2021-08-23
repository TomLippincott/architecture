{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PostfixOperators #-}


module Architecture.Examples.Pergola
  (
    pergola
  , cam
  , light
  ) where

import Diagrams.Prelude hiding (scale, light)
import Diagrams.Backend.SVG.CmdLine
import Data.List.Split (splitOn)
import Debug.Trace (traceShowId)
import Diagrams.Backend.POVRay hiding (light)

foundationWidth = l 18 0 0
foundationDepth = l 26 0 0

viewerCenteringWidth = l 0 0 0
viewerCenteringHeight = l 5 9 0
viewerCenteringDepth = (foundationDepth / 2) + (l 40 0 0)

lightCenteringWidth = l 3 0 0
lightCenteringHeight = l 5 9 0
lightCenteringDepth = l (foundationDepth / 2 + 20) 0 0

pillarSize = l 0 4 0
pillarHeight = l 8 0 0
pillar = cube # scaleX pillarSize . scaleZ pillarSize . scaleY pillarHeight


(%) :: Int -> Int
(%) e = e

l :: (Num a) => a -> a -> a -> a
l feet inches fracInches = (feet * 12) + inches + fracInches

cam = mm50Camera # translate (r3 (viewerCenteringWidth, viewerCenteringHeight, viewerCenteringDepth)) -- (2,2,10))

color :: Colour Double -> Diagram POVRay -> Diagram POVRay
color c = diffuse 0.5 . ambient 0.1 . sc c

xy :: Direction V3 Double
--xy = direction . r3 $ (-1, 10, -0.5)
xy = direction . r3 $ (0, -200, 0)

--light = parallelLight xy (sRGB 1 1 1)
light = pointLight white

foundation = cube # scaleX foundationWidth . scaleY (l 0 4 0) . scaleZ foundationDepth
--foundation = cube # scaleX 100.0 . scaleY  . scaleZ 100.0

pergola :: QDiagram POVRay V3 Double Any
pergola = centerXYZ $ cat unitX [ cam
                                , skin foundation # color gray -- . translateX (foundationWidth / 2) . translateZ (foundationDepth / 2)
                              --, skin (pillar # translateX (l 19 0 0)) # color (sRGB 1.0 0.0 0.0)
                              --, skin (pillar) # color green
                              --, skin (pillar # translateX (l 19 0 0)) # color blue
                              , light # translateY 200
  ]
          --[skin sphere # color cyan, skin sphere # color green, skin sphere # color blue]

--tube length sideA sideB = cube # transform (scalingX sideA) . transform (scalingZ sideB) . transform (scalingY length)

-- base = True
-- current = False
-- proposed = True
-- full = True

-- backgroundColor = white
-- yardColor = white
-- brickColor = white
-- concreteColor = white
-- thickenedColor = white

-- houseDepth = l 3 0 0
-- leftNeighborWidth = l 3 0 0
-- rightNeighborWidth = l 12 0 0
-- sidewalkWidth = l 3 0 0
-- sidewalkDepth = l 11 0 0
-- concreteWidth = l 18 0 0
-- concreteDepth = l 26 0 0
-- concreteHeight = l 0 4 0
-- bevelSide = l 0 30 0
-- shallowBevelSide = bevelSide * 0.85

-- thickeningWidth = l 0 8 0
-- thickeningHeight = l 0 10 0
-- shallowWidth = concreteWidth - (2 * thickeningWidth)
-- shallowDepth = concreteDepth - thickeningWidth

-- porchToFence = l 0 19 0
-- porchWidth = l 10 0 0
-- porchDepth = l 6 0 0
-- stairWidth = l 0 34 0
-- stairDepth = l 3 0 0
-- stairOffset = l 0 9 0

-- oldFenceToAlley = l 5 0 0
-- garageSetback = l 1 0 0
-- garageDepth = concreteDepth

-- cellarWidth = l 0 34 0
-- cellarDepth = l 6 0 0

-- cellarToPorch = l 0 29 (1/2)

-- postsToAlley = l 1 0 0
-- postSize = l 0 3 (1/2)
-- postDistance = (concreteWidth - 3 * postSize) / 2

-- alleyDepth = l 3 0 0

-- yardWidth = concreteWidth + sidewalkWidth
-- yardDepth = concreteDepth + sidewalkDepth + houseDepth
-- planWidth = yardWidth + leftNeighborWidth + rightNeighborWidth
-- planDepth = yardDepth + houseDepth + alleyDepth

-- channelMarginHeight = l 0 1 0
-- channelMarginWidth = l 0 2 0

-- channelHeightA = l 0 1 (5/8)
-- channelWidthA = l 0 7 (5/8)
-- channelHeightB = l 0 1 (5/8)
-- channelWidthB = l 0 5 (5/8)
-- channelHeightC = l 0 1 (5/8)
-- channelWidthC = l 0 3 (5/8)
-- channelWidth = l 0 12 0 -- channelWidthA + 2 * channelMarginWidth
-- channelHeight = l 0 8 0
-- channelToHouse = l 1 0 0

-- minSlabSlope = 2
-- maxSlabSlope = 4

-- minChannelSlope = 2
-- maxChannelSlope = 4

-- sideNudge = l 0 0 (1/2)
-- centerNudge = l 0 3 (1/2)
-- channelNudge = l 0 4 0

-- data Dir = V | H deriving (Eq, Show)

-- measurement fromX fromY toX toY hProp lProp = mconcat [ lower # translateX x # translateY y
--                                                       , higher # translateX x # translateY y
--                                                       , text str # rotate (textAngle @@ deg) # translateX (x + xnudge) # translateY (y + ynudge) # fontSize 18
--                                                       ]
--   where
    
--     dir = if fromX == toX && fromY /= toY then V else H
--     edgeX = planWidth / 2
--     edgeY = planDepth / 2
--     xoffset = if dir == V then fromX else fromX + (toX - fromX) * lProp
--     yoffset = if dir == H then fromY else fromY + (toY - fromY) * lProp
--     textAngle = if dir == V then 270 else 0
--     x = xoffset - edgeX
--     y = yoffset - edgeY
--     totalLen = if dir == V then toY - fromY else toX - fromX
--     higherLen = totalLen * hProp --if dir == V then toY - fromY else toY
--     lowerLen = totalLen * lProp
--     arrowAngle = if dir == V then 90 else 0
--     ynudge = if dir == V then 0 else 2
--     xnudge = if dir == H then 0 else 2
--     higher = arrow' mstyle higherLen # rotate (arrowAngle @@ deg) # lw 1
--     lower = arrow' mstyle (-lowerLen) # rotate (arrowAngle @@ deg) # lw 1
    
--     dist = if dir == V then toY - fromY else toX - fromX
--     feet = round $ dist / 12
--     str = show feet ++ " feet"

-- measurement' fromX fromY toX toY hProp lProp str = mconcat [ lower # translateX x # translateY y
--                                                            , higher # translateX x # translateY y
--                                                            , text str # rotate (textAngle @@ deg) # translateX (x + xnudge) # translateY (y + ynudge) # fontSize 18
--                                                            ]
--   where
    
--     dir = if fromX == toX && fromY /= toY then V else H
--     edgeX = planWidth / 2
--     edgeY = planDepth / 2
--     xoffset = if dir == V then fromX else fromX + (toX - fromX) * lProp
--     yoffset = if dir == H then fromY else fromY + (toY - fromY) * lProp
--     textAngle = if dir == V then 270 else 0
--     x = xoffset - edgeX
--     y = yoffset - edgeY
--     totalLen = if dir == V then toY - fromY else toX - fromX
--     higherLen = totalLen * hProp --if dir == V then toY - fromY else toY
--     lowerLen = totalLen * lProp
--     arrowAngle = if dir == V then 90 else 0
--     ynudge = if dir == V then 0 else 2
--     xnudge = if dir == H then 0 else 2
--     higher = arrow' mstyle higherLen # rotate (arrowAngle @@ deg) # lw 1
--     lower = arrow' mstyle (-lowerLen) # rotate (arrowAngle @@ deg) # lw 1
    
--     dist = if dir == V then toY - fromY else toX - fromX
--     feet = round $ dist / 12



-- scale = 5

-- pergola :: Diagram B
-- pergola = mconcat [brick, perim # translateX 20, hgt, tierA, tierB, tierC, width, height, centerXY plan] # connectPerim' astyle "channel inset" "channel" (270 @@ deg) (90 @@ deg) # connect' astyle "thickening label" "channel"
--   where
--     --thickLabel = text "10\" deep" # fontSize 24 # named "thick label"
--     --thinLabel = text "4\" deep" # fontSize 24 # named "thin label"
--     hx = (leftNeighborWidth + concreteWidth + 0.5 * sidewalkWidth)
--     height = measurement hx alleyDepth hx (alleyDepth + concreteDepth) 0.5 0.5
--     wy = alleyDepth + concreteDepth + (l 1 6 0)
--     width = measurement leftNeighborWidth wy (leftNeighborWidth + concreteWidth) wy 0.7 0.3
--     ctr = leftNeighborWidth + concreteWidth + sidewalkWidth + 0.5 * rightNeighborWidth
--     tierA = if full then measurement' (ctr - scale * channelWidthA / 2) 400 (ctr + scale * channelWidthA / 2) 100 0.5 0.5 "7 5/8\"" else mempty
--     tierB = if full then measurement' (ctr - scale * channelWidthB / 2) 390 (ctr + scale * channelWidthB / 2) 100 0.5 0.5 "5 5/8\"" else mempty
--     tierC = if full then measurement' (ctr - scale * channelWidthC / 2 - scale) 380 (ctr + scale * channelWidthC / 2 - scale) 100 0.4 0.6 "3 5/8\"" else mempty
--     yd = (planDepth - (houseDepth + alleyDepth + garageDepth + garageSetback))
--     ctr' = alleyDepth + garageSetback + garageDepth + 0.5 * yd
--     h = scale * (channelHeightA + channelHeightB + channelHeightC) * 0.5
--     hgt = if full then measurement' (ctr + 40) (ctr' - h) (ctr + 40) (ctr' + h) 0.5 0.5 "4 7/8\"" else mempty
--     perim = if proposed then textBox "dlkjsadsajkdConcrete\n8\" perim. on 3 sides is 10\" deep\nMain pour is 4\" deep" 10 10 # translateX (-40) # translateY 10 # fc black # fontSize 18 else mempty
--     brick = if current then textBox "Current parking-pad\npaved with brick" 10 10 # fc black # fontSize 24 # translateX (-25) else mempty
      
-- plan = (centerXY $ (centerXY $ leftNeighbor ||| (centerXY $ house === yard) ||| rightNeighbor) === alley) # lw 1.0 # named "plan"

-- alley = centerXY $ text "Alley" # fontSize 48 # fc black <> rect (leftNeighborWidth + yardWidth + rightNeighborWidth) alleyDepth # fc white # named "alley"

-- channel = if full then centerXY $ top === bottom # named "channel" # extrudeRight 7 else mempty
--   where
--     top = mconcat [ rect channelWidthA (yardDepth - concreteDepth - channelToHouse)
--                   , rect channelWidthB (yardDepth - concreteDepth - channelToHouse)
--                   , rect channelWidthC (yardDepth - concreteDepth - channelToHouse)
--                   ]
--     bottom = mconcat [ rect channelWidth (concreteDepth - thickeningWidth) # dashing [10, 10] 0 # translateY (-0.5 * thickeningWidth)
--                      , rect channelWidthA concreteDepth
--                      , rect channelWidthB concreteDepth
--                      , rect channelWidthC concreteDepth
--                      ]

-- astyle = with & shaftStyle %~ lw 5 & arrowHead .~ spike & headLength .~ 20

-- mstyle = with & shaftStyle %~ lw 1 & arrowHead .~ spike & headLength .~ 8



-- padAmount = 12
-- box innards w h =
--   let padded =                  strutY (h/2)
--                                 ===
--                                 (strutX (w/2) ||| centerXY innards ||| strutX (w/2))
--                                 ===
--                                 strutY (h/2)
--       height = diameter (r2 (0,1)) padded
--       width  = diameter (r2 (1,0)) padded
--   in centerXY $ innards <> roundedRect width height 0.0 # fc white # lc white

-- textBox s w h = (box (centeredText' s) w h)

-- yard = centerXY $ withText # named "yard" # connectOutside' astyle "label" "p1" # connectOutside' astyle "label" "p2" # connectOutside' astyle "label" "p3"
--   where
--     base = centerXY $ rect yardWidth yardDepth # fc yardColor
--     poured = centerXY $ (alignBL concrete) `atop` (alignBL base)
--     withPorch = centerXY $ (alignTR $ porch # extrudeRight porchToFence) `atop` (alignTR poured)
--     withCellar = centerXY $ (alignTL cellar) `atop` (alignTL withPorch)
--     withPosts = if full then centerXY $ (alignBL posts) `atop` (alignBL withCellar) else withCellar
--     withText = if full then centerXY $ textBox "3 1/2\" steel posts\n3' below\n6' above" 50 30 # named "label" # fontSize 24 # translateX (-70) # translateY (-150) `atop` withPosts else withPosts
-- --    grade = 

-- porch = centerXY $ (alignTR stairs) `atop` (alignBR pantry) # named "porch"
--   where
--     pantry = centerXY $ text "Porch" # fontSize 24 <> rect porchWidth porchDepth # named "pantry"
--     stairs = centerXY $ text "Stairs" # fontSize 24 <> rect stairWidth stairDepth # named "stairs" # extrudeRight stairOffset
  
-- cellar = centerXY $ textBox "Cellar\ndoor" 10 10 # fontSize 24 <> rect cellarWidth cellarDepth # named "cellar" # extrudeLeft (l 3 0 0)

-- posts = if full then centerXY $ hcat' (with & sep .~ postDistance) [post # named "p1", post # named "p2", post # named "p3"] # fc black # extrudeBottom garageSetback else mempty
--   where
--     post = square postSize

-- house = centerXY $ text "House" # fontSize 48 # fc black <> rect yardWidth houseDepth # fc white # named "house"

-- leftNeighbor = centerXY $ (h === w) # fc yardColor # named "left neighbor"
--   where
--     w = text "Neighbor's Walkway" # fontSize 48 # rotate (90 @@ deg) # fc black <> rect leftNeighborWidth yardDepth
--     h = rect leftNeighborWidth houseDepth # fc yardColor
    
-- rightNeighbor = centerXY $ (h === y === g === s) # fc white # named "right neighbor"
--   where
--     h = rect rightNeighborWidth houseDepth
--     y = (alignT channelInset) `atop` (alignT $ rect rightNeighborWidth (planDepth - (houseDepth + alleyDepth + garageDepth + garageSetback)))
--     g = text "Neighbor's Garage" # fontSize 48 # rotate (270 @@ deg) # fc black <> rect rightNeighborWidth garageDepth
--     s = rect rightNeighborWidth garageSetback


-- centeredText ls n = vcat' (with & catMethod .~ Distrib & sep .~ n)
--                      (map (\l -> centerX (text l)) ls)
-- centeredText' s = centeredText (splitOn "\n" s) (12)

-- channelInset = if full then centerXY $ expl === ((alignT $ first === (centerX $ second === third)) # fc black `atop` total) else mempty
--   where
--     first = rect (scale * channelWidthA) (scale * channelHeightA)
--     second = alignL $ rect (scale * channelWidthB) (scale * channelHeightB)
--     third = alignL $ rect (scale * channelWidthC) (scale * channelHeightC) # named "channel inset"
--     total = alignT $ rect (scale * channelWidth) (scale * channelHeight)
--     --label = textBox "1/4\"/ft grade to alley\nSaw-cut in existing concrete" 100 20 # named "channel inset" # fontSize 24 # fc black
--     expl = textBox "Channel set in 8\" deep, 12\" wide center\n1/4\" grade to alley\nSaw-cut extended into existing concrete" 100 40 # fontSize 18 # fc black
    
-- concrete = centerXY $ mconcat [thickLabel # translateX 50 # translateY (200), gradeLabel # rotate (304 @@ deg) # translateX (-50) # translateY (240), arr, shallow # dashing [10, 10] 0, thick # named "thick", alignB channel] # named "concrete"
--   where
--     thick = centerX $ strokeLoop $ fromOffsets (map r2 [ (0, 0)
--                                                        , (concreteWidth, 0)
--                                                        , (0, concreteDepth - bevelSide)
--                                                        , (-bevelSide, bevelSide)
--                                                        , (-(concreteWidth - bevelSide), 0)
--                                                        , (0, -concreteDepth)
--                                                        ]
--                                                )
--     shallow = if proposed then centerX $ strokeLoop $ fromOffsets (map r2 [ (0, 0)
--                                                                           , (shallowWidth, 0)
--                                                                           , (0, shallowDepth - shallowBevelSide)
--                                                                           , (-shallowBevelSide, shallowBevelSide)
--                                                                           , (-(shallowWidth - shallowBevelSide), 0)
--                                                                           , (0, -shallowDepth)
--                                                                           ]
--                                                                   ) else mempty

--     arr = arrowBetween' (with & shaftStyle %~ lw 5 & arrowHead .~ spike & headLength .~ 20) (P $ r2 (-0.5 * concreteWidth, concreteDepth)) (P $ r2 (0.5 * concreteWidth,0)) -- # dashing [10,10] 0
--     gradeLabel = text "Existing grade (~1/4\"/ft)" # fontSize 24
--     thickLabel = mempty -- textBox "Thickened\n12\"wx10\"d" # named "thickening label" # fontSize 24
-- --channelInset = inset
-- --  where
-- --    inset = 
