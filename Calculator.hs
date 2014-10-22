import Control.Monad (when)

import Data.Maybe
import Expr
import Pages

import Haste hiding (eval)
import Haste.Graphics.Canvas

-- gets the value of the input, converts it to a shape and then into a pic
-- in order to draw it on the canvas. Does nothing if expression can't be
-- read.
readAndDraw :: Elem -> Canvas -> IO ()
readAndDraw elem can = do
                           s <- getProp elem "value"
                           if (readExpr s) /= Nothing
                               then do
                                        let shp = path (points (fromJust (readExpr s)) 0.04 (canWidth, canHeight))
                                        let pic = stroke shp
                                        render can pic
                               else return ()

canWidth  = 300
canHeight = 300

main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight   -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    input   <- mkInput 20 "x"                -- The formula input
    draw    <- mkButton "Draw graph"         -- The draw button
      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    row formula [fx,input]
    column documentBody [canvas,formula,draw]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input

    -- Interaction
    Just can <- getCanvas canvas
    onEvent draw  OnClick $ \_ _  -> readAndDraw input can
    onEvent input OnKeyUp $ \code -> when (code==13) $ readAndDraw input can
      -- "Enter" key has code 13


-- returns a list of all the points that should be drawn on the canvas
points :: Expr -> Double -> (Int,Int) -> [Point]
points exp scale (width, height) = points' a exp scale
	where
		a = -((fromIntegral width) / 2) :: Double

points' :: Double -> Expr -> Double -> [Point]
points' 151 exp scale = []
points' x0  exp scale = 
    (x0 + 150, y + 150) : points' (x0+1) exp scale
    where
        x = scale * x0
        y = -(eval exp x) / scale




