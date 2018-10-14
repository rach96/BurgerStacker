module Main (main) where

import Graphics.Gloss
import Codec.Picture
import Graphics.Gloss.Juicy

-- | Data describing the state of the Burger game.
data BurgerStacker = Game
    { burgerLoc :: (Float, Float) -- ^ Burger (x,y) location
    , burgerVel :: (Float, Float) -- ^ Burger (x,y) velocity
    } deriving Show

-- | The starting state for the game of BurgerStacker.
initialState :: BurgerStacker
initialState = Game
  { burgerLoc = (0, -100)
  , burgerVel = (1, -3)
  }

width, height, offset :: Int
width = 500
height = 500
offset = 100

window :: Display
window = InWindow "Burger Stacker" (width, height) (offset, offset)

background :: Color
background = white

default_img :: Picture
default_img = Circle 80

main :: IO ()
main = do
    picture <- loadJuicyPNG "/Users/rachelsunderland/Desktop/burger_sandwich_PNG4160.png"
    case picture of 
        Nothing -> display window background default_img
        (Just i) -> display window background (translate (0) (-100) $ color (dark red) $ i)


