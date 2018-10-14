module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Bitmap
import Codec.Picture
import Graphics.Gloss.Juicy
import Data.Maybe

-- | Data describing the state of the Burger game.
data BurgerStacker = Game
    { burgerLoc :: (Float, Float) -- ^ Burger (x,y) location
    , burgerVel :: (Float, Float) -- ^ Burger (x,y) velocity
    } deriving Show

data BurgerSprites = Sprites {
  bunSprite :: Picture
}

-- | The starting state for the game of BurgerStacker.
initialState :: BurgerStacker
initialState = Game
  { burgerLoc = (0, -100)
  , burgerVel = (1, -3)
  }

loadPicture :: IO BurgerSprites
loadPicture = do
  picture <- loadJuicyPNG "/Users/rachelsunderland/Desktop/burger_sandwich_PNG4160.png"
  return Sprites {
    bunSprite = fromJust picture
  }

renderBun :: BurgerSprites -> Picture
renderBun = do
  bunSprite

-- | Convert a game state into a picture
render :: BurgerStacker -> Picture 
render game = renderBun
  
-- | Update the game by moving the ball.
-- Ignore the ViewPort argument.
update :: ViewPort -> Float -> BurgerStacker -> BurgerStacker 
update _ = moveBurger 

moveBurger :: Float -> BurgerStacker -> BurgerStacker
moveBurger seconds game = game 
  where
    -- Old locations and velocities.
    (x, y) = burgerLoc game
    (vx, vy) = burgerVel game

    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds

-- | Number of frames to show per second
fps :: Int
fps = 60

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

getPicture :: IO (Maybe Picture)
getPicture = do
  loadJuicyPNG "/Users/rachelsunderland/Desktop/burger_sandwich_PNG4160.png"

main :: IO ()
main = do
  simulate window background fps initialState render update

   
{- 
pictureIO :: Picture
pictureIO = do 
  picture <- loadJuicyPNG "/Users/rachelsunderland/Desktop/burger_sandwich_PNG4160.png"
  case picture of 
    Nothing -> default_img
    (Just i) -> i 

main :: IO ()
main = do
    picture <- loadJuicyPNG "/Users/rachelsunderland/Desktop/burger_sandwich_PNG4160.png"
    case picture of 
        Nothing -> display window background default_img
        (Just i) -> display window background (translate (0) (-100) $ color (dark red) $ i)

main :: IO ()
main = animate window background frame 
  where
    frame :: Float -> Picture
    frame seconds = render $ moveBurger seconds initialState -}






