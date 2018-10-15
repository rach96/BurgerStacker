module Main (main) where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Bitmap
import Codec.Picture
import Graphics.Gloss.Juicy
import Data.Maybe
--import System.Random

width, height, offset :: Int
width = 500
height = 500
offset = 100

-- | Data containing burger sprites
data BurgerSprites = Sprites {
  bunSprite :: Picture,
  pattySprite :: Picture,
  onionSprite :: Picture,
  lettuceSprite :: Picture,
  topBunSprite :: Picture
} deriving Show

-- | Data describing the state of the Burger game.
data BurgerStacker = Game
    { btm_bun_Loc :: (Float, Float) -- ^ Burger (x,y) location
    , pattyLoc :: (Float, Float)
    , onionLoc :: (Float, Float)
    , lettuceLoc :: (Float, Float)
    , topBunLoc :: (Float, Float)
    , burgerVel :: (Float, Float) -- ^ Burger (x,y) velocity
    , sprites :: BurgerSprites
    } deriving Show

-- | The starting state for the game of BurgerStacker.
initialState :: BurgerSprites -> BurgerStacker
initialState s = Game
  { btm_bun_Loc = (0, -220)
  , pattyLoc = (0, 200)
  , onionLoc = (0,500)
  , lettuceLoc = (0,700)
  , topBunLoc = (0, 900)
  , burgerVel = (0, -100)
  , sprites = s
  }

loadPictures :: IO BurgerSprites
loadPictures = do
  bunPicture <- loadJuicyPNG "/Users/rachelsunderland/Desktop/bottom_bun.png"
  pattyPicture <- loadJuicyPNG "/Users/rachelsunderland/Desktop/patty.png"
  onionPicture <- loadJuicyPNG "/Users/rachelsunderland/Desktop/onion.png"
  lettucePicture <- loadJuicyPNG "/Users/rachelsunderland/Desktop/lettuce.png"
  topBunPicture <- loadJuicyPNG "/Users/rachelsunderland/Desktop/top_bun.png"
  return Sprites {
    bunSprite = fromJust bunPicture,
    pattySprite = fromJust pattyPicture,
    onionSprite = fromJust onionPicture,
    lettuceSprite = fromJust lettucePicture,
    topBunSprite = fromJust topBunPicture
  }

renderBun :: BurgerSprites -> Picture
renderBun = bunSprite

renderPatty :: BurgerSprites -> Picture
renderPatty = pattySprite

renderOnion :: BurgerSprites -> Picture
renderOnion = onionSprite

renderLettuce :: BurgerSprites -> Picture
renderLettuce = lettuceSprite

renderTopBun :: BurgerSprites -> Picture
renderTopBun = topBunSprite

type BurgerItems = [BurgerSprites -> Picture]
burgerItemList :: BurgerItems
burgerItemList = [renderBun, renderPatty, renderOnion, renderLettuce, renderTopBun]

-- | Convert a game state into a picture
render :: BurgerStacker -> Picture 
render game = pictures [btm_bun, brg_patty]
  where
    btm_bun = uncurry translate (btm_bun_Loc game) $ scale 0.10 0.15 (renderBun (sprites game))
    brg_patty = uncurry translate (pattyLoc game) $ scale 0.25 0.30 (renderPatty (sprites game))
    --onion = uncurry translate (pattyLoc game) $ scale 0.25 0.30 (renderOnion (sprites game))
    --lettuce = uncurry translate (pattyLoc game) $ scale 0.25 0.30 (renderLettuce (sprites game)) 
    --top_bun = uncurry translate (pattyLoc game) $ scale 0.25 0.30 (renderTopBun (sprites game)) 
  
-- | Update the game by moving the ball.
-- | Add new ingredients if the previous position is on half of the page
-- Ignore the ViewPort argument.
update :: Float -> BurgerStacker -> BurgerStacker 
update seconds = itemVelocity . moveBurger seconds

moveBurger :: Float -> BurgerStacker -> BurgerStacker
moveBurger seconds game = game {pattyLoc = (x',y')}
  where
    -- Old locations and velocities.
    (x, y) = pattyLoc game
    (vx, vy) = burgerVel game

    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds

type Radius = Float
type Position = (Float, Float)

-- | Given position of the falling item, 
-- return whether a collision with the bottom bun occurred
bunCollision :: Position -> Position -> Bool 
bunCollision (x',y') (x, y) = bottomCollision && bunCollision
  where
    bottomCollision    = y - 10 <= -fromIntegral width / 2 + 50
    bunCollision = (x >= x' - 110) && (x <= x' + 110)

-- | Detect a collision of the burger item with the bottom bun. 
-- Upon collisions, stop the burger item from falling.
itemVelocity :: BurgerStacker -> BurgerStacker
itemVelocity game = game { burgerVel = (vx, vy')}
  where
    -- The old velocities.
    (vx, vy) = burgerVel game

    vy' = if bunCollision (btm_bun_Loc game) (pattyLoc game)
          then
             -- Update the velocity.
             0
           else
            -- Do nothing. Return the old velocity.
            vy

-- | Respond to key events.
handleKeys :: Event -> BurgerStacker -> BurgerStacker

-- If the left key is pressed, move ONLY the bottom bun if the patty is still falling.
-- Move the patty if it is on top of the bottom bun
handleKeys (EventKey (SpecialKey KeyLeft) _ _ _) game = game { btm_bun_Loc = (x-5, y) , pattyLoc = (a, b)}
  where
    (x, y) = btm_bun_Loc game
    (v, w) = pattyLoc game
    (a, b) = if bunCollision (btm_bun_Loc game) (pattyLoc game)
      then
        (v - 5, w)
      else 
        (v,w)

-- If the right key is pressed, move ONLY the bottom bun if the patty is still falling.
-- Move the patty if it is on top of the bottom bun
handleKeys (EventKey (SpecialKey KeyRight) _ _ _) game = game { btm_bun_Loc = (x+5, y), pattyLoc = (a, b) }
  where
    (x, y) = btm_bun_Loc game
    (v, w) = pattyLoc game
    (a, b) = if bunCollision (btm_bun_Loc game) (pattyLoc game)
      then
        (v + 5, w)
      else 
        (v,w)

-- Do nothing for all other events.
handleKeys _ game = game

window :: Display
window = InWindow "Burger Stacker" (width, height) (offset, offset)

background :: Color
background = white

-- | Number of frames to show per second
fps :: Int
fps = 100

main :: IO ()
main = do
  ioSprites <- loadPictures
  play window background fps (initialState ioSprites) render handleKeys update

{- 
addAdditionalItem :: [Picture] -> Picture -> [Picture]
addAdditionalItem gamePictures newPicture = gamePictures ++ newPicture

generateAdditionalItem :: [BurgerSprites -> Picture] -> Int -> (BurgerSprites -> Picture)
generateAdditionalItem itemList index = itemList !! index -}
  