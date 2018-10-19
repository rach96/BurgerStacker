module Main (main) where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Bitmap
import Codec.Picture
import Graphics.Gloss.Juicy
import Data.Maybe
import System.Random
import Data.Time.Clock

-- window dimensions
width, height, offset :: Int
width = 500
height = 500
offset = 100

window :: Display
window = InWindow "Burger Stacker" (width, height) (offset, offset)

background :: Color
background = white

-- | Number of frames to show per second
fps :: Int
fps = 100

-- | Tests if a list is empty
emptyList :: [a] -> Bool
emptyList [] = True 
emptyList (_:_) = False

---------------------------Initialize Burger Images and Declare Image Retrieval Functions--------------------------

type BurgerImages = [(String, Picture)]

loadPictures :: IO BurgerImages
loadPictures = do
  bunPic <- loadJuicyPNG "/Users/rachelsunderland/Documents/BurgerStacker/SpritesResized/bottomBun2.png"
  pattyPic <- loadJuicyPNG "/Users/rachelsunderland/Documents/BurgerStacker/SpritesResized/patty2.png"
  onionPic <- loadJuicyPNG "/Users/rachelsunderland/Documents/BurgerStacker/SpritesResized/onion2.png"
  lettucePic <- loadJuicyPNG "/Users/rachelsunderland/Documents/BurgerStacker/SpritesResized/lettuce2.png"
  picklePic <- loadJuicyPNG "/Users/rachelsunderland/Documents/BurgerStacker/SpritesResized/pickles2.png"
  cheesePic <- loadJuicyPNG "/Users/rachelsunderland/Documents/BurgerStacker/SpritesResized/cheese2.png"
  topBunPic <- loadJuicyPNG "/Users/rachelsunderland/Documents/BurgerStacker/SpritesResized/topBun3.png"
  endgamePic <- loadJuicyPNG "/Users/rachelsunderland/Documents/BurgerStacker/SpritesResized/gameOver.png"
  wingamePic <- loadJuicyJPG "/Users/rachelsunderland/Documents/BurgerStacker/SpritesResized/youwin.jpg"
  return [("bun", fromJust bunPic),("patty", fromJust pattyPic),("onion", fromJust onionPic), ("lettuce",fromJust lettucePic),("pickle", fromJust picklePic), ("cheese", fromJust cheesePic),("topBun", fromJust topBunPic), ("endgame", fromJust endgamePic),("wingame", fromJust wingamePic)]

getImage :: [Char] -> BurgerStacker -> Picture
getImage name game = fromJust (lookup name (images game)) 

getImageAtIndex :: Int -> BurgerStacker -> Picture
getImageAtIndex index game = let (name,picture) = ((images game) !! index) in picture

getRandomPicture :: Int -> BurgerStacker -> Picture
getRandomPicture index game = getImageAtIndex itemIndex game
    where
        itemIndex = ((randomList game) !! index)

renderImage :: (Float, Float) -> Picture -> Picture
renderImage imagePos image = uncurry translate imagePos (scale 0.20 0.25 image)

renderEndGameImage :: (Float, Float) -> Picture -> Picture
renderEndGameImage imagePos image = uncurry translate imagePos image

------------------------------------Initialize Burger State-------------------------------------------------

-- | Burger Ingredient Containing:
-- 1) Ingredient's Image
-- 2) X and Y coordinates
type BurgerIngredient = (Picture, Float, Float)

data BurgerStacker = Game
    {
    fallingItems :: [BurgerIngredient]
    , stationaryItems :: [BurgerIngredient]
    , numStationaryItems :: Int -- number of items stacked on the bottom bun
    , itemVelocity :: (Float, Float) -- number of units to drop the ingredient per second
    , counter :: Int -- index for random number list for next falling ingredient's picture and initial position
    , randomList :: [Int] -- contains random indexes to get the next ingredient's image
    , randomTuples :: [(Int, Int)] -- contains random (x,y) pairs to get the next ingredient's initial position
    , images :: BurgerImages
    , endGame :: Int  -- 0: continue, 1 is win, 2 is fail
    , timeElapsed :: Int -- number of times update has been called ((1/fps) seconds)
    , rightKeyState :: KeyState -- If they key is 'Up - Released' or 'Down - Pressed'
    , leftKeyState :: KeyState
    , numVisibleItems :: Int
    } 

-- | The starting state for the game of BurgerStacker.
initialStateDynamic :: Picture -> BurgerImages -> [Int] -> [(Int, Int)] -> BurgerStacker
initialStateDynamic picture imageList randomList randomTuples = Game
  {fallingItems = []
  , stationaryItems = [(picture, 0, -220)]
  , numStationaryItems = 1
  , itemVelocity = (0,100)
  , counter = 0
  , randomList = randomList
  , randomTuples = randomTuples
  , images = imageList
  , endGame = 0
  , timeElapsed = 0
  , rightKeyState = Up
  , leftKeyState = Up
  , numVisibleItems = 0
  }

--------------------------------- Rendering the states onto the screen ---------------------------------
-- | Convert a game state into a picture
-- | pictures takes in a list of images to display on the window
-- | appending the bottom bun picture, falling items, and stationary items
-- to be displayed in the window
-- | restartPic = [renderImage (-85,-100) (text ("Press ctrl-r to Restart"))]

render :: BurgerStacker -> Picture 
render game = pictures comboList
    where
        picsFalling = [renderImage (x,y) picture | (picture , x , y) <- (fallingItems game)]
        picsStationary = [renderImage (x,y) picture | (picture , x,y) <- (stationaryItems game)]
        gamePic = picsFalling ++ picsStationary

        endGamePic = [renderEndGameImage (0,0) (getImage "endgame" game)]
        
        congratsPic = [renderImage (-85,0) (text ("Congratulations!"))]
        scorePic = [renderImage (-135,-50) (text ("Your Final Score is: " ++ show (numStationaryItems game)))]
        winGamePic = congratsPic ++ scorePic
        
        comboList = 
          if (endGame game > 0) 
            then 
              if (endGame game == 2) 
                then (endGamePic) 
                else (winGamePic) 
            else 
              gamePic

-- Function is called (1/fps) times a second
update :: Float -> BurgerStacker -> BurgerStacker
update seconds = updateFallingItems . addAdditionalItem . keyDownHandler . moveBurger seconds 

-- | Given position of the topmost ingredient on the burger, and the falling item (x,y), 
-- return if the ingredient has touched the top of the burger
bunCollision :: BurgerStacker -> BurgerIngredient -> BurgerIngredient -> Bool 
bunCollision game (_,x,y) (_,x', y') = withinx && withiny
  where
    withinx = (x' >= x - 30) && (x' <= x + 30)
    withiny = (y' >= y - 30) && (y' <= y + 30)

-- | Update the fallingIngredients, stationary ingredients (ingredients stacked on the bun), and number of stationary ingredients
-- If there are no falling items, return the current state
-- If there are falling items, and the falling item collides with the stacked burger:
--    1) Add the falling ingredient to the list of stationary ingredient
--    2) Remove the falling ingredient from the list of falling ingredients
--    3) Increment the number of stationary items by 1
--    4) Decrement the number of falling items by 1
-- If the top bun has fallen below the screen, remove it from the list of falling items
-- Otherwise:
--    Return the current state
updateFallingItems :: BurgerStacker -> BurgerStacker
updateFallingItems game = game { fallingItems = newFallingItems, stationaryItems = newStationaryItems, numStationaryItems = newNumStationaryItems, numVisibleItems = newNumVisibleItems}
  where
    index = (numStationaryItems game) - 1
    numItems = (numStationaryItems game) + 1

    newNumVisibleItems = if emptyList (fallingItems game)
        then
            (numVisibleItems game)
        else
            if (bunCollision game ((stationaryItems game) !! index) ((fallingItems game) !! 0))
                then
                    ((numVisibleItems game) + 1)
                else 
                    (numVisibleItems game)

    newNumStationaryItems = if emptyList (fallingItems game)
        then
            (numStationaryItems game)
        else
            if (bunCollision game ((stationaryItems game) !! index) ((fallingItems game) !! 0))
                then
                    ((numStationaryItems game) + 1)
                else 
                    (numStationaryItems game)
    
    newStationaryItems = if emptyList (fallingItems game)
        then
            (stationaryItems game)
        else
            if (bunCollision game ((stationaryItems game) !! index) ((fallingItems game) !! 0))
                then
                    (stationaryItems game) ++ [(fallingItems game) !! 0]
                else   
                    (stationaryItems game)

    newFallingItems = if emptyList (fallingItems game)
        then
            (fallingItems game)
        else
            if (bunCollision game ((stationaryItems game) !! index) ((fallingItems game) !! 0))
                then
                    drop 1 (fallingItems game)
                else 
                  if isTopBun && isBelow
                    then 
                      drop 1 (fallingItems game)
                    else
                      (fallingItems game)
                    where
                      (picture,_,y) = ((fallingItems game) !! 0)
                      isTopBun = picture == (getImage "topBun" game)
                      isBelow = (y < -220)

-- | Adds an additional item to the list of falling items when the game begins, and every 3 seconds
-- Generates a random image and random initial (x,y) position for this item
-- The fallingItems list and elapsed time counter are updated
addAdditionalItem :: BurgerStacker -> BurgerStacker
addAdditionalItem game = game {fallingItems = imageList, timeElapsed = newTimeElapsed, counter = newCounter}
  where
    newTimeElapsed = (timeElapsed game) + 1

    newPic = getRandomPicture (counter game) game
    (x,y) = ((randomTuples game) !! (counter game))
    
    imageList = if ((newTimeElapsed `mod` (fps * 3))  == 0 || newTimeElapsed == 1)
        then
            (fallingItems game) ++ [(newPic, fromIntegral x, fromIntegral y)]
        else
            (fallingItems game)

    newCounter = if ((newTimeElapsed `mod` (fps * 3))  == 0 || newTimeElapsed == 1)
        then
            (counter game) + 1
        else
            (counter game)

-- | For all of the falling items, decrease their y position by their velocity * seconds
-- If an falling ingredient (not the top bun) has reached the bottom of the screen:
--    Update the game's state indicating the player lost the game
-- If the top bun has been stacked on top of the burger:
--    Update the game's state indicating the player won the game
moveBurger :: Float -> BurgerStacker -> BurgerStacker
moveBurger seconds game = 
  if isEndGame
    then
      (game {endGame = 2}) 
    else 
      if isTopBun
        then
          (game {endGame = 1}) 
        else
          if (numVisibleItems game) > 5
            then
              (game {fallingItems = newFallingItems, stationaryItems = newStationaryItems, numVisibleItems = 5})
            else
              (game {fallingItems = newFallingItems})
  where
    isBelow = (or [yval < -220 | (_,_,yval) <- (fallingItems game)])
    placeholderImage = getImage "lettuce" game
    (picture,x,y)
      | emptyList (stationaryItems game) = (placeholderImage,2,3) 
      | otherwise = last (stationaryItems game)
    isTopBun = picture == (getImage "topBun" game)
    (picture',x',y')
      | emptyList (fallingItems game) = (placeholderImage,2,3) 
      | otherwise = (fallingItems game) !! 0
    isTopBun2 = picture' == (getImage "topBun" game)
    isEndGame = isBelow && (not isTopBun2)
    (vx, vy) = itemVelocity game
    newFallingItems = [(a, x, y - (vy * seconds)) | (a, x, y) <- (fallingItems game)] 
    newStationaryItems = [(a, x, y - 25) | (a, x, y) <- (stationaryItems game)]  
    
-------------------------------------------- Keyboard Handlers -------------------------------------------------

-- | Respond to left and right key events
handleKeys :: Event -> BurgerStacker -> BurgerStacker

-- If the left key is pressed, move only the stationary items and bottom bun
handleKeys (EventKey (SpecialKey KeyLeft) keyState _ _) game = 
  if keyState == Up
    then
      game {stationaryItems = newStationaryItems, leftKeyState = keyState}
    else
      game {leftKeyState = keyState}
  where
    newStationaryItems = [(a, b-2, c) | (a, b, c) <- (stationaryItems game)]

-- If the right key is pressed, move only the stationary items and bottom bun
handleKeys (EventKey (SpecialKey KeyRight) keyState _ _) game = 
  if keyState == Up
    then
      game {stationaryItems = newStationaryItems, rightKeyState = keyState}
    else 
      game {rightKeyState = keyState}
  where
    newStationaryItems = [(a, b+2, c) | (a, b, c) <- (stationaryItems game)]

-- Do nothing for all other keys.
handleKeys _ game = game

-- | Respond to right or left key presses (keys are held down)
-- Move the stacked burger by the right/left by 2 units 
-- Update the game's state
keyDownHandler :: BurgerStacker -> BurgerStacker
keyDownHandler game =
  if (rightKeyState game) == Down
    then
      game {stationaryItems = newStationaryItemsRight}
    else
      if (leftKeyState game) == Down
        then
          game {stationaryItems = newStationaryItemsLeft}
        else
          game
    where
      newStationaryItemsRight = [(a, b+2, c) | (a, b, c) <- (stationaryItems game)]
      newStationaryItemsLeft = [(a, b-2, c) | (a, b, c) <- (stationaryItems game)]

------------------------------------------------ Main function and Helpers-----------------------------------------

tuplesFromList :: [Int] -> [(Int, Int)]
tuplesFromList [] = []
tuplesFromList [x] = [(x,300)]
tuplesFromList (a:b) = [(a,300)] ++ (tuplesFromList b)

randList :: RandomGen g => Int -> (Int, Int) -> g -> [Int]
randList length range g = map (\x -> randListInf !! x)  [0..length]
  where
    randListInf = randomRs range g

main :: IO ()
main = do
  bunIngredientImages <- loadPictures
  g <- getStdGen
  let randomList = (randList 100 (1,6) g)
  let randomTuples = (tuplesFromList (randList 100 (-200,200) g))
  let (_, bunPic) = (bunIngredientImages !! 0)
  let initialState = (initialStateDynamic bunPic bunIngredientImages randomList randomTuples)
  play window background fps initialState render handleKeys update
  




  