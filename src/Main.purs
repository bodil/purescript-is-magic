module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Int as Int
import Data.Maybe as Maybe
import Data.Tuple(Tuple(..))
import DOM (DOM)
import Math as Math
import Signal (Signal, foldp, runSignal, sampleOn, (<~), (~), (~>))
import Signal.Time (Time, every)
import Signal.DOM (keyPressed, tap)
import Sound (SOUND, SoundEffect(..), play)

-- | This type defines every object on screen.
-- |
-- | It has an `id` string, which is mapped to the HTML `id` attribute, and
-- | a `css` string, which maps to the HTML `class` attribute.
-- |
-- | The `x` and `y` pair, and `baseX`/`baseY`, define the object's position
-- | on screen. `baseX` and `baseY` define a base position, which helps us
-- | think about the game world relative to the object: for instance, we want
-- | to think of Pinkie's `y` coordinate as `0` when she's on the ground, so
-- | we give her a `baseY` which gets added to `y` so that this is true.
-- |
-- | `vx` and `vy` define the object's velocity: every frame, we add these
-- | to `x` and `y` to produce steady movement.
-- |
-- | Finally, we can set a `sound` which will cause a sound effect to start
-- | playing.
type GameObject =
  { id :: String, css :: String
  , x :: Number, y :: Number
  , baseX :: Number, baseY :: Number
  , vx :: Number, vy :: Number
  , sound :: SoundEffect }

-- | A `Bounds` object defines a rectangle on screen.
type Bounds = { x1 :: Number, x2 :: Number, y1 :: Number, y2 :: Number }

-- | Get a `Bounds` from a `GameObject`. This is just a useful approximation:
-- | `GameObject`s don't actually have a size, so we just assume it's roughly
-- | 64 pixels. This is close enough for collision detection between Pinkie
-- | and the other objects in the game.
bounds :: GameObject -> Bounds
bounds a = { x1: a.x + a.baseX, y1: a.y + a.baseY
           , x2: a.x + a.baseX + 64.0, y2: a.y + a.baseY + 64.0 }

-- | Test whether two `GameObject`s are touching each other.
intersects :: GameObject -> GameObject -> Boolean
intersects a b = not ((b'.x1 > a'.x2) || (b'.x2 < a'.x1)
                   || (b'.y1 > a'.y2) || (b'.y2 < a'.y1))
  where a' = bounds a
        b' = bounds b

-- | Update a `GameObject` on screen.
-- |
-- | This is implemented using an external JavaScript function, so we just
-- | declare its type signature here, and the fact that it's declared in
-- | an external file (`foreign import`). You can see the source for it in
-- | the file `Main.js`.
foreign import renderObject :: forall e. GameObject -> Eff (dom :: DOM | e) Unit

-- | The initial state of the Pinkie `GameObject`.
initialPinkie :: GameObject
initialPinkie =
  { id: "pinkie", css: ""
  , x: 0.0, y: 0.0
  , baseX: 0.0, baseY: 276.0
  , vx: 0.0, vy: 0.0
  , sound: Quiet }

-- | The initial state of the coin `GameObject`.
initialCoin :: GameObject
initialCoin =
  { id: "coin", css: ""
  , x: 1600.0, y: 40.0
  , baseX: 0.0, baseY: 0.0
  , vx: -6.0, vy: 0.0
  , sound: Quiet }

-- | The initial state of the hater obstacle `GameObject`.
initialHater :: GameObject
initialHater =
  { id: "hater", css: ""
  , x: 1600.0, y: 300.0
  , baseX: 0.0, baseY: 0.0
  , vx: -8.0, vy: 0.0
  , sound: Quiet }

-- | The frame rate.
-- |
-- | We set this up as a signal which will tick every 33 milliseconds.
-- | We base all our other signals off of this tick.
frameRate :: Signal Number
frameRate = every 33.0

-- | Get the remainder of a number divided by an integer.
numberMod :: Number -> Int -> Number
numberMod a b =
  let a' = Maybe.fromMaybe 0 (Int.fromNumber (Math.floor a))
  in Int.toNumber (a' `mod` b)

-- | Set up the moving ground `GameObject`.
-- |
-- | This is a signal which produces an updated `GameObject` for the
-- | ground every tick. The ground moves from right to left until we
-- | run out of ground texture, then it loops back, so it looks like
-- | a seamless infinite texture.
ground :: Signal GameObject
ground = frameRate ~> \n ->
  { id: "ground", css: ""
  , x: ((n / 33.0) `numberMod` 64) * -8.0, y: 0.0
  , baseX: -128.0, baseY: 384.0
  , vx: 0.0, vy: 0.0
  , sound: Quiet }

-- | This function takes an initial state and a current state for a
-- | `GameObject`, and will return either the current state unchanged,
-- | or if the `GameObject` has gone too far off screen, it will return
-- | the initial state. We use this to reset the coins and the haters
-- | as they go off screen.
reset :: GameObject -> GameObject -> GameObject
reset i o | (o.x + o.baseX) < -100.0
         || (o.y + o.baseY) < -100.0
         || (o.y + o.baseY) > 3000.0 = i
reset _ o = o

-- | Play the sound effect associated with a `GameObject`.
playSound :: forall e. GameObject -> Eff (sound :: SOUND | e) Unit
playSound { sound } = play sound

-- | Clear the sound effect on a `GameObject`.
clearSound :: GameObject -> GameObject
clearSound o = o { sound = Quiet }

-- | Apply gravity to a `GameObject`.
-- |
-- | This just applies a downwards velocity of 0.98 (gravity!) each tick.
-- | If the object is going upwards, this makes it fall back down as the
-- | upwards velocity gradually turns into downwards velocity.
gravity :: GameObject -> GameObject
gravity o = o { vy = o.vy + 0.98 }

-- | Apply a `GameObject`'s velocity to its current position.
velocity :: GameObject -> GameObject
velocity o = o { x = o.x + o.vx
               , y = o.y + o.vy
               }

-- | Make a `GameObject` stop moving downwards when it hits the ground,
-- | and clear any animation that might be going.
solidGround :: GameObject -> GameObject
solidGround o =
  if o.y >= 0.0
  then o { y = 0.0, vy = 0.0, css = "" }
  else o

-- | Make a `GameObject` (Pinkie) jump by giving it an upwards velocity,
-- | and set a jumping animation and sound effect.
jump :: Boolean -> GameObject -> GameObject
jump true p@{ y: 0.0 } = p { vy = -20.0, css = "jumping", sound = Sound "sfx/jump.ogg" 1.0 }
jump _ p = p

-- | Handle game over (when Pinkie collides with a hater).
-- |
-- | Takes the hater object and the Pinkie object, and tests if they're touching.
-- | If so, Pinkie gets the game over animation, we play the game over sound, and
-- | we start her moving slowly upwards.
-- |
-- | If she's already got the game over animation, we make her fall slowly to the
-- | ground until she's off screen, which causes her to reset and resume the game.
-- |
-- | If neither of these conditions are true, we apply Pinkie's game logic function
-- | (that's the third argument) to her `GameObject` and return the result of that.
hated :: GameObject -> GameObject -> (GameObject -> GameObject) -> GameObject
hated _ p@{ css: "gameover" } _ =
  reset initialPinkie $ clearSound $ velocity $ p { vy = p.vy + 0.5 }
hated h p _ | intersects h p =
  velocity $ p { css = "gameover", vy = -15.0, sound = ExclusiveSound "sfx/gameover.ogg" 1.0 }
hated _ p cont = cont p

-- | Pinkie's game logic function.
-- |
-- | It takes as arguments a tuple of things which can affect her (a boolean flag
-- | which goes true if the player has pressed the jump button, and the hater
-- | object which causes her to go into game over if she touches it) and her
-- | current `GameObject`, and applies these effects on her in order:
-- |
-- | * If she's in game over, override normal game logic until she's reset (see the
-- |   `hated` function). If not, carry on down the list.
-- | * `solidGround` makes sure she doesn't fall through the ground.
-- | * `gravity` makes sure she falls back to the ground if she's not on it.
-- | * `velocity` applies her current velocity.
-- | * `jump` makes her jump if the jump button is pressed.
-- | * `clearSound` clears any current sound effect so it doesn't keep restarting.
pinkieLogic :: (Tuple Boolean GameObject) -> GameObject -> GameObject
pinkieLogic (Tuple jumpPressed h) p =
  hated h p
  (solidGround
   <<< gravity
   <<< velocity
   <<< jump jumpPressed
   <<< clearSound)

-- | Define the Pinkie signal.
-- |
-- | This gives us a signal of Pinkie `GameObject`s, updated using the
-- | `pinkieLogic` function.
pinkie :: Signal (Tuple Boolean GameObject) -> Signal GameObject
pinkie input = foldp pinkieLogic initialPinkie
               (sampleOn frameRate input)

-- | Game logic for the hater.
-- |
-- | This function applies velocity to the hater, making it move across
-- | the screen, and resets its position once it goes off screen.
haterLogic :: Time -> GameObject -> GameObject
haterLogic _ h =
  velocity $ reset initialHater h

-- | Define the hater signal.
hater :: Signal GameObject
hater = foldp haterLogic initialHater frameRate

-- | Game logic for the coin.
-- |
-- | This takes Pinkie as an input in addition to the coin's current state,
-- | because Pinkie's position can affect the coin: when she touches it,
-- | the collect sound plays and it changes from moving steadily across the
-- | screen to moving at an increasing rate upwards.
-- |
-- | Whichever way it's moving, when it goes off screen, its position is reset,
-- | making it seem like another coin comes on screen.
coinLogic :: GameObject -> GameObject -> GameObject
coinLogic _ c | c.vy < 0.0 =
  clearSound $ velocity $ reset initialCoin c { vy = c.vy * 2.0 }
coinLogic p c | intersects p c =
  c { vx = 0.0, vy = -1.0, sound = Sound "sfx/coin.ogg" 1.0 }
coinLogic p c =
  clearSound $ velocity $ reset initialCoin c

-- | Define the coin signal.
coin :: Signal GameObject -> Signal GameObject
coin = foldp coinLogic initialCoin

-- | The game's main function.
-- |
-- | This sets up all our `GameObject` signals, collects them into
-- | a single signal of a list of `GameObject`s, and maps the `renderObject`
-- | function over that, causing every object to be updated whenever it
-- | changes.
-- |
-- | It also sets up our inputs: the jump button is either the space bar,
-- | or tapping the screen on touchscreen devices.
main :: Eff (dom :: DOM, sound :: SOUND) Unit
main = do
  play $ RepeatSound "sfx/smile.ogg" 0.3
  spaceBar <- keyPressed 32
  taps <- tap
  let pinkie' = pinkie $ Tuple <~ (spaceBar <> taps) ~ hater
      scene = ground <> hater <> pinkie' <> coin pinkie'
  runSignal $ scene ~> renderObject
  runSignal $ scene ~> playSound
