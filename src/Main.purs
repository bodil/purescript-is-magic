module Main where

import Control.Monad.Eff
import qualified Data.Int as Int
import qualified Data.Maybe as Maybe
import Data.Tuple(Tuple(..))
import DOM
import qualified Math as Math
import Prelude
import Signal
import Signal.Time
import Signal.DOM
import Sound

type GameObject =
  { id :: String, css :: String
  , x :: Number, y :: Number
  , baseX :: Number, baseY :: Number
  , vx :: Number, vy :: Number
  , sound :: SoundEffect }

type Bounds = { x1 :: Number, x2 :: Number, y1 :: Number, y2 :: Number }
bounds :: GameObject -> Bounds
bounds a = { x1: a.x + a.baseX, y1: a.y + a.baseY
           , x2: a.x + a.baseX + 64.0, y2: a.y + a.baseY + 64.0 }
intersects :: GameObject -> GameObject -> Boolean
intersects a b = not ((b'.x1 > a'.x2) || (b'.x2 < a'.x1)
                   || (b'.y1 > a'.y2) || (b'.y2 < a'.y1))
  where a' = bounds a
        b' = bounds b

foreign import renderObject :: forall e. GameObject -> Eff (dom :: DOM | e) Unit

initialPinkie :: GameObject
initialPinkie =
  { id: "pinkie", css: ""
  , x: 0.0, y: 0.0
  , baseX: 0.0, baseY: 276.0
  , vx: 0.0, vy: 0.0
  , sound: Quiet }

initialCoin :: GameObject
initialCoin =
  { id: "coin", css: ""
  , x: 1600.0, y: 40.0
  , baseX: 0.0, baseY: 0.0
  , vx: -6.0, vy: 0.0
  , sound: Quiet }

initialHater :: GameObject
initialHater =
  { id: "hater", css: ""
  , x: 1600.0, y: 300.0
  , baseX: 0.0, baseY: 0.0
  , vx: -8.0, vy: 0.0
  , sound: Quiet }

frameRate :: Signal Number
frameRate = every 33.0

numberMod :: Number -> Int -> Number
numberMod a b =
  let a' = Maybe.fromMaybe 0 (Int.fromNumber (Math.floor a))
  in Int.toNumber (a' `mod` b)

ground :: Signal GameObject
ground = frameRate ~> \n ->
  { id: "ground", css: ""
  , x: ((n / 33.0) `numberMod` 64) * -8.0, y: 0.0
  , baseX: -128.0, baseY: 384.0
  , vx: 0.0, vy: 0.0
  -- psc isn't able to infer the type of the Nothing here, unlike above
  , sound: Quiet }

reset :: GameObject -> GameObject -> GameObject
reset i o | (o.x + o.baseX) < -100.0
         || (o.y + o.baseY) < -100.0
         || (o.y + o.baseY) > 3000.0 = i
reset _ o = o

playSound :: forall e. GameObject -> Eff (sound :: Sound | e) Unit
playSound { sound = sound } = play sound

clearSound :: GameObject -> GameObject
clearSound o = o { sound = Quiet }

gravity :: GameObject -> GameObject
gravity o = o { vy = o.vy + 0.98 }

velocity :: GameObject -> GameObject
velocity o = o { x = o.x + o.vx
                       , y = o.y + o.vy }

solidGround :: GameObject -> GameObject
solidGround o =
  if o.y >= 0.0
  then o { y = 0.0, vy = 0.0, css = "" }
  else o

jump :: Boolean -> GameObject -> GameObject
jump true p@{ y = 0.0 } = p { vy = -20.0, css = "jumping", sound = Sound "sfx/jump.ogg" 1.0 }
jump _ p = p

hated :: GameObject -> GameObject -> (GameObject -> GameObject) -> GameObject
hated _ p@{ css = "gameover" } _ =
  reset initialPinkie $ clearSound $ velocity $ p { vy = p.vy + 0.5 }
hated h p _ | intersects h p =
  velocity $ p { css = "gameover", vy = -15.0, sound = ExclusiveSound "sfx/gameover.ogg" 1.0 }
hated _ p cont = cont p

pinkieLogic :: (Tuple Boolean GameObject) -> GameObject -> GameObject
pinkieLogic (Tuple jumpPressed h) p =
  hated h p
  (solidGround
   <<< gravity
   <<< velocity
   <<< jump jumpPressed
   <<< clearSound)

pinkie :: Signal (Tuple Boolean GameObject) -> Signal GameObject
pinkie input = foldp pinkieLogic initialPinkie
               (sampleOn frameRate input)

haterLogic :: Time -> GameObject -> GameObject
haterLogic _ h =
  velocity $ reset initialHater h

hater :: Signal GameObject
hater = foldp haterLogic initialHater frameRate

coinLogic :: GameObject -> GameObject -> GameObject
coinLogic _ c | c.vy < 0.0 =
  clearSound $ velocity $ reset initialCoin c { vy = c.vy * 2.0 }
coinLogic p c | intersects p c =
  c { vx = 0.0, vy = -1.0, sound = Sound "sfx/coin.ogg" 1.0 }
coinLogic p c =
  clearSound $ velocity $ reset initialCoin c

coin :: Signal GameObject -> Signal GameObject
coin = foldp coinLogic initialCoin

main :: Eff (dom :: DOM, sound :: Sound) Unit
main = do
  play $ RepeatSound "sfx/smile.ogg" 0.3
  spaceBar <- keyPressed 32
  taps <- tap
  let pinkie' = pinkie $ Tuple <~ (spaceBar <> taps) ~ hater
      scene = ground <> hater <> pinkie' <> coin pinkie'
  runSignal $ scene ~> renderObject
  runSignal $ scene ~> playSound
