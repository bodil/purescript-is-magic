module Sound where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)

foreign import data SOUND :: Effect

type Volume = Number
type URL = String

-- | The various types of sound effect.
-- |
-- | * `Quiet` means no sound effect.
-- | * `Sound` plays a sound effect once at a given volume.
-- | * `ExclusiveSound` causes other running sound effects to
-- |    stop before it starts playing.
-- | * `RepeatSound` plays a looping sound effect.
data SoundEffect = Quiet
                 | Sound URL Volume
                 | ExclusiveSound URL Volume
                 | RepeatSound URL Volume

-- | The sound engine is an external JavaScript function.
foreign import play' :: forall e. (SoundEffect -> String) -> SoundEffect -> Eff (sound :: SOUND | e) Unit

-- | A helper function to allow the external JS code to figure out which type
-- | of `SoundEffect` it's been given.
effectType :: SoundEffect -> String
effectType Quiet = "quiet"
effectType (Sound _ _) = "sound"
effectType (ExclusiveSound _ _) = "exclusiveSound"
effectType (RepeatSound _ _) = "repeatSound"

-- | Play a `SoundEffect`.
play :: forall e. SoundEffect -> Eff (sound :: SOUND | e) Unit
play = play' effectType
