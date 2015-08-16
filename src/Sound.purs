module Sound where

import Control.Monad.Eff
import Prelude

foreign import data Sound :: !

type Volume = Number
type URL = String

data SoundEffect = Quiet
                 | Sound URL Volume
                 | ExclusiveSound URL Volume
                 | RepeatSound URL Volume

foreign import play' :: forall e. (SoundEffect -> String) -> SoundEffect -> Eff (sound :: Sound | e) Unit

effectType :: SoundEffect -> String
effectType Quiet = "quiet"
effectType (Sound _ _) = "sound"
effectType (ExclusiveSound _ _) = "exclusiveSound"
effectType (RepeatSound _ _) = "repeatSound"

play :: forall e. SoundEffect -> Eff (sound :: Sound | e) Unit
play = play' effectType
