module Sound where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)

foreign import data SOUND :: Effect

type Volume = Number
type URL = String

data SoundEffect = Quiet
                 | Sound URL Volume
                 | ExclusiveSound URL Volume
                 | RepeatSound URL Volume

foreign import play' :: forall e. (SoundEffect -> String) -> SoundEffect -> Eff (sound :: SOUND | e) Unit

effectType :: SoundEffect -> String
effectType Quiet = "quiet"
effectType (Sound _ _) = "sound"
effectType (ExclusiveSound _ _) = "exclusiveSound"
effectType (RepeatSound _ _) = "repeatSound"

play :: forall e. SoundEffect -> Eff (sound :: SOUND | e) Unit
play = play' effectType
