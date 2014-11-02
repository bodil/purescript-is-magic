module Sound where

import Control.Monad.Eff

foreign import data Sound :: !

type Volume = Number
type URL = String

data SoundEffect = Quiet
                 | Sound URL Volume
                 | ExclusiveSound URL Volume
                 | RepeatSound URL Volume

foreign import play """
  var play = (function() {
    var active = [];
    function play(o) {
      return function() {
        if (!(o instanceof Quiet)) {
          if (o instanceof ExclusiveSound) {
            active.forEach(function(i) { i.pause(); });
            active = [];
          }
          var el = new Audio(o.value0);
          el.volume = o.value1;
          el.autoplay = true;
          el.addEventListener("ended", function(e) {
            active = active.filter(function(i) { return i !== e.target });
          });
          el.loop = (o instanceof RepeatSound);
          active.push(el);
        }
      };
    }
    return play;
  })();""" :: forall e. SoundEffect -> Eff (sound :: Sound | e) Unit
