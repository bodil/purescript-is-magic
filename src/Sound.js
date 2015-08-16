// module Sound

var active = [];

exports["play'"] = function play(type) {
  return function(o) {
    return function() {
      if (type(o) !== "quiet") {
        if (type(o) === "exclusiveSound") {
          active.forEach(function(i) { i.pause(); });
          active = [];
        }
        var el = new Audio(o.value0);
        el.volume = o.value1;
        el.autoplay = true;
        el.addEventListener("ended", function(e) {
          active = active.filter(function(i) { return i !== e.target });
        });
        el.loop = type(o) === "repeatSound";
        active.push(el);
      }
    };
  };
};
