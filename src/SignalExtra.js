"use strict";

// module SignalExtra

exports.foldpEP = function (constant, upd, seed, sig) {
  var acc = seed;
  var out = constant(acc);
  sig.subscribe(function(val) {
    acc = upd(val)(acc)();
    out.set(acc);
  });
  return out;
};
