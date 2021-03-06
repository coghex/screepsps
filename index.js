// Generated by purs bundle 0.13.8
var PS = {};
(function(exports) {
  "use strict";

  exports.runFn4 = function (fn) {
    return function (a) {
      return function (b) {
        return function (c) {
          return function (d) {
            return fn(a, b, c, d);
          };
        };
      };
    };
  };
})(PS["Data.Function.Uncurried"] = PS["Data.Function.Uncurried"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Function.Uncurried"] = $PS["Data.Function.Uncurried"] || {};
  var exports = $PS["Data.Function.Uncurried"];
  var $foreign = $PS["Data.Function.Uncurried"];
  exports["runFn4"] = $foreign.runFn4;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Data.Maybe"] = $PS["Data.Maybe"] || {};
  var exports = $PS["Data.Maybe"];                 
  var Nothing = (function () {
      function Nothing() {

      };
      Nothing.value = new Nothing();
      return Nothing;
  })();
  var Just = (function () {
      function Just(value0) {
          this.value0 = value0;
      };
      Just.create = function (value0) {
          return new Just(value0);
      };
      return Just;
  })();
  exports["Nothing"] = Nothing;
  exports["Just"] = Just;
})(PS);
(function(exports) {
  "use strict";

  exports.log = function (s) {
    return function () {
      console.log(s);
      return {};
    };
  };
})(PS["Effect.Console"] = PS["Effect.Console"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Effect.Console"] = $PS["Effect.Console"] || {};
  var exports = $PS["Effect.Console"];
  var $foreign = $PS["Effect.Console"];
  exports["log"] = $foreign.log;
})(PS);
(function(exports) {
  "use strict";

  exports._lookup = function (no, yes, k, m) {
    return k in m ? yes(m[k]) : no;
  };

  function toArrayWithKey(f) {
    return function (m) {
      var r = [];
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r.push(f(k)(m[k]));
        }
      }
      return r;
    };
  }
})(PS["Foreign.Object"] = PS["Foreign.Object"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Foreign.Object"] = $PS["Foreign.Object"] || {};
  var exports = $PS["Foreign.Object"];
  var $foreign = $PS["Foreign.Object"];
  var Data_Function_Uncurried = $PS["Data.Function.Uncurried"];
  var Data_Maybe = $PS["Data.Maybe"];
  var lookup = Data_Function_Uncurried.runFn4($foreign["_lookup"])(Data_Maybe.Nothing.value)(Data_Maybe.Just.create);
  exports["lookup"] = lookup;
})(PS);
(function(exports) {
  "use strict";

  exports.getGameGlobal = function(){ return Game; }
})(PS["Screeps.Game"] = PS["Screeps.Game"] || {});
(function(exports) {
  "use strict";

  // module Screeps.FFI

  exports.unsafeField = function(key){
      return function(obj){
          return obj[key];
      }
  }
  exports.unsafeGetFieldEff = function(key){
      return function(obj){
          return function(){
              return obj[key];
          }
      }
  }
  exports.unsafeSetFieldEff = function(key){
      return function(obj){
          return function(val){
              return function(){
                  return obj[key];
              }
          }
      }
  }
})(PS["Screeps.FFI"] = PS["Screeps.FFI"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Screeps.FFI"] = $PS["Screeps.FFI"] || {};
  var exports = $PS["Screeps.FFI"];
  var $foreign = $PS["Screeps.FFI"];
  exports["unsafeField"] = $foreign.unsafeField;
  exports["unsafeGetFieldEff"] = $foreign.unsafeGetFieldEff;
  exports["unsafeSetFieldEff"] = $foreign.unsafeSetFieldEff;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Screeps.Game"] = $PS["Screeps.Game"] || {};
  var exports = $PS["Screeps.Game"];
  var $foreign = $PS["Screeps.Game"];
  var Screeps_FFI = $PS["Screeps.FFI"];                
  var spawns = Screeps_FFI.unsafeField("spawns");
  exports["spawns"] = spawns;
  exports["getGameGlobal"] = $foreign.getGameGlobal;
})(PS);
(function(exports) {
  "use strict";

  exports.getMemoryGlobal = function(){ return Memory; }
})(PS["Screeps.Memory"] = PS["Screeps.Memory"] || {});
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Screeps.Memory"] = $PS["Screeps.Memory"] || {};
  var exports = $PS["Screeps.Memory"];
  var $foreign = $PS["Screeps.Memory"];
  var Screeps_FFI = $PS["Screeps.FFI"];                
  var set = function (memoryGlobal) {
      return function (key) {
          return function (val) {
              return Screeps_FFI.unsafeSetFieldEff(key)(memoryGlobal)(val);
          };
      };
  };
  var get = function (memoryGlobal) {
      return function (key) {
          return Screeps_FFI.unsafeGetFieldEff(key)(memoryGlobal);
      };
  };
  exports["set"] = set;
  exports["get"] = get;
  exports["getMemoryGlobal"] = $foreign.getMemoryGlobal;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.8
  "use strict";
  $PS["Main"] = $PS["Main"] || {};
  var exports = $PS["Main"];
  var Effect_Console = $PS["Effect.Console"];
  var Foreign_Object = $PS["Foreign.Object"];
  var Screeps_Game = $PS["Screeps.Game"];
  var Screeps_Memory = $PS["Screeps.Memory"];                
  var main = function __do() {
      var game = Screeps_Game.getGameGlobal();
      var memory = Screeps_Memory.getMemoryGlobal();
      Screeps_Memory.set(memory)("init")("true")();
      var init = Screeps_Memory.get(memory)("init")();
      var spawn = Foreign_Object.lookup("Spawn1")(Screeps_Game.spawns(game));
      return Effect_Console.log(init)();
  };
  exports["main"] = main;
})(PS);
PS["Main"].main();