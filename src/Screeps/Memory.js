"use strict";

exports.getMemoryGlobal = function(){ return Memory; }
exports.getRawMemoryGlobal = function(){ return RawMemory; }
exports.getCreepsUtl = function(){
  var res = []
  for (var c in Memory.creeps) {
    res += Memory.creeps[c]["utility"]
  }
  return res
}
exports.getCreepsNames = function(){
  var res = []
  for (var c in Memory.creeps) {
    res += c
  }
  return res
}
exports.setCreepsUtl = function(key,n){
        return Memory.creeps[key] = n
}
