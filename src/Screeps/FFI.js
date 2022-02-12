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
                obj[key] = val;
            }
        }
    }
}
exports.unsafeDeleteFieldEff = function(key){
  return function(obj){
      return function(){
        delete obj[key];
      }
  }
}
exports.runThisEffFn0 = function(key){
    return function(self){
        return function(){
            return self[key]();
        }
    }
}
exports.runThisEffFn1 = function(key){
    return function(self){
        return function(a){
            return function(){
                return self[key](a);
            }
        }
    }
}
exports.runThisFn0 = function(key){
  return function(self){
    return self[key]();
  }
}

exports.runThisFn1 = function(key){
  return function(self){
    return function(a){
      return self[key](a);
    }
  }
}
