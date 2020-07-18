"use strict";

// module Screeps.FFI

exports.unsafeField = function(key){
    return function(obj){
        return obj[key];
    }
}
exports.runThisEffFn0 = function(key){
    return function(self){
        return function(){
            return self[key]();
        }
    }
}

