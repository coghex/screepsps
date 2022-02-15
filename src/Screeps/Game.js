"use strict";

exports.getGameGlobal = function(){ return Game; }
exports.createCreepImpl = function(structure){
    return function(parts){
      return function(left){
        return function(right){
          return function(){
            var result = structure.createCreep(parts,{utility: 1});
            if (typeof result === "string"){
                return right(result);
            } else {
                return left(result);
            }
          }
        }
      }
    }
}

