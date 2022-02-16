"use strict";

exports.getGameGlobal = function(){ return Game; }
exports.createCreepImpl = function(structure){
    return function(parts){
      return function(role){
        return function(left){
          return function(right){
            return function(){
              var result = structure.createCreep(parts,{utility: 1, role: role});
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
}
exports.rawGetUtility = function(key){
    return function(obj){
        return obj.creeps[key].utility;
    }
}

exports.rawGetRole = function(key){
    return function(obj){
        return obj.creeps[key].role;
    }
}
