"use strict";

exports.spawnStoreCapacity = function(obj){
    var ret = obj.store
    if (typeof ret !== 'undefined')
        return ret.getFreeCapacity(RESOURCE_ENERGY);
}
