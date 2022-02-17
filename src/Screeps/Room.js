"use strict";

exports.setRoomMem = function(room,nharvs,maxnharvs){
    return room.memory = { "nharvs" : nharvs, "maxnharvs" : maxnharvs };
}
