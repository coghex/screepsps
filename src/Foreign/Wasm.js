"use strict";


const bytecode = require('addTwo');
const wasm = new WebAssembly.Module(bytecode);
const imports = {};
imports.env = {
    memoryBase: 0,
    tableBase: 0,
    memory: new WebAssembly.Memory({ initial: 256 }),
    table: new WebAssembly.Table({ initial: 0, element: 'anyfunc' })
};
const wasmInstance = new WebAssembly.Instance(wasm, imports);

exports.calcUtility = function(x){
    return wasmInstance.exports.addTwo(x,3);
}
