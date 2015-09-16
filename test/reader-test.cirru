
var
  reader $ require :../src/reader
  code $ require :./wasm/simple.wasm

var result $ reader.read code
console.log (result.toJS)
