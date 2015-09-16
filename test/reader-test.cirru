
var
  reader $ require :../src/reader
  code $ require :./wasm/test.wasm

console.log $ reader.read code
