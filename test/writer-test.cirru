
var
  writer $ require :../src/writer
  data $ require :./json/test.json

var result $ writer.write data
console.log result
