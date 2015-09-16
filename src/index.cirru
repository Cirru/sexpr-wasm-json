
var
  reader $ require :./reader
  writer $ require :./writer

= exports.read reader.read
= exports.write writer.write
