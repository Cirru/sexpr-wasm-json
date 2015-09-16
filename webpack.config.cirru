
var
  webpack $ require :webpack

= module.exports $ {}
  :entry $ {}
    :vendor $ [] :immutable
    :main :./test/writer-test.cirru
  :module $ {}
    :loaders $ []
      {} (:test /\.cirru$) (:loader :cirru-script)
      {} (:test /\.wasm$) (:loader :raw)
      {} (:test /\.json$) (:loader :json)
  :resolve $ {}
    :extensions $ [] : :.js :.cirru
  :output $ {}
    :filename :[name].js
    :path :build/
  :plugins $ []
    new webpack.optimize.CommonsChunkPlugin :vendor :vendor.js
