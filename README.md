
WebAssembly S-Expression JSON kits
----

WIP...

* design reader `<-- currently`
* implement reader
* test reader
* design writer
* implement writer
* test writer

### Usage

```
npm i sexpr-wasm-json
```

```coffee
s = require 'sexpr-wasm-json'
s.read "code" # parse wasm S-Expression file
s.write {type: 'expression'} # generate S-Expression
```

### License

MIT
