
WebAssembly S-Expression JSON kits
----

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
