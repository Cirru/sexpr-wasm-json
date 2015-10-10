
WebAssembly S-Expression JSON Kit
----

Reads S-Expression WASM into JSON and generates code from JSON

### Usage

```bash
npm i sexpr-wasm-json
```

```coffee
s = require 'sexpr-wasm-json'
s.read "(i32.add (i32.const 1) (i32.const 2))" # parse wasm S-Expression code
s.write ["i32.add", ["i32.const", "1"], ["i32.const", "2"]] # generate S-Expression
```

### Develop

With Webpack and a browser:

```bash
gulp html
webpack
```

### License

MIT
