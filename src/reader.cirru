
var
  assert $ require :assert
  Immutable $ require :immutable

var
  ({}~ fromJS List Map) Immutable

var
  openParen ":("
  closeParen ":)"
  doubleQuote ":\""
  normalChars ":-_:/.=+@$*"

var helperMany $ \ (method state count)
  var result $ method state
  cond (result.get :failed)
    cond (> count 0) state result
    helperMany method
      state.update :value $ \ (value)
        value.push result.value
      + count 1

var combineMany $ \ (method)
  \ (state)
    helperMany method
      state.set value $ List $ []
      , 0

var helperOr $ \ (methods state)
  cond (is methods.size 0)
    ... state (set :failed true) (:msg ":no match case in or")
    bind (methods.get 0) $ \ (method)
      var result $ method state
      cond (result.get :failed)
        helperOr (methods.slice 1) state
        , result

var combineOr $ \ (methods)
  \ (state)
    helperOr methods state

var bind $ \ (v k) (k v)

var helperChain $ \ (methods state)
  cond (is methods.size 0) state
    bind (methods.get 0) $ \ (method)
      var result $ method state
      cond (result.get :failed) result
        helperChain (methods.slice 1) result

var combineChain $ \ (methods)
  \ (state)
    helperChain methods state

var parseNothing $ \ (state) state

var parseWhitespace $ \ (state)
  var code $ state.get :code
  var firstChar $ code.substr 0 1
  cond (is firstChar.length 1)
    cond (is firstChar whitespace)
      ... state (set :code $ code.substr 1)
      ... state (set :failed true) (set :msg ":whitespace not found")
    ... state (set :failed true) (set :msg ":unexpected EOF")

var parseOpenParen $ \ (state)
  var code $ state.get :code
  var firstChar $ code.substr 0 1
  assert.ok (? firstChar) ":need character"
  cond (is firstChar.length 1)
    cond (is firstChar openParen)
      ... state
        set :code $ code.substr 1
        update :stack $ \ (stack)
          stack.push :expression
      ... state (set :failed true) (set :msg ":openParen not found")
    ... state (set :failed true) (set :msg ":unexpected EOF")

var parseCloseParen $ \ (state)
  var code $ state.get :code
  var firstChar $ code.substr 0 1
  cond (is firstChar.length 1)
    cond (is firstChar closeParen)
      state.set :code $ code.substr 1
      ... state (set :failed true) (set :msg ":closeParen not found")
    ... state (set :failed true) (set :msg ":unexpected EOF")

var parseQuote $ \ (state)
  var code $ state.get :code
  var firstChar $ code.substr 0 1
  cond (is firstChar.length 1)
    cond (is firstChar doubleQuote)
      state.set :code $ code.substr 1
      ... state (set :failed true) (set :msg ":quote not found")
    ... state (set :failed true) (set :msg ":unexpected EOF")

var parseNormalChar $ \ (state)
  var code $ state.get :code
  cond (is code :)
    ... state (set :failed true) (set :msg ":unexpected EOF")
    bind (code.substr 0 1) $ \ (firstChar)
      cond
        or (in normalChars firstChar) (? $ firstChar.match /[a-z])
        ... state (set :code $ code.substr 1) (:value firstChar)
        ... state (set :failed true) (:msg ":not normal character")

var parseEscaped $ \ (state)
  var code $ state.get :code
  cond (is code :)
    ... state (set :failed true) (set :msg ":unexpected EOF")
    bind (code.substr 0 1) $ \ (firstChar)
      cond (is firstChar :\)
        cond (> code.length 1)
          ... state (set :code $ code.substr 2) (set :value $ code.substr 1 1)
          ... state (set :failed true) (set :msg ":no content for escaping")
        ... state (set :failed true) (set :msg ":no escaped pattern")

var parseString $ \ (state)
  var code $ state.get :code
  cond (is code :)
    ... state (set :failed true) (set :msg ":unexpected EOF")
    bind
      combineChain $ List $ [] parseQuote
        combineOr $ List $ []
          combineMany $ combineOr $ List
            [] parseNormalChar parseEscaped
          , parseNothing
        , parseQuote
      \ (result)
        result.update :value $ \ (value)
          value.join :

var parseToken $ \ (state)
  combineMany parseNormalChar

var parseExpression $ \ (state)
  var code $ state.get :code
  cond (is code :)
    ... state (set :failed true) (set :msg ":unexpected EOF")
    combineChain $ List $ [] openParen
      combineOr $ List $ []
        combineMany $ combineOr $ List
          [] parseToken parseWhitespace parseExpression parseString
        , parseNothing
      , closeParen

= exports.read parse
  var initialState $ fromJS $ {}
    :code code
    :failed false
    :value (array)
  parseExpression initialState
