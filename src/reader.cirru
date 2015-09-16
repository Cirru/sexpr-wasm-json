
var
  assert $ require :assert
  Immutable $ require :immutable

var
  ({}~ fromJS List) Immutable

var
  openParen ":("
  closeParen ":)"
  doubleQuote ":\""
  normalChars ":-_:/.=+@$*"
  whitespace ": \n"
  tokenEndChars ": )\n"

var helperMany $ \ (method state count)
  -- console.log :helperMany (state.toJS) count
  var result $ method state
  var oldValue $ state.get :value
  cond (result.get :failed)
    cond (> count 0) state result
    helperMany method
      result.update :value $ \ (value)
        oldValue.push value
      + count 1

var combineMany $ \ (method)
  \ (state)
    -- console.group :combineMany
    var result $ helperMany method
      state.set :value $ List $ []
      , 0
    -- console.log :combineMany :result (result.toJS)
    -- console.groupEnd
    result.update :value $ \ (value)
      value.filter $ \ (x) (isnt x null)

var helperOr $ \ (methods state)
  -- console.log :helperOr
  cond (is methods.size 0)
    ... state (set :failed true) (set :msg ":no match case in or")
    bind (methods.get 0) $ \ (method)
      var result $ method state
      -- console.info :combineOr :trying (state.toJS)
      cond (result.get :failed)
        helperOr (methods.slice 1) state
        , result

var combineOr $ \ ((methods))
  = methods $ fromJS methods
  \ (state)
    -- console.group :combineOr
    var result $ helperOr methods state
    -- console.log :combineOr :result (result.toJS)
    -- console.groupEnd
    return result

var bind $ \ (v k) (k v)

var helperChain $ \ (methods state)
  -- console.log :helperChain
  cond (is methods.size 0) state
    bind (methods.get 0) $ \ (method)
      var result $ method state
      var oldValue $ state.get :value
      = result $ ... result
        update :value $ \ (value)
          oldValue.push value
      cond (result.get :failed) result
        helperChain (methods.slice 1) result

var combineChain $ \ ((methods))
  = methods $ fromJS methods
  \ (state)
    -- console.group :combineChain
    var result $ helperChain methods (state.set :value (List $ []))
    -- console.info :combineChain :result (result.toJS)
    -- console.groupEnd
    result.update :value $ \ (value)
      value.find $ \ (x) (isnt x null)

var parseNothing $ \ (state)
  -- console.log :parseNothing
  state.set :value null

var parseWhitespace $ \ (state)
  -- console.log :parseWhitespace
  var code $ state.get :code
  var firstChar $ code.substr 0 1
  cond (is firstChar.length 1)
    cond (in (whitespace.split :) firstChar)
      ... state (set :code $ code.substr 1) (set :value null)
      ... state (set :failed true) (set :msg ":whitespace not found")
    ... state (set :failed true) (set :msg ":unexpected EOF")

var parseOpenParen $ \ (state)
  -- console.log :parseOpenParen
  var code $ state.get :code
  var firstChar $ code.substr 0 1
  cond (is firstChar.length 1)
    cond (is firstChar openParen)
      ... state (set :code $ code.substr 1) (set :value null)
      ... state (set :failed true) (set :msg ":openParen not found")
    ... state (set :failed true) (set :msg ":unexpected EOF")

var parseCloseParen $ \ (state)
  -- console.log :parseCloseParen
  var code $ state.get :code
  var firstChar $ code.substr 0 1
  cond (is firstChar.length 1)
    cond (is firstChar closeParen)
      ... state (set :code $ code.substr 1) (set :value null)
      ... state (set :failed true) (set :msg ":closeParen not found")
    ... state (set :failed true) (set :msg ":unexpected EOF")

var parseQuote $ \ (state)
  -- console.log :parseQuote
  var code $ state.get :code
  var firstChar $ code.substr 0 1
  cond (is firstChar.length 1)
    cond (is firstChar doubleQuote)
      ... state (set :code $ code.substr 1) (set :value null)
      ... state (set :failed true) (set :msg ":quote not found")
    ... state (set :failed true) (set :msg ":unexpected EOF")

var parseNormalChar $ \ (state)
  -- console.log :parseNormalChar (state.toJS)
  var code $ state.get :code
  cond (is code :)
    ... state (set :failed true) (set :msg ":unexpected EOF")
    bind (code.substr 0 1) $ \ (firstChar)
      cond
        or (in normalChars firstChar) (? $ firstChar.match /[a-z0-9])
        ... state (set :code $ code.substr 1) (set :value firstChar)
        ... state (set :failed true) (set :msg ":not normal character")

var parseEscaped $ \ (state)
  -- console.log :parseEscaped
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
  -- console.log :parseString
  var method $ combineChain parseQuote
    combineOr
      combineMany $ combineOr parseNormalChar parseEscaped
      , parseNothing
    , parseQuote
  var result $ method state
  result.update :value $ \ (value)
    + :: (value.join :)

var parseTokenEnd $ \ (state)
  -- console.log :parseTokenEnd
  var code $ state.get :code
  var firstChar $ code.substr 0 1
  cond (in (tokenEndChars.split :) firstChar) state
    ... state (set :failed true) (set :msg ":token not end")

var parseToken $ \ (state)
  -- console.log :parseToken
  var method $ combineChain
    combineMany parseNormalChar
    , parseTokenEnd
  var result $ method state
  result.update :value $ \ (value)
    value.join :

var parseExpression $ \ (state)
  -- console.log :parseExpression
  var method $ combineChain parseOpenParen
    combineOr
      combineMany
        combineOr parseToken parseWhitespace parseExpression parseString
      , parseNothing
    , parseCloseParen
  var result $ method state
  result.update :value $ \ (value) value

var parseEof $ \ (state)
  -- console.log :parseEof
  var code $ state.get :code
  cond (is code :)
    ... state (set :value null)
    ... state (set :failed true) (set :msg ":not EOF")

var parseProgram $ \ (state)
  -- console.log :parseProgram
  var method $ combineChain
    combineOr parseWhitespace parseNothing
    , parseExpression
    combineOr parseWhitespace parseNothing
    , parseEof
  var result $ method state
  -- console.log :parseProgram (result.toJS)
  return result

= exports.read $ \ (code)
  -- console.log :read
  var initialState $ fromJS $ {}
    :code code
    :failed false
    :value (array)
    :msg :initial
  parseProgram initialState
