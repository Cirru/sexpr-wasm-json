
var
  Immutable $ require :immutable

var
  ({}~ fromJS List) Immutable

var
  doubleQuote ":\""
  newline ":\n"
  openParen ":("
  closeParen ":)"
  blank ": "

var bind $ \ (v k) (k v)

var escapeToken $ \ (token)
  var firstChar $ token.substr 0 1
  cond (is firstChar ::)
    + doubleQuote (token.substr 1) doubleQuote
    , token

var isShallow $ \ (node)
  node.every $ \ (x)
    is (typeof x) :string

var makeIndent $ \ (result n)
  cond (is n 0) result
    makeIndent (+ result ":  ") (- n 1)

var renderToken $ \ (state)
  console.log :renderToken (state.toJS)
  ... state
    update :code $ \ (code)
      + code (escapeToken $ state.get :node)

var helperExpression $ \ (state index)
  console.log :helperExpression (state.toJS) index
  var expression $ state.get :node
  cond (is expression.size 0) state
    bind (expression.get 0) $ \ (child)
      var result $ render $ ... state
        set :node child
        update :code $ \ (code)
          cond (> index 0) (+ code blank) code
      helperExpression
        ... result
          set :node (expression.slice 1)
        + index 1

var renderExpression $ \ (state)
  console.log :renderExpression (state.toJS)
  var indentation $ makeIndent : $ state.get :tabs
  var result $ helperExpression
    ... state
      update :code $ \ (code)
        + code newline indentation openParen
      update :tabs $ \ (tabs) (+ tabs 1)
    , 0
  ... result
    update :code $ \ (code)
      + code closeParen
    update :tabs $ \ (tabs) (- tabs 1)

var renderInlineExpression $ \ (state)
  console.log :renderInlineExpression (state.toJS)
  var result $ helperExpression
    ... state
      update :code $ \ (code)
        + code openParen
    , 0
  ... result
    update :code $ \ (code)
      + code closeParen

var render $ \ (state)
  console.log :render (state.toJS)
  var node $ state.get :node
  cond (List.isList node)
    cond (isShallow node)
      renderInlineExpression state
      renderExpression state
    renderToken state

= exports.write $ \ (tree)
  = tree $ fromJS tree
  var initialState $ fromJS $ {}
    :code :
    :node tree
    :tabs 0
  renderExpression initialState
