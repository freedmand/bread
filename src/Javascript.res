type primitiveType =
  | Number(float)
  | Text(string)
  | Boolean(bool)
  | None

type rec js = array<statement>
and statement =
  | InitVariable(variable, expression)
  | Assign(variable, expression)
  | Expression(expression)
and variable = string
and expression =
  | Constant(primitiveType)
  | Variable(variable)
  | Plus(expression, expression)
  | Fn(array<variable>, js)

let rec jsToString: (js) => string = (js) => Js.Array.joinWith("", Js.Array.map(statement => statementToString(statement), js))
and statementToString: (statement) => string = (statement) => {
  switch (statement) {
    | InitVariable(variable, expression) => `var ${variable}=${expressionToString(expression)};`
    | Assign(variable, expression) => `${variable}=${expressionToString(expression)};`
    | Expression(expression) => `${expressionToString(expression)};`
  }
}
and expressionToString: (expression) => string = (expression) => {
  switch (expression) {
    | Constant(Number(number)) => Js.Json.stringify(Js.Json.number(number))
    | Constant(Text(text)) => Js.Json.stringify(Js.Json.string(text))
    | Constant(Boolean(bool)) => Js.Json.stringify(Js.Json.boolean(bool))
    | Constant(None) => "null"
    | Variable(variable) => variable
    | Plus(a, b) => `${expressionToString(a)}+${expressionToString(b)}`
    | Fn(variables, js) => `function(${Js.Array.joinWith(",", variables)}){${jsToString(js)}}`
  }
}