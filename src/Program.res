

type rec program = statements
and statements = list<statement>
and statement =
  | Let(variable, expression)
  | Expression(expression)
  | Return(expression)
and expression =
  | Constant(constant)
  | Variable(variable)
  | ConsoleLog(string, expression)
  | Assign(variable, expression)
  | Plus(expression, expression)
  | Fn(list<variable>, statements)
  | Reactive(list<variable>, statements)
  | Call(expression, list<expression>)
  | Element(expression, list<(attribute, expression)>, list<expression>)
  | Fragment(list<expression>)
and constant =
  | Number(float)
  | Text(string)
  | Bool(bool)
  | None
  | Unit
and attribute =
  | Attribute(expression)
  | EventAttribute(expression)
and variable = string

// Constant helpers
let number = x => Constant(Number(x))
let text = x => Constant(Text(x))
let bool = x => Constant(Bool(x))

// A mechanism to return a globally unique id
let globalId = ref(0)
let getId: () => string = () => {
  globalId := globalId.contents + 1
  `_${Belt.Int.toString(globalId.contents)}`
}

type context = {
  stackDepth: int,
  variables: Belt.Map.String.t<expression>
};
let freshContext: context = {
  stackDepth: 0,
  variables: Belt.Map.String.empty
}

exception VariableNotFound(variable)
exception ExpectedNumbersStrings(expression, expression)
exception ExpectedFunction(expression)
exception ArgumentMismatch(list<variable>, list<expression>)
exception VariableCouldNotBeSet(variable)

let rec evalProgram = (program: program, context: context): (expression, context) => {
  switch (program) {
    | list{statement, ...rest} => {
      switch evalStatement(statement, context) {
        | (Some(expression), context) => (expression, context)
        | (None, context) => evalProgram(rest, context)
      }
    }
    | list{} => (Constant(Unit), context)
  }
}
and evalStatement = (statement: statement, context: context): (option<expression>, context) => {
  switch (statement) {
    | Let(variable, expression) => (None, evalLet(variable, expression, context))
    | Expression(expression) => switch evalExpression(expression, context) {
      | (_, context) => (None, context)
    }
    | Return(expression) => switch evalExpression(expression, context) {
      | (expression, context) => (Some(expression), context)
    }
  }
}
and evalExpression = (expression: expression, context: context): (expression, context) => {
  switch (expression) {
    // Constants
    | Constant(_) => (expression, context)
    // Variables
    | Variable(v) => (evalVariable(v, context), context)
    // Logging
    | ConsoleLog(s, e) => {
      // Log the value of e
      let (e, context) = evalExpression(e, context)
      Js.log(s)
      Js.log(e)
      (e, context)
    }
    // Operators
    | Assign(v, e) => evalAssign(v, e, context)
    | Plus(a, b) => {
      let (a, context) = evalExpression(a, context)
      let (b, context) = evalExpression(b, context)
      switch (a, b) {
        | (Constant(Number(a)), Constant(Number(b))) => (Constant(Number(a +. b)), context)
        | (Constant(Text(a)), Constant(Text(b))) => (Constant(Text(a ++ b)), context)
        | _ => raise(ExpectedNumbersStrings(a, b))
      }
    }
    // Functions
    | Fn(_) => (expression, context)
    | Reactive(variables, statements) => evalExpression(Call(Fn(variables, statements), Belt.List.map(variables, variable => Variable(variable))), context)
    | Call(expression, parameters) => {
      let (caller, context) = evalExpression(expression, context);
      switch caller {
        | Fn(arguments, statements) => evalCall(arguments, statements, parameters, context)
        | _ => raise(ExpectedFunction(expression))
      }
    }
    // Elements
    | Element(tag, attributes, children) => evalElement(tag, attributes, children, context)
    | Fragment(children) => {
      let (children, context) = evalChildren(children, context)
      (Fragment(children), context)
    }
  }
}
and evalVariable = (variable: variable, context: context): expression => {
  switch Belt.Map.String.get(context.variables, variable) {
    | Some(expression) => expression
    | None => raise(VariableNotFound(variable))
  }
}
and evalLet = (variable: variable, expression: expression, context: context): context => {
  let (e, context) = evalExpression(expression, context)
  let newVariables = Belt.Map.String.set(context.variables, variable, e)
  // Return the new context
  {
    ...context,
    variables: newVariables
  }
}
and evalAssign = (variable: variable, expression: expression, context: context): (expression, context) => {
  switch Belt.Map.String.has(context.variables, variable) {
    | true => {
      let (e, context) = evalExpression(expression, context)
      let newVariables = Belt.Map.String.set(context.variables, variable, e)
      // Return the new context
      (e, {
        ...context,
        variables: newVariables
      })
    }
    | false => raise(VariableNotFound(variable))
  }
}
and evalCall = (arguments: list<variable>, statements: statements, parameters: list<expression>, context: context) => {
  let rec assignArgs = (arguments: list<variable>, parameters: list<expression>, context: context): context => {
    switch (arguments, parameters) {
      | (list{argument, ...otherArgs}, list{parameter, ...otherParams}) => {
        let context = evalLet(argument, parameter, context)
        assignArgs(otherArgs, otherParams, context)
      }
      | (list{}, list{}) => context
      | _ => raise(ArgumentMismatch(arguments, parameters))
    }
  }
  let context = assignArgs(arguments, parameters, context)
  evalProgram(statements, context)
}
and evalElement = (tag: expression, attributes: list<(attribute, expression)>, children: list<expression>, context: context): (expression, context) => {
  // Evaluate the tag
  let (tag, context) = evalExpression(tag, context)
  // Evaluate every attribute
  let (attributes, context) = evalAttributes(attributes, context)
  let (children, context) = evalChildren(children, context)
  (Element(tag, attributes, children), context)
}
and evalAttributes = (attributes: list<(attribute, expression)>, context: context): (list<(attribute, expression)>, context) => {
  switch (attributes) {
    | list{(attribute, value), ...rest} => {
      let (attribute, context) = switch attribute {
        | Attribute(e) => {
          let (e, context) = evalExpression(e, context)
          (Attribute(e), context)
        }
        | EventAttribute(e) => {
          let (e, context) = evalExpression(e, context)
          (EventAttribute(e), context)
        }
      }
      let (value, context) = evalExpression(value, context);
      let (newAttributes, context) = evalAttributes(rest, context)
      (list{(attribute, value), ...newAttributes}, context)
    }
    | list{} => (list{}, context)
  }
}
and evalChildren = (children: list<expression>, context: context): (list<expression>, context) => {
  switch (children) {
    | list{child, ...rest} => {
      let (child, context) = evalExpression(child, context)
      let (newChildren, context) = evalChildren(rest, context)
      (list{child, ...newChildren}, context)
    }
    | list{} => (list{}, context)
  }
}

let constantToString = (constant: constant): string => {
  switch (constant) {
    | Text(text) => text
    | Number(number) => Belt.Float.toString(number)
    | Bool(true) => "true"
    | Bool(false) => "false"
    | None | Unit => ""
  }
}

let rec expressionToHtml = (expression: expression): option<Html.html> => {
  switch expression {
    | Element(Constant(Text(tag)), attributes, children) => {
      let attributes: list<(string, string)> = Belt.List.keepMap(attributes, attribute => {
        switch attribute {
          | (Attribute(Constant(key)), Constant(value)) => Some((constantToString(key), constantToString(value)))
          | _ => None
        }
      })
      let children = Belt.List.keepMap(children, child => expressionToHtml(child))
      Some(Html.HtmlElement(tag, Belt.List.toArray(attributes), Belt.List.toArray(children)))
    }
    | Fragment(children) => expressionToHtml(Element(Constant(Text("span")), list{}, children))
    | Constant(constant) => Some(HtmlText(constantToString(constant)))
    | _ => None
  }
}