open Program;

// Test counter program

let count = getId()
let increment = getId()
let countParam = getId()

let program = list{
  Let(count, number(0.0)),
  Let(increment, Fn(list{countParam}, list{
    Expression(
      Assign(
        countParam,
        Plus(Variable(countParam), number(1.0))
      )
    ),
    Return(Variable(countParam))
  })),
  Expression(
    Assign(
      count,
      Call(
        Variable(increment),
        list{Variable(count)},
      )
    )
  ),
  Expression(Variable(count)),
  Return(
    Element(
      text("div"),
      list{},
      list{
        Element(
          text("div"),
          list{},
          list{
            text("Count:"),
            Reactive(list{count}, list{Return(Variable(count))})
          }
        ),
        Element(
          text("div"),
          list{},
          list{
            Element(
              text("button"),
              list{(EventAttribute(text("click")), Variable(increment))},
              list{text("+")}
            ),
         }
        ),
      }
    )
  )
}

let (rendered, context) = evalProgram(program, freshContext)

switch expressionToHtml(rendered) {
  | Some(html) => {
    // Set document body to the HTML
    let html = Html.htmlToString(html)
    let setBodyHtml: (string => unit) = %raw(`
      function(html) {
        document.body.outerHTML = html;
      }
    `)
    setBodyHtml(html)
  }
  | None => ()
}
