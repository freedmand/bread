type rec html =
  | HtmlElement(string, array<(string, string)>, array<html>)
  | HtmlText(string)

let rec htmlToString: (html) => string = (html) => {
  let attributesToString = attributes => Js.Array.joinWith(" ", Js.Array.map(((key, value)) => `${key}="${value}"`, attributes)) // TODO: escape quoted value
  let childrenToString = children => Js.Array.joinWith("", Js.Array.map(child => htmlToString(child), children))

  switch (html) {
    | HtmlElement(tag, [], []) => `<${tag}/>`
    | HtmlElement(tag, [], children) => `<${tag}>${childrenToString(children)}</${tag}>`
    | HtmlElement(tag, attributes, []) => `<${tag} ${attributesToString(attributes)}/>`
    | HtmlElement(tag, attributes, children) => `<${tag} ${attributesToString(attributes)}>${childrenToString(children)}</${tag}>`
    | HtmlText(text) => text  // TODO: escape HTML
  }
}

// let testHtml = HtmlElement("div", [("id", "1")], [
//   HtmlContent(
//     HtmlContent(
//       HtmlContent(
//         HtmlContent(
//           HtmlContent(
//             HtmlElement("button", [], [HtmlText("+")])
//           )
//         )
//       )
//     )
//   )
// ])