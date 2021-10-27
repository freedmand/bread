type app = (Html.html, Javascript.js)

let appToString: (app) => string = ((html, js)) => {
  switch (js) {
    | [] => Html.htmlToString(html)
    | js => `${Html.htmlToString(html)}\n<script>\n${Javascript.jsToString(js)}\n</script>`
  }
}