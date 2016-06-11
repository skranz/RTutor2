var mjAPI = require("mathjax-node/lib/mj-page.js");
var jsdom = require("jsdom").jsdom;
 
var document = jsdom("<!DOCTYPE html><html lang='en'><head><title>Test</title></head><body><h1>Let's test mj-page</h1> <p> \\[f: X \\to Y\\], where \\( X = 2^{\mathbb{N}}\\) </p></body></html>");
 
mjAPI.start();
 
mjAPI.typeset({
  html: document.body.innerHTML,
  renderer: "NativeMML",
  inputs: ["TeX"],
  xmlns: "mml"
}, function(result) {
  "use strict";
  document.body.innerHTML = result.html;
  var HTML = "<!DOCTYPE html>\n" + document.documentElement.outerHTML.replace(/^(\n|\s)*/, "");
  console.log(HTML);
});