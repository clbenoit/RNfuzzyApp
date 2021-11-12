// Wait for page load
window.addEventListener("load", function () {
  // Set normal title without HTML
  var title = document.getElementsByTagName("title")[0];
  title.innerText = "RNFuzzyApp : a RNA-seq Analysis App";
  // Set favicon
  var head = document.getElementsByTagName("head")[0];
  var favicon = document.createElement("link");
  favicon.rel = "shortcut icon";
  favicon.type = "image/svg+xml";
  favicon.href = "react.svg";
  head.appendChild(favicon);
  // Set good size for file button
  var btn = document.querySelector("span.btn.btn-default.btn-file");
  btn.style.padding = "9px 16px";
  btn.style.marginBottom = "4px";
});
