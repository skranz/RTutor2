with.mathjax = function (..., config="TeX-AMS_HTML") 
{
    path <- paste0("https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=",config)
    
    tagList(
      tags$head(
        singleton(tags$script(src = path, class="mathjax_load", type = "text/javascript"))
      ),
        ..., tags$script(class="mathjax_typeset",HTML("MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);")))
}

mathjax.to.offline = function(container.id=NULL, use.button=TRUE) {
  
  inner.target = if (is.null(container.id)) {
    'document.documentElement'    
  } else {
    paste0('document.getElementById("',container.id,'")')
  }
  
  js = paste0('
  Shiny.addCustomMessageHandler("closeWindow", function(m) {window.close();});

  function utf8_to_b64( str ) {
    return window.btoa(unescape(encodeURIComponent( str )));
  }
  function remove_mathjax_and_save() {
    $(".mathjax_load").remove();
    $(".mathjax_typeset").remove();
    $(".MJX_Assistive_MathML").remove();
    $(".remove_me").remove();
    $("#saveOfflineDiv").remove();
    Shiny.onInputChange("downloadHtmlPage", {id: "downloadHtmlPage", html: ', inner.target,'.innerHTML});
  }
  ')
  
  if (use.button) {
    js = paste0(js,'
$("#saveOfflineBtn").click(function(e) {
  remove_mathjax_and_save();
});'      
    )
    btn = actionButton("saveOfflineBtn", "Save as offline html")
  } else {
    js = paste0(js,'
MathJax.Hub.Queue(function () {
  remove_mathjax_and_save();
 
});'
    )
    btn = NULL
  }
  
  tagList(
    div(id="saveOfflineDiv",
      btn,
      tags$script(js)
    )
    
  )
  
}