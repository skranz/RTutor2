fetch.ps = function(ps = NULL, rps.file=NULL, rmd.file=NULL, txt=NULL, ps.name=NULL) {

  if (!is.null(rmd.file)) {
    txt = readLines(rmd.file)
    if (is.null(ps.name)) {
      ps.name = tools::file_path_sans_ext(basename(rmd.file))
    }
  }
  if (!is.null(txt)) {
    if (is.null(ps.name)) ps.name = "ps"
    ps = rtutor.make.frame.ps(txt=txt, ps.name=ps.name)
  } else if (!is.null(rps.file)) {
    ps = read.rps(rps.file)
  }
  ps
}

examples.make.ps.html = function() {
  setwd("D:/lehre/makro")
  outdir = paste0(getwd(),"/figures")
  
  ps.name = "makro101"
  
  rmd.file = paste0(ps.name,".rmd")
  rps.file = paste0(ps.name,".rps")
  html.file = paste0(ps.name,".html")
  
  #rps.file = "makro101.rps"
  #ps = fetch.ps(rmd.file = rmd.file)  
  ps = fetch.ps(rps.file = rps.file)  
  html = make.ps.html(ps=ps)
  writeLines(html, html.file)
  
  view.html(text=html)
  
}



make.ps.html = function(ps = NULL, rps.file=NULL, rmd.file=NULL) {
  restore.point("make.ps.html")
  
  ps = fetch.ps(ps=ps, rps.file=rps.file, rmd.file = rmd.file)  
  
  css = if (!is.null(ps$css)) tags$head(tags$style(ps$css)) else NULL
  head = if (!is.null(ps$head)) tags$head(HTML(ps$head)) else NULL
  content.ui = ps$bdf$ui[[1]]
  mcss = tags$head(tags$style(math.css()))
  
  ui = bootstrapPage(
    head,
    css,
    mcss,
    content.ui,
    tags$script('
$("div").css("display","block")      
    ')
  )
 
  rendered = htmltools::renderTags(ui)
  html = simple.html.page(head=rendered$head, body = rendered$html)
  #html = rendered$html
  html = merge.lines(html)
  html = unicode.html.math(html)
  html = gsub("display: none","display: block", html, fixed=TRUE)
  html
}

shiny.ui.to.html.document = function(ui) {
  rendered = htmltools::renderTags(ui)
  html = simple.html.page(head=rendered$head, body = rendered$html)
  html  
}  

offline.mathjax = function() {

}
