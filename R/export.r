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

create.offline.html = function(tagList=NULL, outfile, head=NULL, body=NULL, use.button = FALSE, launch.browser=TRUE) {
  restore.point("save.as.offline.html")
  
  if (is.null(head) & is.null(body)) {
    rendered = htmltools::renderTags(tagList)
    head = rendered$head
    body = rendered$html
  }

  app = eventsApp(adapt.ui = FALSE)
  app$ui = tagList(
    tags$head(head),
    mathjax.to.offline(container.id = NULL, use.button=use.button),
    with.mathjax(body)
  )
  eventHandler(eventId="downloadHtmlPage", id=NULL, fun=function(...) {
    args = list(...)
    restore.point("downloadHtmlPage")
    html = args$html
    Encoding(html) = "UTF-8"
    html = sep.lines(html)
    start.line = which(html == "<!-- MyHeadStart -->")[1]
    html = html[start.line:length(html)]
    html = c(
'<!DOCTYPE html>

<head>

<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
',
      html)
    stopApp(invisible(html))
    
  })
  html = viewApp(app,launch.browser = launch.browser)
  if (!is.null(outfile)) {
    writeUtf8(merge.lines(html), outfile)
  }

  restore.point("save.as.offline.html.res")
  
  invisible(html)
  
}

examples.make.ps.html = function() {
  setwd("D:/lehre/makro")
  outdir = paste0(getwd(),"/figures")
  
  ps.name = "makro101"
  
  rmd.file = paste0(ps.name,".rmd")
  rps.file = paste0(ps.name,".rps")
  html.file = paste0(ps.name,".html")
  
  
  #ps = fetch.ps(rmd.file = rmd.file)  
  ps = fetch.ps(rps.file = rps.file)  
  
  tl = offline.print.slide.ps.ui(ps=ps)
  create.offline.html(tl, outfile=paste0(ps.name,"_offline.html"),use.button = FALSE)
  

  dir = "D:/programs/node_modules"
  file = paste0(dir,"/", html.file)
  latin.file = paste0(dir,"/latin_", html.file)
  out.file = paste0(dir,"/out.html")

  writeLines(html, html.file)
  writeUtf8(html, file)
  writeLines(html, latin.file)

  txt = rmdtools:::readUtf8(out.file)
  writeLines(txt,"out.html")
  
  
  view.html(txt=txt)
  
  view.html(text=txt)
  
  writeLines(html, html.file)

  rmdtools:::writeUtf8(html, file)
  writeLines(html, file)
  
  str = "hü"
  view.html(text=html)

  txt = rmdtools:::readUtf8(file)
    
}



offline.print.slide.ps.ui = function(ps = NULL, rps.file=NULL, rmd.file=NULL) {
  restore.point("offline.print.slide.ps.ui")
  
  ps = fetch.ps(ps=ps, rps.file=rps.file, rmd.file = rmd.file)  
  
  css = if (!is.null(ps$css)) tags$head(tags$style(ps$css)) else NULL
  head = if (!is.null(ps$head)) tags$head(HTML(ps$head)) else NULL
  content.ui = ps$bdf$ui[[1]]
  
  content = htmltools::renderTags(content.ui)$html
  
  #content = unicode.html.math(content)
  
  mcss = tags$head(tags$style(math.css()))
  
  ui = bootstrapPage(
    tags$head(HTML("\n<!-- MyHeadStart -->\n")),
    tags$head(includeScript(paste0(path.package("shiny"),"/www/shared/jquery.min.js"))),
    head,
    css,
    mcss,
    HTML(content),
    tags$script(class="remove_me",
      '
$("div").css("display","block");
$(".nav_buttons_div").remove();
      ')
  )
  ui
}

shiny.ui.to.html.document = function(ui) {
  rendered = htmltools::renderTags(ui)
  html = simple.html.page(head=rendered$head, body = rendered$html)
  html  
}  

offline.mathjax = function() {

}
