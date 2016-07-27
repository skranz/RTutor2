
examples.make.ps.html = function() {
  setwd("D:/lehre/makro")
  outdir = paste0(getwd(),"/figures")
  
  ps.name = "makro101"
  
  rmd.file = paste0(ps.name,".rmd")
  rps.file = paste0(ps.name,".rps")
  html.file = paste0(ps.name,".html")
  
  
  #ps = fetch.ps(rmd.file = rmd.file)  
  ps = fetch.ps(rps.file = rps.file)  

  tl = offline.slide.ps.ui(ps=ps)
  create.offline.html(tl, outfile=paste0(ps.name,"_offline.html"),use.button = FALSE)
    
  tl = offline.print.slide.ps.ui(ps=ps)
  create.offline.html(tl, outfile=paste0(ps.name,"_offline_print.html"),use.button = FALSE)
  

}

create.offline.slides = function(rmd.file, ps.name = tools::file_path_sans_ext(basename(rmd.file))) {
  ps = fetch.ps(rmd.file = rmd.file)  

  tl = offline.slide.ps.ui(ps=ps)
  create.offline.html(tl, outfile=paste0(ps.name,"_offline.html"),use.button = FALSE)
    
  tl = offline.print.slide.ps.ui(ps=ps)
  create.offline.html(tl, outfile=paste0(ps.name,"_offline_print.html"),use.button = FALSE)
  
}

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

create.offline.html = function(tagList=NULL, outfile, head=NULL, body=NULL, use.button = FALSE, launch.browser=TRUE, browse=TRUE) {
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
    getApp()$session$sendCustomMessage(type = "closeWindow", message = "message")
    stopApp(invisible(html))
    
  })
  html = viewApp(app,launch.browser = launch.browser)
  if (!is.null(outfile)) {
    writeUtf8(merge.lines(html), outfile)
    if (browse)
      browseURL(outfile)

  }

  restore.point("save.as.offline.html.res")
  
  invisible(html)
  
}


offline.slide.ps.ui = function(ps = NULL, rps.file=NULL, rmd.file=NULL) {
  restore.point("offline.slide.ps.ui")
  
  ps = fetch.ps(ps=ps, rps.file=rps.file, rmd.file = rmd.file)  
  
  css = if (!is.null(ps$css)) tags$head(tags$style(ps$css)) else NULL
  head = if (!is.null(ps$head)) tags$head(HTML(ps$head)) else NULL
  content.ui = ps$bdf$ui[[1]]
  mcss = tags$head(tags$style(math.css()))
  
  
  slide.ids = ps$bdf$div.id[ps$slide.bis]
  start.js = paste0('
    rtNumSlides = ', ps$num.slides,';
    rtSlideIds = [', paste0('"',slide.ids,'"', collapse=","),'];
    rtShowSlide(1);
    '  
  )
  
  ui = bootstrapPage(
    
    tags$head(HTML("\n<!-- MyHeadStart -->\n")),
    tags$head(includeScript(paste0(path.package("shiny"),"/www/shared/jquery.min.js"))),
    tags$head(includeCSS(paste0(path.package("shiny"),"/www/shared/bootstrap/css/bootstrap.css"))),
    tags$head(includeScript(paste0(path.package("shiny"),"/www/shared/bootstrap/js/bootstrap.min.js"))),
    head,
    css,
    mcss,
    #tags$style("table { max-width: 100%;}"),
    div(id="maindiv",
      content.ui
    ),
    includeScript(paste0(path.package("RTutor2"),"/www/offline_slides.js")),
    tags$script(
      HTML(start.js)   
    ),
    tags$script(class="remove_me",
      '
      ')
  )
  ui
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
    tags$head(includeCSS(paste0(path.package("shiny"),"/www/shared/bootstrap/css/bootstrap.css"))),
    tags$style(HTML(slides.print.screen.css())),
    
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


slides.print.screen.css = function() {
  '
  body {
    margin-left: 10%;
    margin-right: 10%;
  }
  '
  
}


offline.reveal.slide.ps.ui = function(ps = NULL, rps.file=NULL, rmd.file=NULL) {
  restore.point("offline.reveal.slide.ps.ui")
  
  ps = fetch.ps(ps=ps, rps.file=rps.file, rmd.file = rmd.file)  
  
  css = if (!is.null(ps$css)) tags$head(tags$style(ps$css)) else NULL
  head = if (!is.null(ps$head)) tags$head(HTML(ps$head)) else NULL
  content.ui = ps$bdf$ui[[1]]
  mcss = tags$head(tags$style(math.css()))
  
  
  
  
  slides.uis = lapply(ps$slide.bis, function(bi) {
    tags$section(ps$bdf$ui[bi])
  })
  
  content = tags$div(class="reveal", div(class="slides",slides.uis))

  slides.ids = ps$bdf$div.id[ps$slide.bis]
  slides.sel = paste0("'#", slides.ids,"'", collapse=", ")
  
  start.js = paste0('
    Reveal.initialize(); 
    $(', slides.sel,').css("display","block");
 
  ')
  
  ui = bootstrapPage(
    
    tags$head(HTML("\n<!-- MyHeadStart -->\n")),
    tags$head(includeScript(paste0(path.package("shiny"),"/www/shared/jquery.min.js"))),
    tags$head(includeCSS(paste0(path.package("RTutor2"),"/www/reveal/css/reveal.css"))),  
    #tags$head(includeCSS(paste0(path.package("shiny"),"/www/shared/bootstrap/css/bootstrap.css"))),
    tags$head(includeScript(paste0(path.package("RTutor2"),"/www/reveal/js/reveal.js"))),    
  
    head,
    css,
    mcss,
    
    content,
    #includeScript(paste0(path.package("RTutor2"),"/www/offline_slides.js")),
    tags$script(
      HTML(start.js)   
    ),
    tags$script(class="remove_me",
      '
      ')
  )
  ui
}
