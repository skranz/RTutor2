# To DO: More effective click handler in shinyEvents: register only a single javascript handler... dispatch in R to the corresponding function.

examples.frame.ps = function() {
  library(EconCurves)
  setwd("D:/libraries/RTutor2")
  txt = readLines("ex1.rmd", warn=FALSE)
  frame.ind = NULL
  te = rtutor.make.frame.ps.te(txt, bdf.filter=bdf.frame.filter(frame.ind=frame.ind), catch.errors=FALSE)
  te$lang = "de"
  bdf = te$bdf
  show.frame.ps(te, catch.errors=FALSE)
}

show.frame.ps = function(ps, frame.ind=1, dir=getwd(), offline=FALSE, just.return.html=FALSE, catch.errors = TRUE) {
  restore.point("show.frame.ps")
  
  app = eventsApp()
  app$ps = ps
  
  set.ps(ps)
  opts = default.ps.opts(catch.errors=catch.errors)
  set.ps.opts(opts=opts)
  init.ps.user.objects(ps)
    
  resTags = rtutor.html.ressources()
  app$ui = tagList(
    resTags,
    #buttonHandlerScript(),
    #docClickEvents(),    
    rtutorClickHandler(),
    #swipeEvents(),
    fluidPage(
      withMathJax(uiOutput("frameUI"))
    )
  )
  #setUI("tchunk_2__chunkUI",HTML("Ich werde gesehen!"))
  try(shiny::addResourcePath("figure",paste0(dir,"/figure")), silent=TRUE)  

  bdf = ps$bdf
  
  ps$offline = offline
  ps$use.mathjax = !offline
  ps$show.frames = NULL
  ps$num.frames = sum(bdf$type=="frame")
  add.navigate.handlers()
  frame.ui = set.frame(frame.ind,ps=ps)
  
  
  # May be useful for debugging HTML problems
  if (just.return.html) {
    html = tagList(
      resTags,
      docClickEvents(id="doc_click"),
      fluidPage(
        frame.ui
      )
    )
    return(html)
  }
  
  viewApp(app)
}

init.ps.user.objects = function(ps) {
  ps$uk.li = lapply(ps$org.uk.li, init.user.chunk)
}

show.dyn.ui = function(bi,ps=NULL) {
  restore.point("show.dyn.ui")
  
  # TO DO: store whether UI really needs an update...
  bdf = ps$bdf
  stype = bdf$stype[[bi]]
  if (stype=="task_chunk") {
    uk = ps$uk.li[[ bdf$stype.ind[[bi]] ]]
    
    if (!isTRUE(uk$handlers.initialized)) {
      uk$handlers.initialized = TRUE
      make.chunk.handlers(uk)
    }
    update.chunk.ui(uk=uk)
  }
}

rtutor.navigate.btns = function() {
  btns = tagList(
    bsButton("rtPrevBtn","<",size = "extra-small"),
    bsButton("rtNextBtn",">",size = "extra-small"),
    bsButton("rtForwardBtn",">>",size = "extra-small")
  )
  btns
}

add.navigate.handlers = function() {
  buttonHandler("rtPrevBtn",frame.prev)
  buttonHandler("rtNextBtn",frame.next)
  buttonHandler("rtForwardBtn",frame.forward)
  changeHandler("doc_click",frame.click)
  swipeLeftHandler(fun=frame.prev)
  swipeRightHandler(fun=frame.prev)
  
}


frame.click = function(value,ps=app$ps, app=getApp(),...) {
  restore.point("frame.click")

  is.left = value$pageX < 100 #& value$pageX <= value$width * 0.125 
  
  if (is.left) {
    frame.prev(ps=ps,app=app,...)
  } else {
    frame.next(ps=ps,app=app,...)
  }
}


frame.prev = function(ps=app$ps, app=getApp(),...) {
  restore.point("frame.prev")
  if (ps$frame.ind <= 1) return()
  set.frame(ps$frame.ind-1)
}

frame.next = function(ps=app$ps, app=getApp(),...) {
  restore.point("proceed.with.frame")
  if (ps$frame.ind >= ps$num.frames) return()
  set.frame(ps$frame.ind+1)
}

frame.forward = function(ps=app$ps, app=getApp(),...) {
  frame.next(ps=ps,app=app,...)
}

# TO DO: improve code
rtutor.init.addons = function(addons,ps) {
  restore.point("rtutor.init.addons")
  
  for (ao in addons) {
    #rta = ao$rta
    #Ao = ps$Addons[[rta$type]]
    #Ao$shiny.init.fun(ao)
    add.quiz.handlers(qu=ao, quiz.handler=NULL) 
  }  
}

set.frame = function(frame.ind = ps$frame.ind,ps=app$ps, app=getApp(),use.mathjax = isTRUE(ps$use.mathjax),...) {
  restore.point("set.frame")
  
  ps$frame.ind = frame.ind
  bdf = ps$bdf
  bi = which(bdf$type=="frame")[ps$frame.ind]
  
  # init addons
  # TO DO: improve code  
  ao.ind = which(bdf$parent_frame == bi & bdf$is.addon)
  addons = lapply(ao.ind, function(ind) bdf$obj[[ind]]$ao)
  if (!frame.ind %in% ps$shown.frames) {
    rtutor.init.addons(addons,ps)
    ps$shown.frames = c(ps$shown.frames,frame.ind) 
  }

  # show dynamic ui of the frame
  #dyn.ui.ind = which(bdf$parent_frame == bi & bdf$has.dyn.ui)
  #for (cbi in dyn.ui.ind) {
  #  show.dyn.ui(bi = cbi, ps=ps)
  #}
  
  
  
  obj = ps$bdf$obj[[bi]]

  header.ui = frame.title.bar.ui(title=obj$title, frame.ind=frame.ind, num.frames=ps$num.frames)
  
  ui = bdf$ui[bi]
  frame.ui = tagList(header.ui,ui)
  frame.ui = withMathJax(frame.ui)
  
  setUI("frameUI",frame.ui)
  invisible(frame.ui)
}


frame.title.bar.ui = function(title, frame.ind, num.frames) {
 tagList(
    HTML("<table width='100%'><tr><td>"),
    h4(title),
    HTML("</td><td align='right' valign='top' nowrap>"),
    HTML("<table><tr><td valign='center' nowrap>"),    
    rtutor.navigate.btns(),
    HTML("</td><td valign='center' nowrap style='padding-left: 5px'>"),
    HTML(paste0(frame.ind, " of ",num.frames)),      
    HTML("</td></tr></table>"),
    HTML("</td></tr></table>")
  )  
}

