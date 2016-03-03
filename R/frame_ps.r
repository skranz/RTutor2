# To DO: More effective click handler in shinyEvents: register only a single javascript handler... dispatch in R to the corresponding function.

examples.frame.ps = function() {
  library(EconCurves)
  setwd("D:/libraries/RTutor2")
  txt = readLines("ex1.rmd", warn=FALSE)
  #txt = readLines("test.rmd", warn=FALSE)
  frame.ind = NULL
  
  static.types = "frame"
  static.types = NULL
  slides = !TRUE
  ps = rtutor.make.frame.ps.te(txt, bdf.filter=bdf.frame.filter(frame.ind=frame.ind), catch.errors=FALSE, static.types=static.types, slides=slides)
  bdf = ps$bdf
  #app = slidesApp(ps)
  app = rtutorApp(ps)
  viewApp(app)
}

# Init ps for a new session
init.ps.session = function(ps, app=getApp(), rendered=FALSE, hidden=FALSE) {
  restore.point("init.ps.session")
  
  # make shallow copy of ps
  ps = as.environment(as.list(ps))
  
  # init user state of chunks
  init.ps.user.objects(ps)
  # container state
  if (is.null(ps$cont.state)) {
    n = NROW(ps$bdf)
    ps$cont.state = data_frame(
      rendered = rep(rendered,length.out=n),
      hidden = rep(hidden,length.out=n)
    )
  }

  ps
}

# general initialisation independent of app type
initRTutorApp = function(ps, catch.errors = TRUE, offline=FALSE, use.mathjax = !offline) {
  restore.point("initRTutorApp")
  library(shinyjs)
  
  app = eventsApp()
  
  
  app$ps = ps
  ps$offline = offline
  ps$use.mathjax = use.mathjax
  
  set.ps(ps)
  opts = default.ps.opts(catch.errors=catch.errors)
  set.ps.opts(opts=opts)

  bdf = ps$bdf
    
  try(shiny::addResourcePath("figure",paste0(dir,"/figure")), silent=TRUE)  

  app
}


slidesApp = function(ps, start.slide=1, dir=getwd(), offline=FALSE, just.return.html=FALSE, catch.errors = TRUE, margin=2) {
  restore.point("slidesApp")
  
  app = initRTutorApp(ps=ps, catch.errors = catch.errors,offline = offline)
  
  ps$slide.ind = start.slide
  ps.content.ui = ps$bdf$ui[[1]]   
   
  resTags = rtutor.html.ressources()
  app$ui = tagList(
    useShinyjs(),
    resTags,
    rtutorClickHandler(),
    fluidPage(
      fluidRow(
        column(width=12-2*margin, offset=margin,
          withMathJax(ps.content.ui)
        )
      )
    )
  )
  add.slide.navigate.handlers()
  
  # Each time the problem set is restarted
  # reinit the problem set
  appInitHandler(app=app,function(app,...) {
    ps = init.ps.session(ps=ps,app=app)
    ps$slide.ind = start.slide
    app$ps = ps 
    init.ps.handlers(ps)
    set.slide(ps=ps)
  })

  
  app
}

rtutorApp = function(ps, dir=getwd(), offline=FALSE, just.return.html=FALSE, catch.errors = TRUE, margin=2,...) {
  restore.point("rtutorApp")
  
  if (isTRUE(ps$slides)) {
    return(slidesApp(ps=ps,dir=dir, offline=offline, catch.errors=catch.errors, margin=margin,...))
  }
  
  app = initRTutorApp(ps=ps, catch.errors = catch.errors,offline = offline)
  
  
  
  ps.content.ui = ps$bdf$ui[[1]]
  n = NROW(ps$bdf)
  
  resTags = rtutor.html.ressources()
  app$ui = tagList(
    useShinyjs(),
    resTags,
    rtutorClickHandler(),
    fluidPage(
      fluidRow(
        column(width=12-2*margin, offset=margin,
          withMathJax(ps.content.ui)
        )
      )
    )
  )
  # Each time the problem set is restarted
  # reinit the problem set
  appInitHandler(app=app,function(app,...) {
    ps = init.ps.session(ps=ps,app=app)
    app$ps = ps 
    init.ps.handlers(ps)
    render.container.descendants(ps=ps,type.ind=1, use.mathjax=ps$use.mathjax, skip.if.rendered=FALSE)
  })
  
  
  app
}

init.ps.handlers = function(ps) {
  restore.point("init.ps.handler")
  
  # Add handlers for task chunks
  for (uk in ps$uk.li) {
    make.chunk.handlers(uk)
  }
  
  # Add handlers for addons
  rows = which(ps$bdf$is.addon) 
  for (bi in rows) {
    type = ps$bdf$type[[bi]]
    ao = ps$bdf$obj[[bi]]$ao
    # TO DO: Distinguish between global handlers
    # initialization and per user initilization
    ps$Addons[[type]]$shiny.init.handlers.fun(ao)
  }
  
}

is.cont.rendered = function(bi, ps) {
  restore.point("is.cont.rendered")
  
  if (is.null(ps$cont.state)) return(FALSE)
  isTRUE(ps$cont.state$rendered[[bi]])
}


render.as.slide = function(ps, type=ps$slide.type, type.ind=1, bi=NULL, slide.ind=type.ind, output.id=NULL, ...) {
  restore.point("render.as.slide")
  if (is.null(bi))
    bi = get.bdf.ind(type.ind=type.ind,type = type,bdf = bdf)
  
  render.container(ps=ps,type=type,type.ind=type.ind, bi=bi,output.id=output.id)
}

# not actively used anymore
as.slide.ui.fun = function(ui, bi,ps,...) {
  return(ui)
  
  header.ui = slide.title.bar.ui(title=title, slide.ind=slide.ind, num.slides=ps$num.slides)    
  tagList(div(
    id = paste0(ps$bdf$id[[bi]],"_rtutor_slide_div"),
    class="rtutor-slide-div",
    header.ui,
    ui
  ))
  
}

get.ps.uk = function(ps, bi=NULL, stype.ind=NULL, chunk.ind=stype.ind) {
  if (is.null(chunk.ind)) {
    chunk.ind = ps$bdf$stype.ind[[bi]]    
  }
  ps$uk.li[[chunk.ind]]
}

render.rtutor.task.chunk = function(ps, bi) {
  restore.point("render.rtutor.task.chunk")
  
  uk = get.ps.uk(ps,bi=bi)
  update.chunk.ui(uk)
}

render.rtutor.addon = function(ps, bi) {
  cat("Render add on not yet implemented.")
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

add.slide.navigate.handlers = function() {
  buttonHandler("rtPrevBtn",slide.prev)
  buttonHandler("rtNextBtn",slide.next)
  buttonHandler("rtForwardBtn",slide.forward)
  changeHandler("doc_click",slide.click)
}


slide.click = function(value,ps=app$ps, app=getApp(),...) {
  restore.point("slide.click")

  is.left = value$pageX < 100 #& value$pageX <= value$width * 0.125 
  
  if (is.left) {
    slide.prev(ps=ps,app=app,...)
  } else {
    slide.next(ps=ps,app=app,...)
  }
}


slide.prev = function(ps=app$ps, app=getApp(),...) {
  restore.point("slide.prev")
  if (ps$slide.ind <= 1) return()
  set.slide(ps$slide.ind-1)
}

slide.next = function(ps=app$ps, app=getApp(),...) {
  restore.point("proceed.with.frame")
  if (ps$slide.ind >= ps$num.slides) return()
  set.slide(ps$slide.ind+1)
}

slide.forward = function(ps=app$ps, app=getApp(),...) {
  slide.next(ps=ps,app=app,...)
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

set.slide = function(slide.ind = ps$slide.ind, ps=app$ps,app=getApp(),use.mathjax = isTRUE(ps$use.mathjax),...) {
  restore.point("set.slide")
  
  ps$old.slide.ind = ps$slide.ind
  ps$old.slide.bi = ps$slide.bi
  
  # Don't do anything if the current slide is already set
  # But, we need to deal with pauses in slides
  if (identical(ps$old.slide, slide.ind))
    return()
  
  ps$slide.ind = slide.ind
  bdf = ps$bdf
  
  bi = which(bdf$type==ps$slide.type)[ps$slide.ind]
  ps$slide.bi = bi
  br = bdf[bi,]
  
  is.rendered = ps$cont.state$rendered[bi]
  if (!is.null(ps$old.slide.bi)) {
    hide.container(ps,bi=ps$old.slide.bi)
  }
  if (is.rendered) {
    show.container(ps=ps,bi=bi)  
  } else {
    render.as.slide(ps,type=ps$slide.type,type.ind=slide.ind, bi=bi)
  }
}


