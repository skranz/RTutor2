# To DO: More effective click handler in shinyEvents: register only a single javascript handler... dispatch in R to the corresponding function.

examples.frame.ps = function() {
  setwd("D:/libraries/RTutor2")
  txt = readLines("ex1.rmd", warn=FALSE)
  #txt = readLines("test.rmd", warn=FALSE)
  ps = rtutor.make.frame.ps(txt, catch.errors=FALSE)
  bdf = ps$bdf
  
  app = rtutorApp(ps,catch.errors = FALSE)
  viewApp(app)
}

# Init ps for a new session
init.ps.session = function(ps, user.name, nick=user.name, app=getApp(), rendered=FALSE, hidden=FALSE) {
  
  # make shallow copy of ps
  ps = as.environment(as.list(ps))
  set.ps(ps)

  ups = load.ups(user.name=user.name, nick=user.name)
  restore.point("init.ps.session")
  
  # init user state of chunks
  init.ps.session.task.states(ps=ps, ups=ups)
  # container state
  
  if (is.null(ps$cont.state)) {
    n = NROW(ps$bdf)
  
    rendered = rep(rendered,length.out=n)
    hidden = rep(hidden,length.out=n)
    hidden[ps$bdf$type %in% ps$hidden.container.types] = TRUE
    ps$cont.state = data_frame(
      rendered = rendered,
      hidden = TRUE
    )
  }

  ps$plugin.states = vector("list", length(ps$plugins))
  ps$active.plugin = ps$plugins[1]
  for (plugin in ps$plugins) {
    call.plugin.handler("init.handler",plugin=plugin)
  }
  call.plugin.handler("activate.handler",plugin=ps$active.plugin)
  
  
  ps
}

# general initialisation independent of app type
initRTutorApp = function(ps, catch.errors = TRUE, offline=FALSE, use.mathjax = !offline, opts=list(), dir=getwd(), figure.dir = paste0(dir,"/", ps$figure.sub.dir), ups.dir=dir,...) {
  restore.point("initRTutorApp")
  library(shinyjs)
  
  app = eventsApp()
  setAppHasBottomScript(TRUE, app=app)

  if (!is.null(ps$figure.web.dir))
    addResourcePath(ps$figure.web.dir,figure.dir)

  ps$opts[names(opts)] = opts
  
  app$ps = ps
  ps$dir = dir
  ps$ups.dir = ups.dir
  ps$offline = offline
  ps$use.mathjax = use.mathjax
  ps$is.shiny = TRUE
  ps$opts$is.shiny=TRUE
  ps$opts$catch.errors = catch.errors
  
  ps$given.awards.bi = NULL
  set.rt.opts(ps$opts)
  
  app$ps = ps

  bdf = ps$bdf
    
  try(shiny::addResourcePath("figure",paste0(dir,"/figure")), silent=TRUE)  
  nestedSelectorHandler("rtNavbarSelector",fun = select.ps.part.handler)

  app
}


slidesApp = function(ps,user.name = "John Doe", nick=user.name, start.slide=first.non.null(ps$start.slide,1), dir=getwd(), ups.dir=dir, offline=FALSE, just.return.html=FALSE, catch.errors = TRUE, margin=2, opts=list()) {
  restore.point("slidesApp")
  
  app = initRTutorApp(ps=ps, catch.errors = catch.errors,offline = offline, dir=dir, ups.dir=ups.dir, opts=opts)
  
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
    ps = init.ps.session(ps=ps,user.name=user.name, nick=nick,app=app)
    ps$slide.ind = start.slide
    app$ps = ps 
    init.ps.handlers(ps)
    set.slide(ps=ps)
  })

  
  app
}

rtutorApp = function(ps, user.name = "John Doe", nick=user.name, dir=getwd(), ups.dir=dir, offline=FALSE, just.return.html=FALSE, catch.errors = TRUE, margin=2,opts=list(),...) {
  restore.point("rtutorApp")
  
  if (isTRUE(ps$slides)) {
    return(slidesApp(ps=ps,user.name=user.name, nick=nick,dir=dir, offline=offline, catch.errors=catch.errors, margin=margin,opts=opts, ups.dir=ups.dir,...))
  }
  
  app = initRTutorApp(ps=ps, catch.errors = catch.errors,offline = offline,dir=dir, ups.dir=ups.dir, opts=opts,...)
  
  
  
  ps.content.ui = ps$bdf$ui[[1]]
  n = NROW(ps$bdf)
  
  resTags = rtutor.html.ressources()

    json.opts =
'
defaults: {
  //spacing_open: 4
},
north: {
  size: "auto",
  resizable: false,
  //spacing_open: 4,
  spacing_closed: 10
},
east: {
  closable: true,
  resizable: true
}
'
  style = tags$style(HTML('
.ui-layout-east {
	background:	#FFF;
	border:		none;
	padding:	0px;
	overflow:	auto;
}


.ui-layout-north {
	background:	#FFF;
	border:		none;
	padding:	0px;
	overflow:	auto;
}

.ui-layout-center {
	background:	#FFF;
	border:		none;
	padding:	2px;
	overflow:	auto;
}


'
  ))

    json.opts =
'
defaults: {
  //spacing_open: 4
},
north: {
  size: "auto",
  resizable: false,
  //spacing_open: 4,
  spacing_closed: 10
},
east: {
  closable: true,
  resizable: true
}
'

  app$ui = tagList(
    useShinyjs(),
    resTags,
    rtutorClickHandler(),
    bootstrapPage(
    jqueryLayoutPage(style=style,
      north = div(
        style="margin-left: 2px; margin-right: 2px;",
        ps$navbar.ui
      ),
      center = div(
        #style="margin-left: 10%; margin-right: 10%; overflow: auto; height: 100%;",
        style="margin-left: 10%; margin-right: 10%;",
        withMathJax(ps.content.ui)
      ),
      east = div(
        sidebar.ui()
      )
    ))
  ) 
  

  

  # Each time the problem set is restarted
  # reinit the problem set
  appInitHandler(app=app,function(app,...) {
    restore.point("rtApp.appInitHandler")
    cat("app =",capture.output(app)," getApp() =",capture.output(getApp()))
    ps = init.ps.session(ps=ps,,user.name=user.name, nick=nick,app=app)
    app$ps = ps 
    init.ps.handlers(ps)
    render.container.descendants(ps=ps,type.ind=1, use.mathjax=ps$use.mathjax, skip.if.rendered=FALSE)
    
    setApp(app)
  })
  
  
  app
}

fixed.header = function(ui, height="30px") {
  ui = div(
    style="
 width:100%;
 height:50px;
 position:fixed;
 top:0px;",            
    ui
  )
}

header.content.page = function(header, content, header.height = "50px") {
  restore.point("header.content.page")
  ui = div(style="margin: 0em 0 0em 0; width: 100%",
    div(
      style="width:100%; height:3em; position:fixed; top:0px;",
      header      
    ),
    div(
      style="top:3.1em; bottom:1em; overflow:auto; position: fixed; width: 100%",
      content     
    )
  )
  return(ui)
    
  ui= tagList(
    HTML('<section style="display: flex; flex-flow: column; height: 100%">'),
    HTML('<header>'),
    header,
    HTML('</header>'),
    div(style="flex: 1;", content),
    HTML('</section>')
  )
  return(ui)
  
  div(style = "height:100vh; display:table; width:100%;",
    div(
      style = "display:table-row;",
      header
    ),
    div(
      style = "display:table-row; height: 100%; overflow: auto;",
      content
    )
  )
}

select.ps.part.handler = function(value, shown_contents, app=getApp(), ps=app$ps, ...) {
  restore.point("select.ps.part.handler")
  
  bis = as.numeric(unlist(value))
  render.container(bi=bis[1],render.desc = TRUE, ps=ps)
  
  #cont.bi = which(ps$bdf$div.id %in% shown_contents)
  for (cbi in bis)
    show.container(bi = cbi,ps=ps)
}

init.ps.handlers = function(ps) {
  restore.point("init.ps.handler")
  
  # Add menu bar handler
  
  nestedSelectorHandler("rtNavbarSelector",fun = select.ps.part.handler)
  
  make.global.chunk.hotkey.handlers()
  # Add handlers for task chunks
  for (uk in ps$uk.li) {
    make.chunk.handlers(uk)
  }
  
  # Add handlers for addons
  # rows = which(ps$bdf$is.addon) 
  # for (bi in rows) {
  #   type = ps$bdf$type[[bi]]
  #   ao = ps$bdf$obj[[bi]]$ao
  #   # TO DO: Distinguish between global handlers
  #   # initialization and per user initilization
  #   handler.fun = ps$Addons[[type]]$init.handlers
  #   if (!is.null(handler.fun))
  #     handler.fun(ao)
  # }
  
}

is.cont.rendered = function(bi, ps) {
  restore.point("is.cont.rendered")
  
  if (is.null(ps$cont.state)) return(FALSE)
  isTRUE(ps$cont.state$rendered[[bi]])
}

get.ps.uk = function(ps, bi=NULL, stype.ind=NULL, chunk.ind=stype.ind) {
  if (is.null(chunk.ind)) {
    chunk.ind = ps$bdf$stype.ind[[bi]]    
  }
  ps$uk.li[[chunk.ind]]
}

render.rtutor.task.chunk = function(ps, bi) {
  restore.point("render.rtutor.task.chunk")
  uk = get.ts(task.ind=ps$bdf$task.ind[bi])
  make.chunk.handlers(uk)
  #  get.ps.uk(ps,bi=bi)
  update.chunk.ui(uk)
}

render.rtutor.addon = function(ps, bi) {
  restore.point("render.rtutor.addon")
  task.ind = ps$bdf$task.ind[bi]
  ts = get.ts(task.ind)
  ao = ts$ao
  type = ps$bdf$type[[bi]]
  Ao = ps$Addons[[type]]
  ui = Ao$ui.fun(ts=ts)
  output.id = ps$bdf$output.id[[bi]]  
  setUI(output.id, ui)
  Ao$init.handlers(ao=ao,ts=ts,bi=bi)
  #cat("render add on not yet implemented.")
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
  restore.point("add.slide.navigate.handlers")
  
  buttonHandler("rtPrevBtn",slide.prev)
  buttonHandler("rtNextBtn",slide.next)
  buttonHandler("rtForwardBtn",slide.forward)
  eventHandler(eventId="documentClickHandlerEvent",id=NULL, fun=slide.click)
}


slide.click = function(pageX,pageY,ps=app$ps, app=getApp(),...) {
  restore.point("slide.click")

  is.left = pageX < 100 #& value$pageX <= value$width * 0.125 
  
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
  restore.point("slide.next")
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
  hidden = ps$cont.state$hidden[bi]
  if (!is.null(ps$old.slide.bi)) {
    hide.container(ps,bi=ps$old.slide.bi)
  }
  if (is.rendered) {
    show.container(ps=ps,bi=bi)  
  } else {
    render.container(ps=ps,bi=bi)
    if (hidden)
      show.container(ps=ps,bi=bi)
  }
}


