# To DO: More effective click handler in shinyEvents: register only a single javascript handler... dispatch in R to the corresponding function.

examples.frame.ps = function() {
  setwd("D:/libraries/RTutor3")
  ps = create.ps(file="test.rmd", catch.errors=FALSE)
  bdf = ps$bdf
  
  app = rtutorApp(ps,catch.errors = FALSE)
  viewApp(app)
  viewApp(app, launch.browser = rstudio::viewer)
}

# Init ps for a new session
init.ps.session = function(ps, user.name, nick=user.name, app=getApp(), rendered=FALSE, hidden=FALSE) {
  
  # make shallow copy of ps
  ps = as.environment(as.list(ps))
  set.ps(ps)
  ps$ses.id = random.string(nchar=12)

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
initRTutorApp = function(ps, catch.errors = TRUE, offline=FALSE, use.mathjax = !offline, opts=list(), dir=getwd(), figure.dir = paste0(dir,"/", ps$figure.sub.dir), ups.dir=dir, ...) {
  restore.point("initRTutorApp")

  app = eventsApp()
  setAppHasBottomScript(TRUE, app=app)

  if (!is.null(ps$figure.web.dir))
    addResourcePath(ps$figure.web.dir,figure.dir)

  #shiny::addResourcePath(prefix = "shinyAce", directoryPath = system.file("www", package = "shinyAce"))
  #shinyAce:::initResourcePaths()
  
  ps$opts[names(opts)] = opts
  
  
  ps$opts$use.clicker = first.non.null(ps$opts$use.clicker, nchar(ps$opts$clicker.dir)>0 & nchar(ps$opts$course.id)>0)
  
  app$ps = ps
  ps$dir = dir
  ps$ups.dir = ups.dir
  ps$offline = offline
  ps$use.mathjax = use.mathjax
  ps$is.shiny = TRUE
  ps$opts$is.shiny=TRUE
  ps$opts$catch.errors = catch.errors
  ps$given.awards.bi = NULL
  
  load.ps.libs(ps$opts$libs)
  
  set.rt.opts(ps$opts)
  
  
  
  app$ps = ps

  bdf = ps$bdf
    
  try(shiny::addResourcePath("figure",paste0(dir,"/figure")), silent=TRUE)  

  app
}

slidesApp = function(ps,user.name = "John Doe", nick=user.name, start.slide=first.non.null(ps$start.slide,1), dir=getwd(), ups.dir=dir, offline=FALSE, just.return.html=FALSE, catch.errors = TRUE, margin=2, opts=list()) {
  restore.point("slidesApp")
  
  app = initRTutorApp(ps=ps, catch.errors = catch.errors,offline = offline, dir=dir, ups.dir=ups.dir, opts=opts)

  app$ui = ps$ui

  # Each time the problem set is restarted
  # reinit the problem set
  appInitHandler(app=app,function(app,...) {
    restore.point("slidesApp.appInitHandler")
    ps = init.ps.session(ps=ps,user.name=user.name, nick=nick,app=app)
    ps$slide.ind = start.slide
    app$ps = ps 
    init.ps.handlers(ps)
    
    render.rtutor.widgets(ps=ps)
  })

  
  app
}


rtutorApp = function(ps, user.name = "John Doe", nick=user.name, dir=getwd(), ups.dir=dir, offline=FALSE, just.return.html=FALSE, catch.errors = TRUE, margin=2,opts=list(),...) {
  restore.point("rtutorApp")
  
  if (isTRUE(ps$slides)) {
    return(slidesApp(ps=ps,user.name=user.name, nick=nick,dir=dir, offline=offline, catch.errors=catch.errors, margin=margin,opts=opts, ups.dir=ups.dir,...))
  }
  
  app = initRTutorApp(ps=ps, catch.errors = catch.errors,offline = offline,dir=dir, ups.dir=ups.dir, opts=opts,...)
  
  app$ui = ps$ui

  # Each time the problem set is restarted
  # reinit the problem set
  appInitHandler(app=app,function(app,...) {
    restore.point("rtApp.appInitHandler")
    cat("app =",capture.output(app)," getApp() =",capture.output(getApp()))
    setApp(app)
    ps = init.ps.session(ps=ps,user.name=user.name, nick=nick,app=app)
    app$ps = ps 
    init.ps.handlers(ps)
    initial.render.widgets(ps)
    # cat("\n\ndelayed Run\n\n"))
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
}

init.ps.handlers = function(ps) {
  restore.point("init.ps.handler")
  
  
  sidebar.ui.handlers(ps)
  # Add menu bar handler
  #nestedSelectorHandler("rtNavbarSelector",fun = select.ps.part.handler)
  
  make.global.chunk.hotkey.handlers()
  # Add handlers for task chunks
  for (uk in ps$uk.li) {
    make.chunk.handlers(uk)
  }
}

get.ps.uk = function(ps, bi=NULL, stype.ind=NULL, chunk.ind=stype.ind) {
  if (is.null(chunk.ind)) {
    chunk.ind = ps$bdf$stype.ind[[bi]]    
  }
  ps$uk.li[[chunk.ind]]
}

render.rtutor.task.chunk = function(ps, bi) {
  uk = get.ts(task.ind=ps$bdf$task.ind[bi])
  restore.point("render.rtutor.task.chunk")
  make.chunk.handlers(uk)
  update.chunk.ui(uk,dset = TRUE)
}

rtutor.navigate.btns = function() {
  btns = tagList(
    smallButton("rtPrevBtn","<",size = "extra-small"),
    smallButton("rtNextBtn",">",size = "extra-small"),
    smallButton("rtForwardBtn",">>",size = "extra-small"),
    bsButton("rtSlideMenuBtn","", size="extra-small", icon=icon(name="bars", lib="font-awesome"))
  )
  btns
}


add.slide.navigate.handlers = function() {
  restore.point("add.slide.navigate.handlers")
  
  buttonHandler("rtPrevBtn",slide.prev)
  buttonHandler("rtNextBtn",slide.next)
  buttonHandler("rtForwardBtn",slide.forward)
  eventHandler(eventId="documentClickHandlerEvent",id=NULL, fun=slide.click)
  buttonHandler("rtSlideMenuBtn",slide.menu.click)
  buttonHandler("rtCloseMenuBtn", function(...) {
    restore.point("rtCloseMenuBtn")
    setHtmlHide(id="slideMenuDiv")
  })
}


slide.click = function(pageX,pageY,ps=app$ps, app=getApp(),...) {
  restore.point("slide.click")

  is.left = isTRUE(pageX < 100) #& value$pageX <= value$width * 0.125 
  
  if (is.left) {
    slide.prev(ps=ps,app=app,...)
  } else {
    slide.next(ps=ps,app=app,...)
  }
}


rtutor.bubble.click = function(pageX=NA,pageY=NA,app=getApp(), ps=get.ps(),...) {
  if (isTRUE(ps$slides)) {
    slide.click(pageX=pageX,pageY=pageY,...)
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


slide.menu.click = function(..., ps=app$ps, app=getApp()) {
  restore.point("slide.menu.click")
  slide.ind = ps$slide.ind

  dsetUI(id="slideMenuUI",make.slide.menu.ui(ps=ps))
  setHtmlShow("slideMenuDiv")
}


make.slide.menu.ui = function(ps, slide.ind=ps$slide.ind) {
  div(width="100%",
    tags$button(id="rtCloseMenuBtn",style="position: absolute; top: 0; right: 0",class="btn btn-small action-btn", "X"),
    bsButton("rtSendClickerBtn" ,"Send as Clicker", size="small"),
    bsButton("rtClickerStatsBtn","Clicker Stats", size="small")
  )
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



load.ps.libs = function(libs) {
  if (length(libs)==0)
    return()
  for (i in seq_along(libs)) {
    lib = libs[i]
    display("load package ", lib, "...")
    ret = suppressWarnings(require(lib, quietly=TRUE, warn.conflicts =FALSE,character.only=TRUE))
    if (!ret) {
      stop(paste0("Please install the package '", lib,
                  "', which is required to solve this problem set."))
    }
    display("... all required packages loaded.")
  }
}
