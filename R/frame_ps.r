# To DO: More effective click handler in shinyEvents: register only a single javascript handler... dispatch in R to the corresponding function.

examples.frame.ps = function() {
  library(EconCurves)
  setwd("D:/libraries/RTutor2")
  txt = readLines("ex1.rmd", warn=FALSE)
  frame.ind = NULL
  ps = rtutor.make.frame.ps.te(txt, bdf.filter=bdf.frame.filter(frame.ind=frame.ind), catch.errors=FALSE)
  ps$lang = "de"
  app = slidesApp(ps)
  viewApp(app)
}

slidesApp = function(ps, slide.type="frame", start.slide=1, dir=getwd(), offline=FALSE, just.return.html=FALSE, catch.errors = TRUE, margin=2) {
  restore.point("slidesApp")
  library(shinyjs)
  
  app = eventsApp()
  app$ps = ps
  
  set.ps(ps)
  opts = default.ps.opts(catch.errors=catch.errors)
  set.ps.opts(opts=opts)
  init.ps.user.objects(ps)
  bdf = ps$bdf
  
  ps$offline = offline
  ps$use.mathjax = !offline
  ps$slide.type = slide.type
  ps$slide.ind = start.slide
  ps$num.slides = sum(ps$bdf$type==slide.type)
  
  # container state
  if (is.null(ps$constate)) {
    n = NROW(ps$bdf)
    ps$constate = data_frame(
      rendered = rep(FALSE,n),
      hidden = rep(FALSE,n)
    )
  }
  
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
  try(shiny::addResourcePath("figure",paste0(dir,"/figure")), silent=TRUE)  
  add.slide.navigate.handlers()
  
  set.slide(ps=ps)
  
  app
}

is.cont.rendered = function(bi, ps) {
  if (is.null(ps$constate)) return(FALSE)
  ps$constate$rendered[[bi]]
}


render.as.slide = function(ps, type=ps$slide.type, type.ind=1, bi=NULL, slide.ind=type.ind, title=NULL,num.slides=ps$num.slides, output.id=NULL, ...) {
  restore.point("render.as.slide")
 if (is.null(bi))
    bi = get.bdf.ind(type.ind=type.ind,type = type,bdf = bdf)
  
  if (is.null(title)) {
    title = ps$bdf$obj[[bi]]$title
  }
  
  render.container(ps=ps,type=type,type.ind=type.ind, bi=bi,output.id=output.id, ..., ui.fun = as.slide.ui.fun, title=title, num.slides=num.slides, slide.ind=slide.ind)
}

as.slide.ui.fun = function(ui, bi,ps, slide.ind,title,num.slides=ps$num.slides...) {
  header.ui = slide.title.bar.ui(title=title, slide.ind=slide.ind, num.slides=ps$num.slides)    
  tagList(div(
    id = paste0(ps$bdf$id[[bi]],"_rtutor_slide_div"),
    class="rtutor-slide-div",
    header.ui,
    ui
  ))
  
}

show.container = function(ps,type="ps",  type.ind=1, bi=NULL,anim=!FALSE) {
  restore.point("show.container")
  bdf = ps$bdf
  if (is.null(bi))
    bi = get.bdf.ind(type.ind=type.ind,type = type,bdf = bdf)
  
  div.id = ps$bdf$div.id[[bi]]
  shinyjs::show(div.id,anim=anim)
}

hide.container = function(ps,type="ps",  type.ind=1, bi=NULL, anim=!FALSE,animType="fade") {
  restore.point("hide.container")
  bdf = ps$bdf
  if (is.null(bi))
    bi = get.bdf.ind(type.ind=type.ind,type = type,bdf = bdf)
  
  div.id = ps$bdf$div.id[[bi]]
  shinyjs::hide(div.id,anim=anim, animType="animType")
}


render.container = function(ps, type="ps",  type.ind=1, bi=NULL, output.id=NULL, use.mathjax=isTRUE(ps$use.mathjax), render.desc = TRUE, skip.if.rendered=NA, skip.desc.if.rendered=FALSE, is.rendered=is.cont.rendered(bi=bi,ps=ps), add.navigation=FALSE, only.return.ui=FALSE, ui.fun = NULL,...) {
  restore.point("render.container")
  
  bdf = ps$bdf
  if (is.null(bi))
    bi = get.bdf.ind(type.ind=type.ind,type = type,bdf = bdf)
  
  is.static = bdf$is.static[bi]
  if (is.na(skip.if.rendered)) {
    skip.if.rendered = is.null(output.id) & !bdf$always.reload[bi]
  }
  
  # For static container only render descendants if output.id is not given
  no.render = (!only.return.ui) &
              ((is.static & is.null(output.id)) | 
              (skip.if.rendered & is.rendered))
  
  if (no.render) {
    
    if (render.desc) {
      render.container.descendants(ps=ps,bi=bi,output.id=NULL, use.mathjax=use.mathjax, skip.if.rendered=skip.desc.if.rendered, skip.desc.if.rendered=skip.desc.if.rendered)
    }
    return()
  }
  
  if (is.null(output.id)) {
    output.id = bdf$output.id[[bi]]   
  }
  
  stype = bdf$stype[[bi]]
  if (stype == "task_chunk") {
    render.rtutor.task.chunk(ps,bi=bi)
    return()
  }
  if (bdf$is.addon[[bi]]) {
    render.rtutor.addon(ps,bi=bi)
    return()
  }
  if (!is.static) {
    inner.ui = bdf$inner.ui[[bi]]
  } else {
    inner.ui = bdf$ui[[bi]]
  }
  if (!is.null(ui.fun)) {
    inner.ui = ui.fun(ui=inner.ui, bi=bi, ps=ps,...)
  }
  if (use.mathjax)
    inner.ui = withMathJax(inner.ui)

  if (only.return.ui) {
    return(inner.ui)
  }
  setUI(output.id, inner.ui)
  ps$constate$rendered[bi] = TRUE

  if (render.desc) {
    render.container.descendants(ps=ps,bi=bi,output.id=NULL, use.mathjax=use.mathjax, skip.if.rendered=skip.desc.if.rendered, skip.desc.if.rendered=skip.desc.if.rendered)
  }
  

}

render.container.descendants = function(ps, type="ps",  type.ind=1, bi=NULL, output.id=NULL, use.mathjax=TRUE, render.desc = TRUE, skip.if.rendered=NA, skip.desc.if.rendered=FALSE, is.rendered=is.cont.rendered(bi=bi,ps=ps), add.navigation=FALSE,...) {
  restore.point("render.container.descendants")
  bdf = ps$bdf
  if (is.null(bi))
    bi = get.bdf.ind(type.ind=type.ind,type = type,bdf = bdf)

  child.inds = which(bdf$parent_container==bi) 
  
  for (cbi in child.inds) {
    render.container(ps=ps,bi=cbi,output.id=NULL, use.mathjax=use.mathjax, skip.if.rendered=skip.if.rendered, skip.desc.if.rendered=skip.desc.if.rendered)
  }
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
  
  is.rendered = ps$constate$rendered[bi]
  if (!is.null(ps$old.slide.bi)) {
    hide.container(ps,bi=ps$old.slide.bi)
  }
  if (is.rendered) {
    show.container(ps=ps,bi=bi)  
  } else {
    render.as.slide(ps,type=ps$slide.type,type.ind=slide.ind, bi=bi)
  }
}


slide.title.bar.ui = function(title, slide.ind, num.slides) {
 
   div(class="rtutor-slide-title-bar",
    HTML("<table width='100%'><tr><td>"),
    h4(title),
    HTML("</td><td align='right' valign='top' nowrap>"),
    HTML("<table><tr><td valign='center' nowrap>"),    
    rtutor.navigate.btns(),
    HTML("</td><td valign='center' nowrap style='padding-left: 5px'>"),
    HTML(paste0(slide.ind, " of ",num.slides)),      
    HTML("</td></tr></table>"),
    HTML("</td></tr></table>")
  )  
}

