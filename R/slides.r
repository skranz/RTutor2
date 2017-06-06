

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


