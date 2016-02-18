examples.frame.ps = function() {
  library(EconCurves)
  setwd("D:/libraries/RTutor2")
  txt = readLines("ex1.rmd")
  frame.ind = NULL
  te = rtutor.make.frame.ps.te(txt, bdf.filter=bdf.frame.filter(frame.ind=frame.ind), catch.errors=FALSE)
  te$lang = "de"
  bdf = te$bdf
  show.frame.ps(te)
}

show.frame.ps = function(ps, frame.ind=1, dir=getwd(), offline=FALSE, just.return.html=FALSE) {
  restore.point("show.frame.ps")
  
  app = eventsApp()
  app$ps = ps
  resTags = rtutor.html.ressources()
  app$ui = tagList(
    resTags,
    docClickEvents(id="doc_click"),
    fluidPage(
      uiOutput("frameUI")
      #,highlight.code.script()
    )
  )
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

show.dyn.ui = function(stype=bdf$stype[[bi]], obj=bdf$obj[[bi]], bi, bdf = ps$bdf,ps=NULL) {
  
  # TO DO: store whether UI really needs an update...
  if (stype=="task_chunk") {
    update.chunk.ui(ck=obj$ck)
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
  dyn.ui.ind = which(bdf$parent_frame == bi & bdf$has.dyn.ui)
  for (cbi in dyn.ui.ind) {
    show.dyn.ui(bi = cbi, bdf=bdf)
  }
  
  obj = ps$bdf$obj[[bi]]
  # header.ui = tagList(
  #   HTML("<table width='100%'><tr><td>"),
  #   h4(obj$title),
  #   HTML("</td><td align='right' valign='top' nowrap>"),
  #   list(
  #     rtutor.navigate.btns(),
  #     HTML(paste0(frame.ind, " of ",ps$num.frames))      
  #   ),
  #   HTML("</td></tr></table>")
  # )

  header.ui = tagList(
    HTML("<table width='100%'><tr><td>"),
    h4(obj$title),
    HTML("</td><td align='right' valign='top' nowrap>"),
    HTML("<table><tr><td valign='center' nowrap>"),    
    rtutor.navigate.btns(),
    HTML("</td><td valign='center' nowrap style='padding-left: 5px'>"),
    HTML(paste0(frame.ind, " of ",ps$num.frames)),      
    HTML("</td></tr></table>"),
    HTML("</td></tr></table>")
  )
  
  
  if (!use.mathjax) {
    ui = bdf$ui[bi]     
  } else {
    ui = bdf$obj[[bi]]$mathjax.ui
  }
  frame.ui = list(header.ui,ui)

  setUI("frameUI",frame.ui)
  
  invisible(frame.ui)
}


# Add javascript to deal with clicks on free html area,
# i.e. not on inputs, buttons, links or images
# can be used to proceed with slides
docClickEvents = function(id="doc_click") { 
  tags$script(paste0('
  $(document).on("click", function (e) {
    var nn = e.target.nodeName;

    if (nn === "BUTTON" || nn === "IMG" || nn === "INPUT" || nn === "A") {
      return;
    }
    var pn = e.target.parentNode;
    if (pn.className === "radio" || pn.className === "checkbox") {
      return;
    }
    var gpn = pn.parentNode;
    if (gpn.className === "radio" || gpn.className === "checkbox") {
      return;
    }

    Shiny.onInputChange("',id,'", {targetId: e.target.id, targetType: nn, nonce: Math.random(), pageX: e.pageX, pageY: e.pageY});
  });'))
}

