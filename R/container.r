
show.container = function(ps,type="ps",  type.ind=1, bi=NULL,anim=FALSE) {
  restore.point("show.container")
  bdf = ps$bdf
  if (is.null(bi))
    bi = get.bdf.ind(type.ind=type.ind,type = type,bdf = bdf)
  
  div.id = ps$bdf$div.id[[bi]]
  shinyjs::show(div.id,anim=anim)
  
  if (!is.null(ps$cont.state))
    ps$cont.state$hidden[bi] = FALSE

}

hide.containers = function(ps, type, type.ind=NULL, bi=NULL) {
  if (is.null(bi)) {
    bis = which(ps$bdf$type == type)
    if (!is.null(type.ind)) bis = bis[type.ind]
  }
  for (bi in bis) {
    hide.container(ps,bi=bi,anim=FALSE)
  }
}

hide.container = function(ps,type="ps",  type.ind=1, bi=NULL, anim=FALSE,animType="fade") {
  restore.point("hide.container")
  bdf = ps$bdf
  if (is.null(bi))
    bi = get.bdf.ind(type.ind=type.ind,type = type,bdf = bdf)
  
  div.id = ps$bdf$div.id[[bi]]
  shinyjs::hide(div.id,anim=anim, animType="animType")
  if (!is.null(ps$cont.state))
    ps$cont.state$hidden[bi] = TRUE
  
}


render.container = function(ps, type="ps",  type.ind=1, bi=NULL, output.id=NULL, use.mathjax=isTRUE(ps$use.mathjax), render.desc = TRUE, skip.if.rendered=NA, skip.desc.if.rendered=FALSE, is.rendered=NA, only.return.ui=FALSE, ui.fun = NULL, ...) {
  restore.point("render.container")
  
  bdf = ps$bdf
  if (is.null(bi))
    bi = get.bdf.ind(type.ind=type.ind,type = type,bdf = bdf)
  cat("\nrender container ",bi, ps$bdf$id[bi])
  
  is.static = bdf$is.static[bi]
  if (is.na(skip.if.rendered)) {
    skip.if.rendered = TRUE
  }
  
  if (is.na(is.rendered)) {
    is.rendered = isTRUE(try(is.cont.rendered(bi=bi,ps=ps)))   
  }
  
  if (!is.null(ps$cont.state))
    ps$cont.state$rendered[bi] = TRUE

  
  # For static container only render descendants if output.id is not given
  no.render = (!only.return.ui) &
              ((is.static & is.null(output.id)) | 
              (skip.if.rendered & is.rendered))
  
  if (no.render) {
    
    if (render.desc) {
      render.container.descendants(ps=ps,bi=bi,output.id=NULL, use.mathjax=use.mathjax, skip.if.rendered=skip.desc.if.rendered, skip.desc.if.rendered=skip.desc.if.rendered, add.desc.title=add.desc.title)
    }
    return()
  }
  
  if (is.null(output.id)) {
    output.id = bdf$output.id[[bi]]   
  }
  
  stype = bdf$stype[[bi]]
  if (stype == "task_chunk") {
    restore.point("render.container.task.chunk")
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
    inner.ui = with.mathjax(inner.ui)

  if (only.return.ui) {
    return(inner.ui)
  }
  setUI(output.id, inner.ui)
  ps$cont.state$rendered[bi] = TRUE

  if (render.desc) {
    render.container.descendants(ps=ps,bi=bi,output.id=NULL, use.mathjax=use.mathjax, skip.if.rendered=skip.desc.if.rendered, skip.desc.if.rendered=skip.desc.if.rendered)
  }
  

}

render.container.descendants = function(ps, type="ps",  type.ind=1, bi=NULL, output.id=NULL, use.mathjax=TRUE, render.desc = TRUE, skip.if.rendered=NA, skip.desc.if.rendered=FALSE,...) {
  restore.point("render.container.descendants")
  bdf = ps$bdf
  if (is.null(bi))
    bi = get.bdf.ind(type.ind=type.ind,type = type,bdf = bdf)

  child.inds = which(bdf$parent_container==bi) 
  
  for (cbi in child.inds) {
    render.container(ps=ps,bi=cbi,output.id=NULL, use.mathjax=use.mathjax, skip.if.rendered=skip.if.rendered, skip.desc.if.rendered=skip.desc.if.rendered)
  }
}
