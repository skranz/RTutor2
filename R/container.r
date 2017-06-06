
show.container = function(ps=am,type="armd",  type.ind=1, bi=NULL,anim=FALSE,am=NULL) {
  restore.point("show.container")
  bdf = ps$bdf
  if (is.null(bi))
    bi = get.bdf.ind(type.ind=type.ind,type = type,bdf = bdf)

  div.id = ps$bdf$div.id[[bi]]
  setHtmlShow(id=div.id)

  #shinyjs::show(div.id,anim=anim)

  if (!is.null(ps$cont.state))
    ps$cont.state$hidden[bi] = FALSE

}

hide.containers = function(ps=am, type, type.ind=NULL, bi=NULL,am=NULL) {
  if (is.null(bi)) {
    bis = which(ps$bdf$type == type)
    if (!is.null(type.ind)) bis = bis[type.ind]
  }
  for (bi in bis) {
    hide.container(ps,bi=bi,anim=FALSE)
  }
}

hide.container = function(ps=am,type="armd",  type.ind=1, bi=NULL, anim=FALSE,animType="fade",am=NULL) {
  restore.point("hide.container")
  bdf = ps$bdf
  if (is.null(bi))
    bi = get.bdf.ind(type.ind=type.ind,type = type,bdf = bdf)

  div.id = ps$bdf$div.id[[bi]]
  setHtmlHide(div.id)

  #shinyjs::hide(div.id,anim=anim, animType="animType")
  if (!is.null(ps$cont.state))
    ps$cont.state$hidden[bi] = TRUE

}


render.container = function(ps=am, type="armd",  type.ind=1, bi=NULL, output.id=NULL, use.mathjax=isTRUE(ps$use.mathjax), render.desc = TRUE, skip.if.rendered=NA, skip.desc.if.rendered=FALSE, is.rendered=NA, only.return.ui=FALSE, ui.fun = NULL,am=NULL, ...) {
  restore.point("render.container")

  bdf = ps$bdf
  if (is.null(bi))
    bi = get.bdf.ind(type.ind=type.ind,type = type,bdf = bdf)
  cat("\nrender container ",bi, ps$bdf$id[bi])

  if (is.na(skip.if.rendered)) {
    skip.if.rendered = TRUE
  }

  if (is.na(is.rendered)) {
    is.rendered = isTRUE(try(is.cont.rendered(bi=bi,ps=ps)))
  }

  if (!is.null(ps$cont.state))
    ps$cont.state$rendered[bi] = TRUE


  is.widget =ps$bdf$is.widget[[bi]]
  # For static container only render descendants if output.id is not given
  no.render = (!only.return.ui) &
              ((is.widget & is.null(output.id)) |
              (skip.if.rendered & is.rendered))

  if (no.render) {

    if (render.desc) {
      render.container.descendants(ps=ps,bi=bi,output.id=NULL, use.mathjax=use.mathjax, skip.if.rendered=skip.if.rendered, skip.desc.if.rendered=skip.desc.if.rendered, add.desc.title=add.desc.title)
    }
    return()
  }

  if (is.null(output.id)) {
    output.id = bdf$output.id[[bi]]
  }

  if (bdf$is.widget[[bi]]) {
    cat("\nBefore render.rtutor.widget")
    render.rtutor.widget(ps,bi=bi)
    return()
  }
  if (is.widget) {
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

render.container.descendants = function(ps=am, type="armd",  type.ind=1, bi=NULL, output.id=NULL, use.mathjax=TRUE, render.desc = TRUE, skip.if.rendered=NA, skip.desc.if.rendered=FALSE,am=NULL, ...) {
  restore.point("render.container.descendants")
  bdf = ps$bdf
  cat("\nBefore get.bdf.ind")
  if (is.null(bi)) {
    bi = get.bdf.ind(type.ind=type.ind,type = type,bdf = bdf)
  }
  cat("\nAfter get.bdf.ind")

  child.inds = which(bdf$parent_container==bi)
  cat("\nAfter which")

  for (cbi in child.inds) {
    cat("\nBefore render.container")
    render.container(ps=ps,bi=cbi,output.id=NULL, use.mathjax=use.mathjax, skip.if.rendered=skip.if.rendered, skip.desc.if.rendered=skip.desc.if.rendered)
  }
  cat("\nEnd render.container.descendants")
  
}


is.cont.rendered = function(bi, ps=am, am=NULL) {
  restore.point("is.cont.rendered")
  
  if (is.null(ps$cont.state)) return(FALSE)
  isTRUE(ps$cont.state$rendered[[bi]])
}

