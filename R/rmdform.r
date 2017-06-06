examples.show.rmdform = function() {
  setwd("D:/libraries/RTutor2/examples/invest")
  file = "game1_result2.Rmd"
  show.rmdform(file=file)
}

show.rmdform = function(file = NULL, txt=readLines(file,warn = FALSE, encoding = "UTF8")) {
  restore.point("show.rmdform")
  txt = mark_utf8(txt)
  wid = rtutor.rmdform.parse(txt)
  ts = new.env(parent=globalenv())
  ts$wid = wid
  ts$data.env = new.env(parent=globalenv())
  try(wid$fun.env$update(ts=ts))
  ui = render.compiled.rmd(wid$cr,envir = ts$data.env)
  if (is.character(ui)) ui = HTML(ui)
  ui = with.mathjax(ui)
  view.html(ui=ui)
}

rtutor.widget.rmdform = function() {
  list(
    package = "RTutor",
    type = "rmdform",
    need.task.env = FALSE,
    change.task.env = FALSE,
    is.task = TRUE,
    use.clicker = TRUE,
    is.static = FALSE,
    parse.fun = rtutor.rmdform.parse,
    #make.org.task.state = rtutor.rmdform.make.org.task.state,
    init.handlers = rtutor.rmdform.init.handlers,
    listener.handler = rtutor.listener.handler,
    init.task.state = rtutor.rmdform.init.task.state,
    ui.fun = rtutor.rmdform.ui,
    update = rtutor.rmdform.update
  )
}



rtutor.rmdform.init.task.state = function(ts,ups=NULL, task.ind=ts$task.ind,...) {
  restore.point("rtutor.rmdform.init.task.state")
  
  ts$ready = TRUE
  if (!is.null(ts$wid$fun.env$update)) {
    ts$wid$fun.env$update(ts=ts)
  }
  ts
}

rtutor.rmdform.update = function(wid, bi, ps=get.ps(),...) {
  restore.point("rtutor.rmdform.update")
  
  ts = get.ts(bi = bi)
  if (!is.null(wid$fun.env[["update"]]))
    wid$fun.env$update(ts=ts, wid=wid, bi=bi,...)
}




rtutor.rmdform.ui = function(ts, wid=ts$wid, ps= get.ps(),...) {
  restore.point("rtutor.rmdform.ui")
  if (is.false(ts[["ready"]])) {
    return(ts$wid$notready.ui)
  }
  if (is.null(ts$data.env))
    ts$data.env = new.env(parent=globalenv())
  if (!is.null(ts$wid$form))
    set.form(ts$wid$form)
  ui = render.compiled.rmd(ts$wid$cr,envir = ts$data.env)
  if (is.character(ui)) {
    ui = HTML(ui)
  }
  #ui = mark_utf8(ui)
  with.mathjax(ui)
}

set.task.data = function(ts, data) {
  ts$data.env = as.environment(data)
  if (!is.null(ts$wid$fun.env)) {
    parent.env(ts$data.env) = ts$wid$fun.env
  } else {
    parent.env(ts$data.env) = parent.env(globalenv())
  }
}

rtutor.rmdform.init.handlers = function(wid=ts$wid,ps=get.ps(), app=getApp(),ts=NULL,...) {
  restore.point("rtutor.rmdform.init.handlers")
  
  if (!is.null(wid$form)) {
    library(webforms)
    add.form.handlers(wid$form, rtutor.rmdform.submit.handler, ts=ts)
  }
  if (!is.null(wid$fun.env$init.extra.handlers)) {
    wid$fun.env(init.extra.handlers(ts=ts, ps=ps))
  }
}


rtutor.listener.handler = function(ts, bi, target.ts, ps = get.ps(), ...) {
  restore.point("rtutor.listener.handler")
  if (!is.null(ts$wid$update))
    ts$wid$update(ts=ts, target.ts=target.ts)
  render.rtutor.widget(ps=ps,bi=bi,ts=ts, init.handlers=FALSE)
}


rtutor.rmdform.submit.handler = function(values,ts,ps=get.ps(), app=getApp(),wid=ts$wid,...) {
  restore.point("rtutor.rmdform.submit.handler")
  wid.submit.handler = wid$fun.env$submit.handler
  # by default a valid form entry makes the task solved
  # the wid.submit.handler may overwrite this decision however
  ts$solved = TRUE
  if (!is.null(wid.submit.handler)) {
    wid.submit.handler(values=values, ts=ts, ps=ps, ...)
  }
  process.checked.task(ts=ts)
}


rtutor.rmdform.parse = function(inner.txt,type="rmdform",name="",id=paste0("addon__",type,"__",name),args=NULL, bdf=NULL, bi=NULL, ps=get.ps(),...) {
  restore.point("rtutor.rmdform.parse")
  
  li = parse.hashdot.yaml(inner.txt)
  settings = read.yaml(text=li$settings)
  settings$out.type = first.non.null(settings$out.type, "shiny")
  form = settings[["form"]]
  
  id = first.non.null(settings[["id"]],id)
  try(ps$bdf$id[bi] <- id, silent=TRUE)
  
  if (!is.null(form)) {
    library(webforms)
    form = init.form(form, prefix=paste0(id,"_"))
    set.form(form)
  }  
  cr = compile.rmd(text = li$rmd, out.type = settings$out.type)
  if (!is.null(li$not.ready)) {
    notready.ui = render.compiled.rmd(compile.rmd(text=li$notready, out.type="shiny"))
  } else {
    notready.ui = HTML("Task not ready to be shown.")
  }
  funs.df = find.rmd.chunks(sep.lines(li$functions),add.code = TRUE)
  fun.env = new.env(parent = globalenv())
  for (i in seq_len(NROW(funs.df))) {
    code = funs.df$code[i]
    if (!is.null(code))
      eval(parse(text=code), fun.env)
  }
  wid = list(id=id,settings=settings,form=form,cr=cr,notready.ui=notready.ui, fun.env=fun.env)
  wid
}
 