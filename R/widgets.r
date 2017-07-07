# render widgets when app starts
# render some widgets directly and others delayed
initial.render.widgets = function(ps, n.direct=5) {
  bis = which(ps$bdf$is.widget)
  first = bis[1:min(n.direct,length(bis))]
  other =  setdiff(bis, first)
  
  render.rtutor.widgets(ps=ps,bis=first)
    
  if (length(other)>0) {
    shinyDelayedRun(100, function(ps,other,...) {
      render.rtutor.widgets(ps=ps, bis=other)
    },ps=ps, other=other)
  }
}

render.first.widgets = function(ps,n=2) {
  if (length(bis)==0) return()
  bis = 
  render.rtutor.widgets(ps=ps, bis=bis)
}



rtutor.parse.widget = function(bi, ps, opts=ps$opts) {
  restore.point("rtutor.parse.widget")
  
  bdf = ps$bdf; br = bdf[bi,];
  type = br$type
  
  # special treatment for chunks and awards
  # in order not to change old code too much
  if (type=="chunk") {
    rtutor.parse.chunk(bi,ps,opts)
    return()
  } else if (type=="award") {
    rtutor.parse.award(bi=bi, ps=ps)
    return()
  }
  
  Wid = ps$Widgets[[type]]
  
  args = parse.block.args(arg.str = br$arg.str)
  if (!is.null(args$name)) ps$bdf$name[[bi]] = args$name
  
  use.clicker = !is.null(Wid[["clicker"]]) & isTRUE(opts$use.clicker)
  
  # if (use.clicker & is.null(opts[["courseid"]])) {
  #   use.clicker = FALSE
  #   cat("\nClicker cannot be used since no 'courseid' is specified in the settings.")
  # }
  # if (use.clicker & is.null(opts[["clicker.dir"]])) {
  #   use.clicker = FALSE
  #   cat("\nClicker cannot be used since no 'clicker.dir' is specified in the settings.")
  # }
  
    
  if (use.clicker) {
    library(RTutorClicker)
    wid = call.fun(Wid$clicker$parse.fun,
      block.txt = ps$txt[br$start:br$end],
      inner.txt = ps$txt[(br$start+1):(br$end-1)],
      id = paste0(type,"__",bi),
      args = args,
      type = type,
      bdf=bdf,
      bi = bi,
      ps = ps
    )
    wid$type = type
    wid$use.clicker = TRUE
    wid$task.id = wid$id
    
    ps$bdf$is.container[[bi]] = TRUE
    set.container.div.and.output(bi,ps)
  } else {
    wid = Wid$parse.fun(
      block.txt = ps$txt[br$start:br$end],
      inner.txt = ps$txt[(br$start+1):(br$end-1)],
      id = paste0(type,"__",bi),
      args = args,
      type = type,
      bdf=bdf,
      bi = bi,
      ps = ps
    )
    wid$type = type
    wid$use.clicker = FALSE
    wid$task.id = wid$id
    
    if (!is.null(Wid$ui.fun)) {
      # the widget will be put inside a container
      ps$bdf$is.container[[bi]] = TRUE
      set.container.div.and.output(bi,ps)
    }
  
    if (isTRUE(Wid$is.task)) {
      ps$bdf$is.task[[bi]] = Wid$is.task
      wid$task.ind = sum(ps$bdf$is.task[1:bi])
      
      create.bi.task.env.info(bi=bi,ps=ps,need.task.env = isTRUE(Wid$need.task.env),change.task.env = isTRUE(Wid$change.task.env),args=list(optional = TRUE),presolve.task = opts$presolve, opts=opts)  
  
    }
  }
  
  ps$bdf$obj[[bi]] = list(wid=wid)
  
  if (!isTRUE(opts$rtutor)) {
    ps$bdf$ui[[bi]] = first.non.null(wid[["armd.ui"]],wid[["ui"]])
  }
  
  return()
}


# will be called from parse.armd
rtutor.init.widgets = function(ps) {
  bdf = ps$bdf
  restore.point("rtutor.init.widgets")
  
  n = NROW(ps$bdf)
  ps$bdf = mutate(bdf,
    is.task = FALSE,task.ind = 0,
    task.line = NA_character_,
    task.in = vector("list", n),
    task.listeners = vector("list",n),
    
    # These arguments deal with task.envs
    need.task.env = FALSE,
    change.task.env = FALSE,
    presolve.task = ps$opts$presolve
  )
  bdf = ps$bdf
  widgets = na.omit(unique(bdf$type[bdf$is.widget]))

  # currently still
  # special treatment for chunks and awards
  widgets = setdiff(widgets,c("chunk","award"))
  
  Widgets = lapply(widgets, function(widget) {
    pkg = get.bt(widget,ps)$package
    call = parse(text=paste0(pkg,":::rtutor.widget.",widget,"()"))
    Widget = eval(call)
  })
  names(Widgets) = widgets
  ps$Widgets=Widgets
  ps$env = new.env(parent=ps$init.env)

}

render.rtutor.widgets = function(ps, init.handlers=TRUE,bis = which(ps$bdf$is.widget)) {
  restore.point("render.rtutor.widgets")
  for (bi in bis) {
    render.rtutor.widget(ps=ps, bi=bi, init.handlers=init.handlers)
  }
}

render.rtutor.widget = function(ps, bi,  ts=NULL, init.handlers=TRUE, dset=TRUE, opts=ps$opts) {
  restore.point("render.rtutor.widget")

  type = ps$bdf$type[[bi]]
  # special treatment for chunks and awards
  # in order not to change old code too much
  if (type=="chunk") {
    render.rtutor.task.chunk(ps=ps,bi=bi)
    return()
  } else if (type=="award") {
    show.award(award.bi = bi, ps=ps)
    return()
  }
  cat("\n******************************************")
  cat("\nrender.rtutor.widget")

  wid = ps$bdf$obj[[bi]]$wid
  type = ps$bdf$type[[bi]]
  Wid = ps$Widgets[[type]]
  
  output.id = ps$bdf$output.id[[bi]]  
  
  use.clicker = isTRUE(wid$use.clicker)
  
  # render clicker widget
  if (use.clicker) {
    library(RTutorClicker)
    ui = clicker.server.ui.fun(wid=wid, Wid=Wid)
    setUI(output.id, ui)
    if (dset)
      dsetUI(output.id, ui)
    
    if (init.handlers)
      clicker.server.init.handlers(wid=wid, Wid=Wid)
    
  } else {
    if (is.null(ts))
      ts = get.ts(bi=bi)
    ui = Wid$ui.fun(ts=ts)
    
    setUI(output.id, ui)
    if (dset)
      dsetUI(output.id, ui)
  
    if (init.handlers)
      Wid$init.handlers(wid=wid,ts=ts,bi=bi, opts=opts)
  }
  
  cat("end render.rtutor.widget")
}

update.widget = function(id, bi = which(ps$bdf$id==id),ps=get.ps(),...) {
  restore.point("update.widget")
  cat("\n++++++++++++++++++++++++++++++++++++++++++")
  cat("\nupdate.widget")

  Wid = get.Widget(bi=bi)
  wid = get.widget(bi=bi)
  if (!is.null(Wid[["update"]]))
    Wid$update(wid=wid,bi=bi,...)
  render.rtutor.widget(bi=bi, ps=ps, init.handlers = FALSE)
}

get.widget = function(bi, ps=get.ps()) {
  ps$bdf$obj[[bi]]$wid
}

make.widgets.list = function(widgets="quiz") {
   li = lapply(widgets, function(widget) {
     fun = paste0("rtutor.widget.",widget)
     do.call(fun,list())
   })
   names(li) = widgets
  li
}

get.Widget = function(type=ps$bdf$type[[bi]],bi=NULL, ps=get.ps()) {
  ps$Widgets[[type]]
}

check.Widget = function(Wid) {
  restore.point("check.Widget")

}

check.Widget.function = function(fun.name, type="") {
  for (fun in fun.name) {
    if (is.null(Wid[[fun]])) {
      stop(paste0("The widget ", type, " has not defined the function ", fun))
    }
  }
}
check.Widget.field = function(fields, type="") {
  for (field in fields) {
    if (is.null(Wid[[field]])) {
      stop(paste0("The widget ", type, " has not defined the required field ", field))
    }
  }
  
}

get.yaml.block.args = function(bi,ps) {
  restore.point("get.yaml.block.args")

  args = parse.block.args(arg.str = ps$bdf$arg.str[[bi]])
  yaml = get.bi.ps.str(bi,ps)
  if (!is.null(yaml)) {
    yaml.arg = yaml.load(paste0(yaml,collapse="\n"))
    args[names(yaml.arg)] = yaml.arg
  }
  args
}


dsetUI = function(...) {
  shinyEvents::dsetUI(...)
  shinyEvents::setUI(...)
}