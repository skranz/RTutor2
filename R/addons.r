
render.rtutor.addon = function(ps, bi,  ts = get.ts(bi=bi), init.handlers=TRUE) {
  restore.point("render.rtutor.addon")
  cat("\n******************************************")
  cat("\nrender.rtutor.addon")

  ao = ts$ao
  type = ps$bdf$type[[bi]]
  Ao = ps$Addons[[type]]
  ui = Ao$ui.fun(ts=ts)
  output.id = ps$bdf$output.id[[bi]]  
  setUI(output.id, ui)
  dsetUI(output.id, ui)
  if (init.handlers)
    Ao$init.handlers(ao=ao,ts=ts,bi=bi)
  #cat("render add on not yet implemented.")
}



update.addon = function(id, bi = which(ps$bdf$id==id),ps=get.ps(),...) {
  restore.point("update.addon")
  cat("\n++++++++++++++++++++++++++++++++++++++++++")
  cat("\nupdate.addon")

  Ao = get.Addon(bi=bi)
  ao = get.addon(bi=bi)
  if (!is.null(Ao[["update"]]))
    Ao$update(ao=ao,bi=bi,...)
  render.rtutor.addon(bi=bi, ps=ps, init.handlers = FALSE)
}

get.addon = function(bi, ps=get.ps()) {
  ps$bdf$obj[[bi]]$ao
}

make.addons.list = function(addons="quiz") {
   li = lapply(addons, function(addon) {
     fun = paste0("rtutor.addon.",addon)
     do.call(fun,list())
   })
   names(li) = addons
  li
}

get.Addon = function(bi=NULL,type=ps$bdf$type[[bi]], ps=get.ps()) {
  ps$Addons[[type]]
}

check.Addon = function(Ao) {
  restore.point("check.Addon")
  check.Addon.field(c("type"),type="")  
  type = Ao$type
  check.Addon.field(c("package","is.task","is.static"),type=type)   
  check.Addon.function("parse.fun",type)
  if (Ao$is.task) {
    check.Addon.function(c("init.task.state","init.handlers", "ui.fun"),type)
    check.Addon.field(c("need.task.env","change.task.env"),type=type) 
  }
  
  
  
}

check.Addon.function = function(fun.name, type="") {
  for (fun in fun.name) {
    if (is.null(Ao[[fun]])) {
      stop(paste0("The addon ", type, " has not defined the function ", fun))
    }
  }
}
check.Addon.field = function(fields, type="") {
  for (field in fields) {
    if (is.null(Ao[[field]])) {
      stop(paste0("The addon ", type, " has not defined the required field ", field))
    }
  }
  
}