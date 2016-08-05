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