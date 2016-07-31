make.base.env = function() {
  new.env(parent=parent(globalenv()))
}

make.fresh.task.env = function(task.ind=ts$task.ind, ps=get.ps(), ts=NULL) {
  restore.point("make.fresh.task.env")
  #stop()
  penv = get.task.parent.env(task.ind)
  
  # shallow copy objects from earlier tasks in task.line
  # and from task.in
  req = ps$task.table$required.tasks.ind[[task.ind]]
  if (length(req)==0) {
    env = new.env(parent = penv)  
  } else {
    ienv = get.task.env(task.ind = req[1],ps = ps)
    env = as.environment(as.list(ienv))
    for (iind in req[-1]) {
      ienv = get.task.env(task.ind = iind,ps = ps)
      copy.into.env(source=ienv, dest=env)
    }
    parent.env(env) = penv    
  }
  env
}


get.task.parent.env = function(task.ind=ts$task.ind,ps = get.ps(), ts=NULL) {
  ps$init.env
}


get.task.env = function(task.ind=ts$task.ind,bi=NULL, ps=get.ps(), ts=NULL) {
  restore.point("get.task.env")
  if (is.null(ts)) {
    if (is.null(task.ind)) {
      if (ps$bdf$is.task[bi]) {
        task.ind = ps$bdf$task.ind[bi]
      }
    }
    if (!is.null(task.ind)) {
      ts = get.ts(task.ind=task.ind)
    }    
  }
  if (!is.null(ts)) {
    if (is.null(ts$task.env)) {
      ts$task.env = make.fresh.task.env(ts=ts)
    }
    return(ts$task.env)
  }
  stop("get.task.env only implemented for tasks so far!")
  #ps$task.envs[[bi]]  
}
