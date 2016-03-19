

make.base.env = function() {
  new.env(parent=parent(globalenv()))
}

make.fresh.task.env = function(task.ind=ts$task.ind, ps=get.ps(), ts=NULL) {
  restore.point("make.fresh.task.env")
  #stop()
  penv = get.task.parent.env(task.ind)
  senv = get.task.start.env(task.ind)
  if (is.null(senv)) {
    env = new.env(parent = penv)
  } else {
    env = as.environment(as.list(senv))
    parent.env(env) = penv    
  }
  env
}


get.task.parent.env = function(task.ind=ts$task.ind,ps = get.ps(), ts=NULL) {
  if (!is.null(ps$pre.env)) return(ps$pre.env)
  new.env(parent=parent(globalenv()))
}

get.task.start.env = function(task.ind=ts$task.ind, ps = get.ps(), opts = rt.opts(),ts=NULL, bi=NULL) {
  restore.point("get.task.start.env")
  
  if (opts$precomp) stop("get.task.start.env for precompute not yet implemented.")
  
  if (is.null(bi))
    bi = ps$task.table$bi[task.ind]
  
  # the bi that contains the start env
  start.bi = ps$bdf$task.start.env.bi[bi]
  # return empty env if no start.bi is defined
  if (is.na(start.bi)) return(NULL)
  
  get.task.env(bi=start.bi)
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
