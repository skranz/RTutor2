# Tasks:
#
# - Examples for tasks are quizzes and chunks that an user needs to solve.
#
# - A task is an object that requires a user input.
#
# - In general, tasks can be solved, and scores, points and awards can be given
#   max.points are assigned to a task.
#
# - A task can internally contain subtasks,
#   but from RTutor perspective, a task is an atom.
#
# - Each task can be mapped to a block or chunk in the solution file,
#   but several subblocks of that block or chunk can describe the task.
#
# - Each task should have a unique id, if not provided in the rmd, we should create one
#
# - Tasks may or may not change the task.env. This depends on task type and options.
#
# - task.state: user specific state of a task
#
#     - Minimum data storage for a task is solved, points, and score.
#     - Task states are stored in app$task.list
#     - ps$tt can contain some background information like max.points that
#       speeds up computation of some statistics
#     - After a task state has changed, we may want to update ups, but
#       ups does not directly store task.state
#     - update from and to ups is performed by the functions
#       - update.task.state.from.ups
#       - update.ups.from.task.state
#     - Additionally tasks can have a task state ts that contains additional
#       information. The structure of the task state depends on the task object.
#       For a chunk, the task state could contain the user code and information
#       about which tests are passed.
#     - Tasks that can change task.env have a post task environment.
#     - Task states and environments may be stored in ups
#       or in separate files or they may not be stored.
#
# - Atomizing: It should be possible to present tasks separately to users,
#   independently of the problem set context. Depending on the task, we may
#   have to provide a pretask environment, however.
#   For example, in a lecture, we may push a quiz task from RTutor frames,
#   as a live "clicker" quiz to students' mobile phones.
#

update.task.ui = function(ui, bi=ts$bi, ts=NULL) {
  
}

are.required.tasks.solved = function(task.ind, ps) {
  restore.point("are.required.tasks.solved")
  req = ps$task.table$required.tasks.ind[[task.ind]]
  solved = sapply(req,is.task.solved, ps=ps)
  ok = all(solved)
  unsolved = req[!solved]
  list(ok=ok, unsolved = unsolved)
}

is.task.solved = function(task.ind, ps) {
  get.ts(task.ind)$solved
}

create.ps.tasks = function(ps, opts=rt.opts()) {
  restore.point("create.ps.tasks")
  
  bdf = ps$bdf
  bi = which(bdf$is.task)
  
  # The problem set has no tasks
  if (NROW(bi)==0) {
    ps$has.tasks = FALSE
    ps$org.task.states = NULL
    ps$task.table = NULL
    return()
  }
  
  ps$has.tasks = TRUE
  df = bdf[bdf$is.task,]
  
  
  
  
  task.ind = seq_along(bi)
  n = length(task.ind)
  
  ps$org.task.states = lapply(bi, make.org.task.state, ps=ps)
  names(ps$org.task.states) = as.character(bi)  
  max.points = sapply(seq_along(bi), function(i) {
    if (ps$org.task.states[[i]]$stype == "task_chunk") {
      return(ps$org.task.states[[i]]$ck$max.points)
    } else {
      #max.points = ps$org.task.states[[i]]$ao$max.points
      #if (is.null(max.points)) {
      #  stop("a task ao object has not defined max.points")
      #}
      return(first.non.null(ps$org.task.states[[i]]$ao$max.points,0))
    }
  })
  

  # Find the task that should be activated when
  # the current task is successfully solved
  ptype = paste0("parent_",opts$show.together)
  if (ptype %in% colnames(bdf)) {
    activate.next.task.bi = sapply(bi, function(b) {
      next.bi = which(
        bdf$is.task & 
        bdf$index >b & 
        bdf$parent_note == 0 & 
        bdf[[ptype]] == bdf[[ptype]][b]
      )
      
      if (length(next.bi)==0) return(NA_integer_)
      next.bi[1]
    })
  } else {
    activate.next.task.bi = rep(NA_integer_,n)    
  }
  
  
  
  # Find the required tasks that have to be
  # solved in order to edit the current task
  required.tasks.ind = lapply(bi, function(b) {
    restore.point("unfnfhrbg")
    # earlier tasks from same task line
    req.bi = which(
      bdf$is.task & 
      bdf$index < b & 
      is.true(bdf$task.line == bdf$task.line[b])
    )
    if (length(req.bi)>0) req.bi = max(req.bi)
    
    # earlier tasks from all task.in
    for (ti in bdf$task.in[[b]]) {
      res = which(
        bdf$is.task & 
        bdf$index < b & 
        is.true(bdf$task.line == ti)
      )
      if (length(res)>0) res = max(res)
      req.bi = c(req.bi, res)
    }
    req.bi = sort(req.bi)#
    
    # transform bi to task.ind
    match(req.bi,bi)
  })
  
  task.table = data_frame(
    task.ind = task.ind,
    bi = bi,
    task.line = ps$bdf$task.line[bi],
    task.in = ps$bdf$task.in[bi],
    max.points = max.points,
    award.bi = NA_integer_,
    activate.next.task.ind = match(activate.next.task.bi,bi),
    required.tasks.ind =  required.tasks.ind
  )
  

 
  # Match awards to tasks
  award.bi = which(bdf$type=="award")
  award.task.ind = findInterval(award.bi,bi)
  task.table$award.bi[award.task.ind] = award.bi
  
  ps$task.table = task.table
}

get.ts = function(task.ind=ps$task.ind, app=getApp(), ps=get.ps(), bi=NULL) {
  restore.point("get.ts")
  
  if (!is.null(bi)) task.ind = ps$bdf$task.ind[bi]
  if (is.null(task.ind)) return(NULL)
  ps$task.states[[task.ind]]
}

set.ts = function(task.ind,ts, app=getApp(),ps=get.ps()) {
  ps$task.states[[task.ind]] = ts
}

init.ps.session.task.states = function(ps, ups=get.ups(), app=getApp()) {
  restore.point("init.ps.session.task.states")

  ps$task.states = lapply(ps$org.task.states, init.task.state, ups=ups)
}

init.task.state.without.ups = function(org.ts,obj=NULL,opts=rt.opts(),ps=get.ps()) {
  restore.point("init.task.state.without.ups")
  
  ts = as.environment(as.list(org.ts))
  if (ts$stype=="task_chunk") {
    ts$log = new.env()
  } else {
    Addon = ps$Addons[[ts$stype]]
    Addon$init.task.state(ts, ups=NULL, opts=opts)
  }
  ts
}

init.task.state = function(org.ts,obj=NULL, ups=get.ups(), opts=rt.opts(), ps=get.ps()) {
  if (is.null(ups) | is.null(ups$utt)) return(init.task.state.without.ups(org.ts,obj=obj, opts=opts))
  restore.point("init.task.state")
  
  ts = org.ts
  task.ind = ts$task.ind
  utr = ups$utt[task.ind,]
  if (ts$stype=="task_chunk") {
    restore.point("init.task.state.task.chunk")
    
    ts = as.environment(org.ts)
    ts$solved = utr$was.solved
    ts$points = utr$points
    ts$score = utr$score
    
    if (has.col(utr,"sts")) {
      if (!is.null(utr$sts[[1]]$stud.code)) {
        ts$stud.code = utr$sts$stud.code
      } else {
        if (ts$solved) {
          ts$stud.code = ts$ck$sol.txt
        }
      }
    } else {
      if (ts$solved) {
        ts$stud.code = ts$ck$sol.txt
      }
    }
  } else {
    restore.point("init.task.state.addon")
    ts = as.environment(as.list(org.ts))
    Addon = ps$Addons[[ts$stype]]
    
    ts = Addon$init.task.state(ts, ups=ups,opts=opts)
  }
  return(ts)
}


make.org.task.state = function(bi, ps, opts = rt.opts()) {
  restore.point("make.org.task.state")
  
  task.ind = ps$bdf$task.ind[[bi]]
  stype = ps$bdf$stype[bi]
  
  if (stype == "task_chunk") {
    ck = ps$bdf$obj[[bi]]$ck
    ts = make.user.chunk(ck)
  } else {
    Addon = ps$Addons[[stype]]
    ao = ps$bdf$obj[[bi]]$ao
    
    if (is.null(Addon$make.org.task.state)) {
      ts = new.env()
    } else {
      ts = Addon$make.org.task.state(ao)
    }
    ts$ao = ao
  }
  ts$task.ind = task.ind
  ts$stype = stype
  ts$bi = bi
  
  ts = as.list(ts)
  ts
}

task.solved.give.award = function(ts,ps=get.ps(), ups=get.ups(),...) {
  restore.point("task.solved.give.award")
  
  tt = ps$task.table
  award.bi = tt$award.bi[ts$task.ind]
  if (is.na(award.bi)) return()
  give.award(award.bi, ps=ps)
}


add.task.listener = function(target.bi, listener.bi, ps = get.ps()) {
  restore.point("add.task.listeners")
  stop("not implemented")
  task.listeners = c(ps$bdf$task.listeners[[target.bi]], listener.bi)
  
  ps$bdf$task.listeners[[bi]] = task.listeners
}

call.task.listeners = function(ts, ps = get.ps()) {
  restore.point("call.task.listeners")
  stop("not implemented")
  
  tl =  ps$bdf$task.listeners[[ts$bi]]
  if (length(tl)==0) return()
  for (bi in tl) {
    Ao = get.Addon(bi=bi)
    Ao$listener.handler(target.ts=ts, target.ao=ts$ao, target.bi=ts$bi, ts=ts, bi=bi)
  }
}

process.checked.task = function(ts,ps = get.ps(), ups=get.ups(), save.ups=TRUE,...) {
  restore.point("process.checked.task")

  if (is.null(ups)) {
    if (ts$solved) {
      task.solved.give.award(ts)
      call.plugin.handler("task.checked.handler", ts=ts)
    }
    #call.task.listeners(ts=ts, ps=ps)
    return()
  }
  
  restore.point("process.checked.task2")
  ups$task.ind = ts$task.ind
  utr = ups$utt[ts$task.ind,]
  if (!is.null(ups$utt.dates))
    if (!is.na(ups$utt.dates$first.check.date[ts$task.ind]))
      ups$utt.dates$first.check.date[ts$task.ind] = Sys.time()
  
  # TO DO: REMOVE TRUE
  if (ts$solved & !utr$was.solved) {
    task.solved.give.award(ts)

    ups$utt$was.solved[ts$task.ind] = TRUE
    ups$utt$points[ts$task.ind] = max(ups$utt$points[ts$task.ind], ts$points)
    if (!is.null(ts$score)) {
      if (!is.na(ts$score))
        ups$utt$score[ts$task.ind] = max(c(ups$utt$score[ts$task.ind], ts$score), na.rm=TRUE)
    }
    if (!is.null(ups$utt.dates))
      ups$utt.dates$solved.date[ts$task.ind] = Sys.time()
  } else if (!utr$was.solved) {
    ups$utt$num.failed[ts$task.ind] = ups$utt$num.failed[ts$task.ind]+1
  }
  # set small task state
  if (!is.null(ts[["sts"]])) {
    ups$sts[[ts$task.ind]] = ts$sts
  }
  if (save.ups)
    update.ups(ups)
  
  #call.task.listeners(ts=ts, ps=ps)
  call.plugin.handler("task.checked.handler", ts=ts)

}

process.checked.addon = function(ts, ps = get.ps(), ups=get.ups(),...) {
  restore.point("process.checked.addon")
  process.checked.task(ts)
  
  return()
}

create.bi.task.env.info = function(bi,ps, args=NULL, need.task.env=TRUE, change.task.env=TRUE, optional=FALSE, presolve.task=opts$presolve, opts=rt.opts()) {
  restore.point("create.bi.task.env.info")
  
  if (!need.task.env) return()
  
  bdf = ps$bdf
  
  ps$bdf$need.task.env[bi] = need.task.env
  ps$bdf$change.task.env[bi] = change.task.env
  ps$bdf$presolve.task[bi] = presolve.task
  
  set.task.line(bi,ps,args=args,need.task.env=need.task.env, change.task.env=change.task.env)
  
  
}

task.is.selected = function(ts,ps=get.ps()) {
  ps$task.ind = ts$task.ind
  call.plugin.handler("task.selected.handler")
}


set.task.line = function(bi,ps, args=NULL, opts=rt.opts(), need.task.env=TRUE, change.task.env=TRUE) {
  restore.point("set.task.line")
  res = compute.default.task.line(bi,ps,args,opts, need.task.env=need.task.env, change.task.env=change.task.env)
  
  task.line = first.non.null(args$task.line, res$task.line)
  task.in = first.non.null(args$task.in, res$task.in)
  
  ps$bdf$task.line[bi] = task.line
  ps$bdf$task.in[bi] = list(task.in)
  
  
}

compute.default.task.line = function(bi,ps, args=NULL, opts=rt.opts(), need.task.env=TRUE, change.task.env=TRUE) {
  restore.point("compute.default.task.line")
  br = ps$bdf[bi,]
  
  if (!need.task.env) {
    return(list(task.line = NA, task.in = NULL))
  }
  
  # check if we are a type that starts own task.line
  if (br$stype %in% opts$new.task.line.parts) {
    return(list(task.line = paste0("_",br$id), task.in = NULL))
  }
  
  pbi = get.default.task.line.parent(bi,ps,opts)
  
  # no parent task.line is found
  # create own task.line
  if (is.na(pbi)) {
    return(list(task.line = paste0("_",br$id), task.in = NULL))
  }
  
  # nest parent task.line
  if (br$stype %in% opts$nested.task.line.parts) {
    return(list(task.line = paste0("_",br$id), task.in = ps$bdf$task.line[pbi]))
  }
  
  
  task.line = ps$bdf$task.line[pbi]
  task.in = NULL
  pos = 1
  if (bi > 1) {
    rows  = seq_len(bi-1)
    pos = sum(ps$bdf$task.line[seq_len(bi-1)] == task.line & ps$bdf$is.task[rows], na.rm = TRUE)+1
  }
  if (pos == 1) {
     task.in = ps$bdf$task.in[[pbi]]    
  } 
  
  # an optional chunk is like a single nested chunk
  if (isTRUE(args$optional) | !change.task.env) {
    if (pos == 1) {
      return(list(task.line = paste0("_",br$id), task.in = task.in))
    } else {
      return(list(task.line = paste0("_",br$id), task.in = ps$bdf$task.line[pbi]))
    }
  }

  
  # just continue parent task.line 
  return(list(task.line = task.line, task.in = task.in))
}

get.default.task.line.parent = function(bi, ps, opts=rt.opts()) {
  restore.point("get.default.task.line.parent")
  for (type in c(opts$nested.task.line.parts, opts$new.task.line.parts)) {
    col = paste0("parent_",type)
    if (isTRUE(ps$bdf[[col]][bi] > 0)) {
      return(ps$bdf[[col]][bi])
    }
  }
  return(NA)
}
