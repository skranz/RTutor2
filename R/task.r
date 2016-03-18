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



create.ps.tasks = function(ps, opts=rt.opts()) {
  restore.point("create.ps.tasks")
  
  bdf = ps$bdf
  bi = which(bdf$is.task)
  df = bdf[bdf$is.task,]
  
  
  task.ind = seq_along(bi)
  n = length(task.ind)
  
  ps$org.task.states = lapply(bi, make.org.task.state, ps=ps)
  names(ps$org.task.states) = as.character(bi)  
  max.points = sapply(seq_along(bi), function(i) {
    if (ps$org.task.states[[i]]$stype == "task_chunk") {
      return(ps$org.task.states[[i]]$ck$max.points)
    } else {
      return(ps$org.task.states[[i]]$ao$max.points)
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
  required.task.bi = sapply(bi, function(b) {
    req.bi = which(
      bdf$is.task & 
      bdf$index < b & 
      is.true(bdf$task.env.line == bdf$task.env.line[b])
    )
    if (length(req.bi)==0) return(NA_integer_)
    req.bi[1]
  })
  
  task.table = data_frame(
    task.ind = task.ind,
    bi = bi,
    max.points = max.points,
    award.bi = NA_integer_,
    activate.next.task.ind = match(activate.next.task.bi,bi),
    required.task.ind =  match(required.task.bi,bi)
  )
  

 
  # Match awards to tasks
  award.bi = which(bdf$type=="award")
  award.task.ind = findInterval(award.bi,bi)
  task.table$award.bi[award.task.ind] = award.bi
  
  ps$task.table = task.table
}

get.task.env = function(task.ind=ps$task.ind, app=getApp(), ps=get.ps()) {
  ts = get.ts(task.ind)
  if (ts$stype=="task_chunk") return(ts$task.env)
  return(NULL)
}

get.ts = function(task.ind=ps$task.ind, app=getApp(), ps=get.ps()) {
  app$task.states[[task.ind]]
}

set.ts = function(task.ind,ts, app=getApp()) {
  app$task.states[[task.ind]] = ts
}

init.ps.session.task.states = function(ps, ups=get.ups(), app=getApp()) {
  restore.point("init.ps.session.task.states")

  app$task.states = lapply(ps$org.task.states, init.task.state.with.ups, ups=ups)
}

init.task.state.without.ups = function(org.ts,obj=NULL,opts=rt.opts()) {
  restore.point("init.task.state.without.ups")
  
  ts = as.environment(as.list(org.ts))
  if (ts$stype=="task_chunk") {
    ts$log = new.env()
  } else {
    Addon = ps$Addons[[ts$stype]]
    Addon$init.task.state.without.ups(ts, opts=opts)
  }
  ts
}

init.task.state.with.ups = function(org.ts,obj=NULL, ups=get.ups(), opts=rt.opts()) {
  if (is.null(ups) | is.null(ups$utt)) return(init.task.state.without.ups(org.ts,obj=obj, opts=opts))
  restore.point("init.task.state.with.ups")
  
  ts = org.ts
  task.ind = ts$task.ind
  utr = ups$utt[task.ind,]
  if (ts$stype=="task_chunk") {
    restore.point("init.task.state.with.ups.task.chunk")
    
    ts = as.environment(org.ts)
    ts$solved = utr$was.solved
    ts$points = utr$point
    ts$score = utr$score
    if (!is.null(utr$sts[[1]]$stud.code)) {
      ts$stud.code = utr$sts$stud.code
    } else {
      # only overwrite if solved
      if (ts$solved) {
        ts$stud.code = ts$ck$sol.txt
      }
    } 
  } else {
    restore.point("init.task.state.with.ups.addon")
    ts = as.environment(as.list(org.ts))
    Addon = ps$Addons[[ts$stype]]
    ts = Addon$init.task.state.with.ups(ts, ups=ups,opts=opts)
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
    ts = Addon$make.org.task.state(ao)
    ts$ao = ao
  }
  ts$task.ind = task.ind
  ts$stype = stype
  
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

process.checked.task = function(ts,ps = get.ps(), ups=get.ups(),...) {
  restore.point("process.checked.task")

   
  if (is.null(ups)) {
    if (ts$solved) {
      task.solved.give.award(ts)
      call.plugin.handler("task.checked.handler", ts=ts)
   }
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
  update.ups(ups)
  
  call.plugin.handler("task.checked.handler", ts=ts)

}

process.checked.addon = function(ts, ps = get.ps(), ups=get.ups(),...) {
  restore.point("process.checked.addon")
  process.checked.task(ts)
  return()
}


create.bi.task.env.info = function(bi,ps, need.task.env=TRUE, change.task.env=TRUE, optional=FALSE, precomp.task.env=opts$precomp, opts=rt.opts()) {
  restore.point("create.bi.task.env.info")
  
  if (!need.task.env) return()
  
  bdf = ps$bdf
  
  # determine task.env.line
  line.type = opts$task.env.together
  line.bi = bdf[[paste0("parent_",line.type)]][bi]

  task.env.line = paste0(line.type,"_",line.bi)
  if (need.task.env) {
    task.start.env.bi = which(bdf$task.env.line==task.env.line & bdf$change.task.env)[1]      
  } else {
    task.start.env.bi = NA_integer_
  }
  if (optional) {
    task.env.line = paste0("opt_",bi)
  }
  
  ps$bdf$task.env.line[bi] = task.env.line
  ps$bdf$need.task.env[bi] = need.task.env
  ps$bdf$change.task.env[bi] = change.task.env
  ps$bdf$task.start.env.bi[bi] = task.start.env.bi
  ps$bdf$precomp.task.env[bi] = precomp.task.env
}
