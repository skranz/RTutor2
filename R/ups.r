# Store user specific information about the problem set solution
#
# Always saved:
#
# user.name
# ps.name
#
# utt (ups task table, a data_frame)
#   is.solved
#   was.solved
#   num.failed
#   points
#   score
#   num.hints
#
# awards (integer vector of given awards, negative numbers stand for manual
#         awards that will be saved in a file .awards.ups)
#
# utt.times (task time table, optional)
#   first.run.date 
#   solved.date
#
# sts: (environment with small task states)
#
# bts: big task states will be stored in separate files
# with endings like .t7.ups where the 7 stands for the
# task.ind

#' Specify which information will be automatically saved in ups
default.ups.save = function(
    chunks = TRUE,
    awards = TRUE,
    addons = TRUE,
    code = FALSE | all,
    chunk.ind = FALSE | all,
    all = FALSE
) {
  nlist(
    chunks,
    awards,
    addons,
    code,
    chunk.ind
  )
}


get.user.name = function(ps=get.ps()) {
  ps$user.name
}

init.ups = function(user.name=ps$user.name, ps = get.ps(), opts=rt.opts()) {
  restore.point("init.ups")

  # current task, will be shown when reloaded
  task.ind = NULL

  # task table
  n = NROW(ps$tt)
  
  utt = data_frame(
    is.solved = rep(FALSE,n),
    was.solved = FALSE,
    points = 0L,
    score = 0,
    num.failed = 0L,
    num.hints = 0L
  )
  
  utt.dates = data.frame(
    first.run.date = rep(as.POSIXct(NA),n),
    solved.date = as.POSIXct(NA)
  )
  
  # small task state: take initial sts from ps
  sts = ps$tt$init.sts
  
  # awards, a vector of integers
  awards = NULL

  ups = as.environment(list(ps.name=ps$name, user.name=user.name, task.ind = task.ind, utt=utt, utt.dates=utt.dates, sts=sts, awards = awards))

  save.ups(ups=ups,ps=ps)
  ups
}

ups.init.shiny.ps = function(ps=get.ps(), ups=get.ups(), rerun=FALSE, sample.solution=FALSE, precomp=isTRUE(ps$precomp), replace.sol = isTRUE(ps$replace.sol), ups.save=ps$ups.save) {
  restore.point("init.shiny.ps.from.ups")
  
  if (NROW(ps$cdt)==0) return()

  chunk.ind = ups$chunk.ind
  if (is.null(chunk.ind)) {
    chunk.ind = 1
  }
  
  is.solved = rep(FALSE, NROW(ps$cdt))
  ps$cdt$mode = "output"
  ps$cdt$mode[chunk.ind] = "input"
  
  if (!sample.solution) {
    if (!is.null(ups$cu$stud.code) & !sample.solution) {
      ps$cdt$stud.code = ups$stud.code
    } else if (!sample.solution) {
      ps$cdt$stud.code = ps$cdt$shown.txt
    }
    if (!is.null(ups$cu$solved)) {
      is.solved = ups$cu$solved
    } else {
      is.solved =  rep(FALSE, NROW(ps$cdt))
    }
  } else {
    ps$cdt$stud.code = ps$cdt$sol.txt
    is.solved = rep(TRUE, NROW(ps$cdt))
  }
  if (ups.save$code & is.null(ups$cu$stud.code)) {
    ups$cu$stud.code = ps$cdt$stud.code
  }
  
  if (rerun) {
    ps$cdt$is.solved = is.solved
    rerun.solved.chunks(ps)
    ps$cdt$mode[1] = "output"
  } else if (precomp) {
    ps$cdt$is.solved = is.solved
  } else {
    ps$cdt$is.solved = rep(FALSE, NROW(ps$cdt))
  }

  if (replace.sol) {
    ps$cdt$stud.code[ps$cdt$is.solved] = ps$cdt$sol.txt[ps$cdt$is.solved]
  }
  
}

get.ups = function() {
  ps = get.ps()
  ps[["ups"]]
}

set.ups = function(ups) {
  ps = get.ps()
  ps[["ups"]] = ups
}


load.ups = function(user.name, ps.name = ps$name, ps = get.ps(),...) {
  restore.point("load.ups")
  
  dir = get.ps()$ups.dir
  file = paste0(dir,"/",user.name,"_",ps$name,".ups")
  
  if (is.null(user.name)) stop("user.name is NULL. This is not allowed")

  if (nchar(user.name)==0)
    return(NULL)
  
  
  if (!file.exists(file)) {
    ups = init.ups(user.name = user.name, ps=ps,...)
  } else {
    load(file=file)
  }
  return(ups)
}

# need to rewrite
update.ups = function(ups = get.ups(), ps=get.ps(), task.ind=NULL, addon=NULL, award=NULL,task=NULL, hint=NULL, ups.save = ps$ups.save) {
  restore.point("update.ups")
  
  if (!is.null(task.ind)) {
    ups$task.ind = task.ind
  }
  save.ups(ups=ups,ps=ps)
}

save.ups = function(ups = get.ups(), ps=get.ps()) {
  restore.point("save.ups")
  
  if (isTRUE(ps$save.nothing)) return()

  if (is.null(ups$chunk.ind))
    ups$chunk.ind = ps$chunk.ind
  
  dir = ps$ups.dir
  file = paste0(dir,"/",ups$user.name,"_",ps$name,".ups")
  #cat("save ups to file: ", file)
  
  suppressWarnings(save(ups,file=file))
}

# remove old ups files when new problem set structure is generated
remove.ups = function(ps.name = get.ps()$name, dir = get.ps()$ups.dir) {
  set.ups(NULL)

  if (is.null(dir)) dir =getwd()

  files = list.files(path = dir,full.names = TRUE)
  files = files[str.ends.with(files,paste0("_",ps.name,".ups"))]
  if (length(files)>0) {
    file.remove(files)
  }
}

