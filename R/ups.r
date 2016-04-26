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
#   first.check.date 
#   solved.date
#
# sts: (environment with small task states)
#
# bts: big task states will be stored in separate files
# with endings like .t7.ups where the 7 stands for the
# task.ind

init.ups = function(user.name=ps$user.name, nick=user.name, user.id=user.name, ps = get.ps(), opts=rt.opts()) {
  restore.point("init.ups")

  # current task, will be shown when reloaded
  task.ind = NULL

  # task table
  
  n = NROW(ps$task.table)
  
  # problem set has no tasks
  if (n==0) {
    ups = as.environment(list(ps.name=ps$ps.name, user.name=user.name, nick=nick, task.ind = NULL, utt=NULL, utt.dates=NULL, sts=NULL))

    save.ups(ups=ups,ps=ps)
    return(ups)
  }
  
  utt = data_frame(
    was.solved = rep(FALSE,n),
    points = 0L,
    score = 0,
    num.failed = 0L,
    num.hints = 0L
  )
  
  utt.dates = data.frame(
    first.check.date = rep(as.POSIXct(NA),n),
    solved.date = as.POSIXct(NA)
  )
  
  # small task states, can be used to init 
  # addons from ups
  sts = vector("list",n)
  
  ups = as.environment(list(ps.name=ps$ps.name, user.name=user.name, nick=nick,user.id=user.id, task.ind = task.ind, utt=utt, utt.dates=utt.dates, sts=sts))

  save.ups(ups=ups,ps=ps)
  ups
}

get.ups = function() {
  ps = get.ps()
  ups = ps[["ups"]]
  if (is.null(ups)) {
    return(get(".__rtutor_ups", ups, .GlobalEnv))
  }
  ups
}

set.ups = function(ups) {
  ps = get.ps()
  ps[["ups"]] = ups
  assign(".__rtutor_ups", ups, .GlobalEnv)
}

get.user.name = function() {
  get.ups()$user.name
}

load.ups = function(user.name, nick=user.name, user.id=user.name, ps = get.ps(), dir=ps$ups.dir,...) {
  restore.point("load.ups")
  
  file = paste0(dir,"/",ps$ps.name,"_",user.id,".ups")
  if (is.null(user.id)) stop("user.id is NULL. This is not allowed")

  if (nchar(user.id)==0)
    return(NULL)
  
  
  if (!file.exists(file)) {
    ups = init.ups(user.name = user.name, nick=nick,user.id=user.id, ps=ps,...)
  } else {
    load(file=file)
  }
  set.ups(ups)
  return(ups)
}

# need to rewrite
update.ups = function(ups = get.ups(), ps=get.ps(), task.ind=NULL, addon=NULL, award=NULL,task=NULL, hint=NULL,...) {
  restore.point("update.ups")
  
  if (!is.null(task.ind)) {
    ups$task.ind = task.ind
  }
  save.ups(ups=ups,ps=ps)
}

save.ups = function(ups = get.ups(), ps=get.ps(), opts=rt.opts(), dir=ps$ups.dir) {
  restore.point("save.ups")
  
  if (isTRUE(opts$save.nothing)) return()
  
  file = paste0(dir,"/",ps$ps.name,"_",ups$user.id,".ups")
  suppressWarnings(save(ups,file=file))
}

# remove old ups files when new problem set structure is generated
remove.existing.ups = function(ps.name = get.ps()$ps.name, dir = get.ps()$ups.dir) {
  restore.point("remove.existing.ups")
  
  set.ups(NULL)

  if (is.null(dir)) dir =getwd()

  files = list.files(path = dir, pattern=glob2rx("*.ups"),full.names = TRUE)
  files = files[has.substr(files,paste0(ps.name,"_")) & str.ends.with(files,".ups")]
  if (length(files)>0) {
    file.remove(files)
    cat("\n",length(files),"old .ups files removed.\n")
  }
}

