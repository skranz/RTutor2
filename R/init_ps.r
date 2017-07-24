get.or.init.rmd.ps = function(ps.name, user.name, dir, stud.short.file = paste0(ps.name,".Rmd"), reset=FALSE, ps=get.ps(), ups.dir = dir, user.dir=ups.dir) {
  restore.point("get.or.init.ps")

  # Just take current problem set information
  if (!is.null(ps) & !reset) {
    if (isTRUE(ps$ps.name == ps.name & ps$stud.path==dir & ps$stud.short.file == stud.short.file & ps$user.name == user.name & isTRUE(ps$is.initialzed))) {
      return(ps)
    }
  }

  # Initialize problem set newly
  ps = init.rmd.ps(ps.name=ps.name,user.name=user.name,dir=dir,stud.short.file=stud.short.file, ups.dir = ups.dir, user.dir=user.dir)
  ps
}


#' Initialize a problem set for the student
#' @param ps.name the name of the problem set
#' @param dir the path in which the stud has stored his file
#' @param stud.hort.file the file name (without path) of the .rmd problem set file
#' @export
init.rmd.ps = function(ps.name,user.name="", dir=getwd(), stud.short.file = paste0(ps.name,".Rmd"), rps.file = paste0(dir,"/",ps.name,".rps"),log.file = paste0(dir,"/",ps.name,".log"), rps.dir=dir, ups.dir=dir, user.dir=ups.dir) {
  restore.point("init.rmd.ps")

  ps = read.rps(file=rps.file)
  set.ps(ps)
  ps$ups.dir = ups.dir
  ps$user.dir = user.dir
  ps$user.name = user.name

  set.rt.opts(ps$opts)
  
  ps$stud.path = dir
  stud.file=paste0(dir,"/",stud.short.file)
  ps$stud.file = stud.file
  ps$stud.short.file = stud.short.file
  ps$log.file = log.file
    
  load.ps.libs(ps$opts$libs, need.all.libs=!isTRUE(ps$opts$need.all.libs==FALSE))

  if (isTRUE(ps$opts$use.memoise)) {
    copy.into.env(dest=ps$init.env, source=ps$memoise.fun.li)
  }
  parent.env(ps$init.env) = parent.env(globalenv())

  ps$ups = load.ups(user.name = user.name, ps=ps)
  # init user state of chunks
  init.ps.session.task.states(ps=ps, ups=ps$ups)
  
  
  log.event(type="init_rmd_ps")

  ps$is.initialzed = TRUE  
  return(ps)
}


load.ps.libs = function(libs, verbose=FALSE, need.all.libs=TRUE) {
  if (length(libs)==0)
    return()
  num.missing = 0
  for (i in seq_along(libs)) {
    lib = libs[i]
    if (verbose)
      display("load package ", lib, "...")
    ret = suppressWarnings(require(lib, quietly=TRUE, warn.conflicts =FALSE,character.only=TRUE))
    if (!ret) {
      if (need.all.libs) {
        stop(paste0("Please install the required package '", lib,"'."))
      } else {
        num.missing = num.missing+1
        warning(paste0("Warning could not find package '", lib,"'."))
      }
    }
    if (verbose) {
      if (num.missing==0) {
        display("... all required packages loaded.")
      } else {
        display(num.missing, " required packages were not found.")
      }
    }

  }
}


