#' Grade your problem set and make submission file
#' 
#' The command will rerun and check all chunks of your problem set and grade it, i.e. it determines which tests are passed or not. The results are stored in a submission file: psname___username.sub, which will be part of the submitted solution. The function works similarly than check.problem.set, but makes sure that all exercies are checked.
#'@export
make.submission = function(ps = get.ps(),from.knitr=!interactive(),...) {

  restore.point("make.submission")

  if (is.null(ps)) {
    cat("Please check your problem set once before you make the submission.")
    return()
  }
  if (from.knitr) {
    cat("Cannot grade when called from knitr")
    return()
  }
  check.problem.set(ps.name = ps$ps.name, stud.path=ps$stud.path, stud.short.file = ps$stud.short.file, reset=TRUE,..., for.submission=TRUE)

  restore.point("make.submission2")
    
  display("\n****************************************************")
  stat = stats()

  ups = get.ups()
  by.task = cbind(data_frame(ps.name=ps.name, user.name=user.name),select(ps$task.table, task.ind, bi, max.points),ups$utt, ups$utt.dates)
  
  by.part = cbind(data_frame(ps.name=ps.name, user.name=user.name),stat)
  
  sub = as.list(ups)
  sub$by.task = by.task
  sub$by.part = stat
  sub$rmd.code = ps$stud.code 
  sub$grade.time = Sys.time()
  sub$rtutor.version = packageVersion("RTutor3")
  
  sub$hash = digest::digest(list(sub$user.name,sub$ps.name,sub$grade.time,sub$total))
  
  try(sub$log.txt <- readLines(ps$log.file))
  try(sub$log.df <- import.log(txt=sub$log.txt))
  #object.size(sub)
  
  sub.file = paste0(sub$ps.name,"__",sub$user.name,".sub")
  sub = as.environment(sub)
  save(sub,file=sub.file)
 
  cat(paste0("\nI created the submission file '", sub.file,"'"))
  
  invisible(sub) 
}


load.submission = function(file) {
  load(file)
  return(sub)
}

