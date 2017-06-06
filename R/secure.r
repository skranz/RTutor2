secure.base.env = function(ps=get.ps()) {
  env = new.env(parent=globalenv())
  env$get.ps = function(...) {
    ps
  }
  env
}

rtutor.eval.secure = function(..., envir=parent.frame(), timeout = ps$secure.eval.timeout, profile=ps$secure.eval.profile, ps=get.ps(), silent.check=TRUE, check.app.armor=FALSE) {

  if (check.app.armor) {
    # check if AppArmor is running. Otherwise don't eval 
    con =  try(RAppArmor::aa_getcon(verbose=!silent.check), silent = TRUE)
    check = is(con,"try-error")
    if (!check) check = !identical(con$mode,"enforce")
    if (check) {
      if (!RAppArmor::aa_is_enabled(verbose=!silent.check))
        stop("AppArmor is not enabled. User input is not evaluated.")
    }
  }
   
   
  #restore.point("rtutor.eval.secure")
  RAppArmor::eval.secure(...,envir=envir, timeout=timeout, profile=profile)
}

