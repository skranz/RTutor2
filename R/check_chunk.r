
empty.log = function() {
  new.env()
}

#' Check a chunk described by uk
#' @param uk the user chunk object; an environment that will be adapted
#' @details Returns the modified uk with all information from the check. uk$passed denotes whether all checks where passed or not. Note that the uk object and only the uk object will be modified to store all relevant information from the check. Saving ups or giving awards must be separately performed afterwards
#' @export
check.chunk = function(uk, stud.code=uk$stud.code, task.env=make.fresh.task.env(ts=uk), opts=rt.opts(), log=empty.log(), expect.change = FALSE, store.output=TRUE, noeval = opts$noeval, precomp=opts$precomp,verbose=isTRUE(opts$verbose), use.secure.eval = opts$use.secure.eval, save.ups = TRUE) {
  restore.point("check.chunk")

  opts$noeval = noeval
  uk$log = log
  ck = uk$ck
  uk$passed = FALSE
  chunk.ind = ck$chunk.ind
  chunk.name = ck$chunk.name
  uk$last.check.code = stud.code
  
  if (is.null(stud.code)) {
    log$failure.message=paste0("stud.code is NULL")
    return(FALSE)
  }
  
  if (expect.change)  {
    if (stud.code == ck$shown.txt) {
      uk$chunk.changed = FALSE
      log$failure.message = paste0("You have not yet changed chunk ", chunk.name)
      return(TRUE)
    }
  }
  if (verbose)
    display("Check chunk ", chunk.name," ...")
  #stop("analyse below")
  log$success.log = log$test.log = NULL
  log$failure.message  = "No failure message recorded"
  log$warning.messages = list()
  log$check.date = Sys.time()

  uk$has.error = uk$had.warning = has.error = FALSE
  uk$stud.expr.li = NULL
  uk$task.env = task.env
  

  if (verbose) {
    display("parse stud.code...")
  }
  if (!is.false(opts$catch.errors)) {
    tryCatch( uk$stud.expr.li <- base::parse(text=stud.code, srcfile=NULL),
              error = function(e) {
                log$failure.message=paste0("parser error: ",geterrmessage())
                has.error <<- TRUE
              })
  } else {
    uk$stud.expr.li <- base::parse(text=stud.code, srcfile=NULL)
  }
  uk$has.error =  has.error
  if (has.error) {
    return(FALSE)  
  }


  if (isTRUE(opts$check.whitelist)) {
    if (verbose)
      display("check whitelist")
    res = rtutor.check.whitelist(uk$stud.expr.li,uk=uk)
    if (!res$ok) {
      log$failure.message=paste0("security error: ",res$msg)
      uk$has.error = TRUE
      return(FALSE)
    }
  }

  if (verbose) {
    display("make.chunk.task.env...")
  }
  has.error = FALSE
  uk$stud.seed = as.integer(Sys.time())
  set.seed(uk$stud.seed)

  
  # Turn graphics device off
  if (isTRUE(opts$use.null.device)) {
    try(png("NUL"), silent=TRUE)
    on.exit(try(dev.off(), silent=TRUE),add = TRUE)
  }

  if (verbose) {
    display("eval stud.code...")
  }
  # relevant for hint
  uk$e.ind = 0

  res = check.chunk.eval.part(uk=uk, log=log, task.env=task.env,opts=opts, store.output=store.output, verbose=verbose)
  
  uk$solved = uk$passed
  uk$points = uk$ck$max.points * uk$solved
  process.checked.task(uk, save.ups = save.ups)
  if (uk$passed) {
    log.event(type="check_chunk",chunk.ind=ck$chunk.ind, e.ind=0,code=stud.code, ok=TRUE,message="")
    if (isTRUE(uk$had.warning)) {
      return("warning")
    } else {
      return(TRUE)
    }
  } else {
    log.event(type="check_chunk",chunk.ind=ck$chunk.ind, e.ind=uk$e.ind,code=stud.code, ok=FALSE,message=log$failure.message)
    return(uk$passed)
  }

}

secure.check.chunk.eval.part = function(uk,log, task.env, opts, store.output,verbose=FALSE) {
  
  secure.fun = function(uk,log, task.env, opts, store.output,verbose=FALSE) {
    ok = check.chunk.eval.part(uk,log, task.env, opts, store.output,verbose=FALSE)
    
    class(task.env) = "environment"
    res = list(
      ok = as.logical(ok),
      success.message=as.character(log$success.message),
      failure.message=as.character(log$failure.message),
      success.log=as.character(log$success.log),
      chunk.console.out = as.character(log$chunk.console.out),
      e.ind = as.integer(uk$e.ind),
      passed = as.logical(uk$passed),
      points= as.numeric(uk$points),
      
      # BEWARE: task.env can be toxic. 
      # Objects inside may be redefined...
      # Also functions may be included inside...
      # Never perform any evaluation using task.env
      # not wrapped by RAppArmor!
      task.env=task.env
    )
    return(res)
  }
  res = rtutor.eval.secure(secure.fun(uk,log, task.env, opts, store.output,verbose))
  
  log$success.message=res$success.message
  log$failure.message=res$failure.message
  log$success.log=res$success.log
  log$chunk.console.out = res$chunk.console.out
  
  uk$e.ind = uk$e.ind
  uk$passed = uk$passed
  uk$points= uk$points

  copy.into.env(dest=task.env, source=res$task.env)
  return(res$ok)
}

# the part of check chunk that performs evaluations of student code
# we put in a separate function in order to easier wrap it inside a 
# secure.eval call when RAppArmor is used.
check.chunk.eval.part = function(uk,log, task.env, opts, store.output,verbose=FALSE) {
  restore.point("check.chunk.eval.part")
  ck = uk$ck
  # run student code in student.env
  if (!opts$noeval) {
    # We may not store output for speed reasons
    # storing output slows down checking of chunk
    # if large data frame is shown.
    if (!store.output) log$chunk.console.out=""
    has.error = !stepwise.eval.stud.expr(stud.expr=uk$stud.expr.li,task.env=task.env, log=log, store.output=store.output)
    if (has.error) {
      uk$has.error = TRUE
      return(FALSE)
    }
  }

  # no tests
  if (length(ck$test.expr)==0) {
    log$success.message = ""
    uk$passed = uk$solved = TRUE
    uk$points = uk$ck$max.points
    return(TRUE)
  }

  
  uk$had.warning = FALSE
  if (verbose) {
    display("run tests...")
  }


  e.ind = 1
  #test.env = make.base.env()
  #test.env$ck = ck
  #test.env$opts = opts
  for (e.ind in seq_along(ck$e.li)) {
    uk$e.ind = e.ind
    tests = ck$test.expr[[e.ind]]
    test.ind = 1
    for (test.ind in seq_along(tests)){
      uk$test.ind = test.ind
      test = tests[[test.ind]]
      log$success.message = NULL
      passed.before = uk$test.passed[test.ind]
      if (verbose) {
        display("  Test #", test.ind, ": ",deparse1(test))
      }
      
      # note that tests draw ck and opts from the parent.frame
      ret = eval(test)
      uk$test.passed[test.ind] = ret

      # test failed
      if (ret==FALSE) {
        log.event(type="check_chunk",chunk.ind=ck$chunk.ind, e.ind=e.ind,code=uk$stud.code, ok=FALSE,message=log$failure.message)
        log$test.log = c(log$test.log, log$failure.message)
        return(uk$passed)
      } else if (ret=="warning") {
        uk$had.warning = TRUE
        log$test.log = c(log$test.log, log$warning.message)
      } else {
        log$test.log = c(log$test.log, log$success.message)
        if (!is.null(log$success.message) & !passed.before) {
          log$success.log = c(log$success.log,log$success.message)
          cat(paste0(log$success.message,"\n"))
        }
      }
    }
  }
  uk$passed = TRUE
  uk$solved = TRUE


  return(TRUE)
}



update.log.test.result = function(...) {
  return()
}

stepwise.eval.stud.expr = function(stud.expr, task.env = uk$task.env, log=uk$log, uk=NULL, seed=NULL, store.output = TRUE, source=NULL, opts=rt.opts()) {
  restore.point("stepwise.eval.stud.expr")
  if (!is.null(seed))
    set.seed(seed)
  has.error = FALSE

  err.fun = function(e) {
    log$failure.message = paste0("evaluation error in \n  ",
        deparse1(part.expr),"\n  ",adapt.console.err.message(as.character(e)))
    has.error <<- TRUE
  }

  if (store.output) {
    log$chunk.console.out = ""
    add = function(...) {
      str = paste0(..., collapse="\n")
      if (length(str)>0)
        log$chunk.console.out = paste0(log$chunk.console.out,str, sep="\n")
    }
  }

  i = 1
  for (i in seq_along(stud.expr)) {
    part.expr = stud.expr[[i]]

    if (!store.output) {
      tryCatch( eval(part.expr, task.env),error = err.fun)
    } else {
      if (is.null(source)) {
        add("> ",deparse1(part.expr, collapse="\n+"))
      } else {
        add("> ",paste0(li$source[[i]], collapse="\n+ "))
      }
      out = NULL
      tryCatch(out <- capture.output(eval(part.expr, task.env)),error = err.fun)
      if (length(out)>0) add(out)
    }
    if (has.error) {
      if (is.false(opts$catch.errors))
        stop(log$failure.message)

      return(FALSE)
    }
  }
  #cat(log$chunk.console.out)
  return(!has.error)
}


adapt.console.err.message = function(str) {
  if (str.starts.with(str,"Error in eval(")) {
    str = paste0("Error: ",str.right.of(str,":"))
  }
  str
}

#' Used inside tests: adds a failure to an exercise
#'
#' @param message a longer description shown to the user
#' @param ... variables that will be rendered into messages that have whiskers
#' @export
add.failure = function(log,message,..., ps= get.ps()) {
  args = list(...)
  message=replace.whiskers(message,args, eval=FALSE)
  restore.point("add.failure")
  log$failure.message = message
}

#' Used inside tests: adds a sucess message
#'
#' @param message a longer description shown to the user
#' @param ... variables that will be rendered into messages that have whiskers
#' @export
add.success = function(log,message,...) {
  args = list(...)
  message=replace.whiskers(message,args, eval=FALSE)
  log$success.message = message
}


#' Used inside tests: adds a warning
#'
#' @param message a longer description shown to the user
#' @param ... variables that will be rendered into messages that have whiskers
#' @export
add.warning = function(log,message,...) {
  args = list(...)
  message=replace.whiskers(message,args, eval=FALSE)
  #restore.point("add.warning")
  ind = length(log$warning.messages)+1
  log$warning.messages[[ind]] = message
}


