# Functions for checking student solutions
# Tests are implemented in in tests_for_ps.r

#' Checks a student problem set
#'
#' The command will be put at the top of a student's problem set. It checks all exercises when the problem set is sourced. If something is wrong, an error is thrown and no more commands will be sourced.
#'@export
check.problem.set = function(ps.name,stud.path=getwd(), stud.short.file, reset=FALSE, set.warning.1=TRUE, user.name=NULL, do.check=interactive(), verbose=FALSE, catch.errors=TRUE, from.knitr=!interactive(), use.null.device=TRUE, just.init=FALSE, for.submission=FALSE, stud.code = NULL) {

  restore.point("check.problem.set")

  if (from.knitr) {
    # Allows knitting to HTML even when there are errors
    knitr::opts_chunk$set(error = TRUE)
    ps = NULL
    try(ps <- get.or.init.rmd.ps(ps.name,user.name, stud.path, stud.short.file, reset), silent=TRUE)

    # Copy extra code into globalenv
    if (!is.null(ps$init.env)) {
      copy.into.env(source=ps$init.env, dest = globalenv())
    }
    return()
  }

  # If called from knitr, I don't want to check by default
  if (!do.check) return("not checked")

  if (set.warning.1) {
    if (options()$warn<1)
      options(warn=1)
  }
  if (!isTRUE(try(file.exists(stud.path),silent = TRUE))) {
    str= paste0("I could not find your problem set directory '", stud.path,"'.")
    stop(str,call. = FALSE)
  }
  if (!file.exists(paste0(stud.path,"/", stud.short.file))) {
    str= paste0("I could not find your file '", stud.short.file,"' in your problem set folder '",stud.path,"'.")
    stop(str,call. = FALSE)
  }

  setwd(stud.path)

  if (is.null(stud.code))
    stud.code = readLines(stud.short.file)
  
  if (is.null(user.name)) {
    user.name = get.user.name.from.rmd(stud.code)
  }
  
  if (user.name=="ENTER YOUR USERNAME HERE") {
    stop('You have not picked a user name. Change the text "ENTER YOUR USERNAME HERE" at the beginning of your Rmd file to some username that you can freely pick.',call. = FALSE)
  }

  if (verbose)
    display("get.or.init.ps...")

  ps = get.or.init.rmd.ps(ps.name,user.name,stud.path, stud.short.file, reset)
  log.event(type="check_ps")

  ps$catch.errors = catch.errors
  ps$use.null.device = use.null.device

  set.ps(ps)
  ps$stud.code = stud.code
  rmc = ps$rmc
  if (!has.col(rmc,"successfully.run")) {
    rmc$successfully.run = FALSE
  } 
  if (!has.col(rmc,"old.stud.code")) {
    rmc$old.stud.code = rmc$shown.code
  } 

  rmc$stud.code = get.stud.chunk.code(ps=ps)
  rmc$code.as.shown = rmc$stud.code == rmc$shown.code
  rmc$chunk.changed = rmc$stud.code != rmc$old.stud.code
  rmc$old.stud.code = rmc$stud.code


  if (!any(rmc$chunk.changed) & !for.submission) {
    code.change.message = "\nBTW: I see no changes in your code... did you forget to save your file?"
  } else {
    code.change.message = NULL
  }

  ps$rmc = rmc

  if (just.init) return(invisible())

  
  # Check chunks
  check.chunks = get.check.chunks(ps, for.submission=for.submission)
  #check.chunks = c(check.chunks, setdiff(ps$rmc$chunk.ind, check.chunks))

  for (chunk.ind in check.chunks) {
    res = check.chunk.in.rstudio(chunk.ind=chunk.ind, ps=ps, verbose=verbose)
    if (!res) {
      if (!for.submission) {
        save.ups()
        return()
      }
    }
  }
  
  save.ups()
  if (for.submission) return()
  
  display("\n****************************************************")
  stats()
  display("You solved the problem set. Congrats!")
}

check.chunk.in.rstudio = function(chunk.ind, ps, verbose=TRUE) {
  restore.point("check.chunk.in.rstudio")
  
  log = empty.log()  
  rmc = ps$rmc
  i = chunk.ind
  ps$prev.checked.chunk = i
  bi = rmc$bi[i]
  task.ind = rmc$task.ind[i]
  ps$task.ind = task.ind
  
  task.env = make.fresh.task.env(task.ind)
  uk = get.ts(task.ind=task.ind)
  stud.code = rmc$stud.code[[i]]
  
  
  ret <- FALSE
  
  display("Check chunk ", rmc$chunk.name[i]  ,"...", end.char="")

  if (!is.false(ps$catch.errors)) {
    ret = tryCatch(check.chunk(uk = uk,log=log,task.env = task.env ,stud.code = stud.code ,opts = ps$opts,use.secure.eval = FALSE, save.ups=FALSE),
      error = function(e) {
        log$failure.message <- as.character(e);
        return(FALSE)
      }
    )
  } else {
    ret = check.chunk(uk = uk,log=log, task.env = task.env ,stud.code = stud.code ,use.secure.eval = FALSE, opts=ps$opts, save.ups=FALSE)
  }
  
  # Copy variables into global env
  copy.into.envir(source=task.env,dest=.GlobalEnv, set.fun.env.to.dest=TRUE)
  
  #save.ups()
  if (ret==FALSE) {
    ps$rmc$successfully.run[i] = FALSE
    if (rmc$code.as.shown[chunk.ind]) {
      message = paste0("You have not yet started with chunk ", rmc$chunk.name[i],"\nIf you have no clue how to start, try hint().")
      cat(message)
      return(FALSE)
    }

    message = log$failure.message
    message = paste0(message,"\nFor a hint, type hint() in the console and press Enter.")
    message = paste(message)
    stop(message, call.=FALSE, domain=NA)
  } else if (ret=="warning") {
    message = paste0(log$warning.messages,collapse="\n\n")
    message(paste0("Warning: ", message))
    return(TRUE)
  } else {
    ps$rmc$successfully.run[i] = TRUE   
    cat("... chunk ok!\n")
    return(TRUE)
  }
}

get.check.chunks = function(ps, for.submission=FALSE) {
  restore.point("get.check.chunks")
  
  rmc = ps$rmc
  if (for.submission) return(seq.int(NROW(rmc)))
  
  # all changed chunks
  # and chunks different from shown code that have not been run
  cc = unique(c(which(rmc$chunk.changed),which(!rmc$code.as.shown & !rmc$successfully.run)))

  # no chunk changed
  if (length(cc)==0) {
    # we have a previously checked chunk (if exists)
    if (!is.null(ps$prev.checked.chunk)) {
      pcc = ps$prev.checked.chunk
      # rerun previous checked chunk
      if (!rmc$successfully.run[[pcc]]) {
        return(pcc)
      } else {
        if (pcc < NROW(rmc)) {
          cc = pcc+1
        } else {
          return(pcc)
        }
      }
    } else {
      # check the first chunk
      return(1)
    }
  }

  # all chunks that are required for changed chunks
  req = unique(unlist(lapply(cc, function(chunk.ind) ps$rmc$all.required[[chunk.ind]])))
  
  # only rerun required chunks that were not successfully run
  req = req[!rmc$successfully.run[req]]
  
  sort(unique(c(cc,req)))
}

can.chunk.be.edited = function(chunk.ind, ps = get.ps()) {
  restore.point("can.chunk.be.edited")

  cdt = ps$cdt
  ck = cdt[chunk.ind,]
  ex.ind = ck$ex.ind


  non.optional = which(cdt$ex.ind == ex.ind & !cdt$optional)
  if (length(non.optional)==0) {
    start.ex = TRUE
  } else {
    first.ind = non.optional[1]
    start.ex = chunk.ind <= first.ind
  }

  if (start.ex) {
    if (ex.ind==1)
      return(TRUE)
    ex.names = names(ps$edt$import.var[[ck$ex.ind]])
    if (is.null(ex.names))
      return(TRUE)
    edt = ps$edt
    ex.inds = edt$ex.ind[match(ex.names,edt$ex.name)]

    chunks = which(ps$cdt$ex.ind %in% ex.inds)
    solved = all(ps$cdt$is.solved[chunks] | ps$cdt$optional[chunks])
    if (all(solved))
      return(TRUE)
    log$failure.message = paste0("You must first solve and check all chunks in exercise(s) ", paste0(ex.names[ex.inds],collapse=", "), " before you can start this exercise.")
    return(FALSE)
  } else {
    ex.rows = which(cdt$ex.ind == ex.ind & cdt$chunk.ps.ind < chunk.ind)
    if (all(ps$cdt$is.solved[ex.rows] | ps$cdt$optional[ex.rows])) {
      return(TRUE)
    }

    log$failure.message = paste0("You must first solve and check all previous, non-optional chunks in this exercise before you can edit and solve this chunk.")
    return(FALSE)
  }

}

get.stud.chunk.code = function(txt = ps$stud.code,chunks = ps$rmc$chunk.name, ps = get.ps()) {
  restore.point("get.stud.chunk.code")
  chunk.start = which(str.starts.with(txt,"```{"))
  chunk.end   = setdiff(which(str.starts.with(txt,"```")), chunk.start)
  chunk.end = remove.verbatim.end.chunks(chunk.start,chunk.end)

  # remove all chunks that have no name (initial include chunk)
  chunk.name = str.between(txt[chunk.start],'"','"', not.found=NA)

  na.chunks  = is.na(chunk.name)
  chunk.start= chunk.start[!na.chunks]
  chunk.end  = chunk.end[!na.chunks]
  chunk.name = chunk.name[!na.chunks]

  chunk.txt = sapply(seq_along(chunk.start), function (i) {
      if (chunk.start[i]+1 > chunk.end[i]-1) return("")
      code = txt[(chunk.start[i]+1):(chunk.end[i]-1)]
      paste0(code, collapse="\n")
  })

  names(chunk.txt) = chunk.name
  chunk.txt = chunk.txt[chunk.name %in% chunks]

  if (!identical(names(chunk.txt), chunks)) {
    missing.chunks = paste0(setdiff(chunks, chunk.name),collapse=", ")
    stop("I miss chunks in your solution: ",missing.chunks,". You probably removed them or changed the title line or order of your chunks by accident. Please correct this!", call.=FALSE)
  }
  chunk.txt
}

get.user.name.from.rmd = function(txt) {
  txt = txt[1:min(10, length(txt))]
  rows = which(str.starts.with(txt,"Username:"))
  if (length(rows)==0) {
    stop("Within the first 10 lines of your Rmd file, you need a line of the form:\n\nUsername: Your username\n\nwhere you can replace 'Your username' with your username.")
  }
  rows = rows[1]
  user.name = str.trim(str.right.of(txt[rows],"Username:"))
  if (nchar(user.name)==0) {
    stop("Within the first 10 lines of your Rmd file, you need a line of the form:\n\nUsername: Your username\n\nwhere you can replace 'Your username' with your username.")
  }
  return(user.name)
}
