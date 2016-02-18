set.nali.names = function(x, nali) {
  restore.point("set.nali.names")
  ind = match(names(x), names(nali))
  names(x)[!is.na(ind)] = unlist(nali[ind[!is.na(ind)]])
  x
}

# update a chunk.ui to the specified mode
update.chunk.ui = function(ck, mode=ui.state$mode, ui.state=ck$ui.state,app=getApp()) {
  restore.point("update.chunk.ui")
  ui.state$mode = mode
  ui = get.chunk.ui(ck)
  setUI(ck$nali$chunkUI, ui)
}

# returns the ui for a chunk based on its current mode
# mode can be "input", "output", or "inactive"
get.chunk.ui = function(ck) {
  restore.point("get.chunk.ui")
  mode = ck$ui.state$mode
  if (mode=="input") {
    return(make.chunk.input.ui(ck=ck))
  } else if (mode=="output") {
    return(make.chunk.output.ui(ck=ck))
  } else if (mode=="inactive") {
    HTML("You must first solve the earlier chunks...")
  } else  {
    HTML("Not shown")
  }
}

make.chunk.input.ui = function(ck, theme="textmate", height=NULL, code.lines=NULL, fontSize=12, console.height=height, opts=ps.opts()) {
  restore.point("make.chunk.input.ui")

  nali = ps$cdt$nali[[chunk.ind]]
  code = ck$state$stud.code

  if (is.null(code.lines))
    code.lines = max(length(sep.lines(code)), length(sep.lines(ck$sol.txt)))+1

  if (is.null(height)) {
    height = max((fontSize * 1.25) * code.lines,30)+35
  }
  if (is.null(console.height)) {
    console.code.lines = min(code.lines,10)
    console.height = (fontSize * 1.25) * console.code.lines + 50
  }

  if (ck$state$is.solved[chunk.ind]) {
    label = "was already solved"
  } else {
    label = "not yet solved"
  }

  solutionBtn  = NULL
  if (isTRUE(opts$show.solution.btn)) {
    solutionBtn=bsButton(nali$solutionBtn, "solution",size="extra-small")
  } else {
    solutionBtn  = NULL
  }
  if (isTRUE(opts$show.data.exp)) {
    dataBtn = bsButton(nali$dataBtn, "data", size="extra-small")
  } else {
    dataBtn  = NULL
  }

  if (isTRUE(opts$show.save.btn)) {
    saveBtn = bsButton(nali$saveBtn, "save",size="extra-small")
  } else {
    saveBtn = NULL
  }
  if (!ps$noeval) {
    button.row = tagList(
      bsButton(nali$checkBtn, "check",size="extra-small"),
      bsButton(nali$hintBtn, "hint", size="extra-small"),
      bsButton(nali$runBtn, "run chunk",size="extra-small"),
      dataBtn,
      saveBtn,
      solutionBtn
    )
    keys = list(runLineKey="Ctrl-Enter", helpKey="F1", runKey="Ctrl-R|Ctrl-Shift-Enter", hintKey="Ctrl-H", checkKey = "Ctrl-Alt-R|Ctrl-T")

  } else {
    button.row = tagList(
      bsButton(nali$checkBtn, "check",size="extra-small"),
      bsButton(nali$hintBtn, "hint", size="extra-small"),
      solutionBtn
    )
    keys = list(hintKey="Ctrl-H", checkKey = "Ctrl-Alt-R|Ctrl-T")
  }

  keys = set.nali.names(keys, nali)

  edit.row = tagList(
    aceEditor(nali$editor, code, mode="r",theme=theme, height=height, fontSize=13,hotkeys = keys, wordWrap=TRUE, debounce=10),
    aceEditor(nali$console, "", mode="r",theme="clouds", height=console.height, fontSize=13,hotkeys = NULL, wordWrap=TRUE, debounce=10, showLineNumbers=FALSE,highlightActiveLine=FALSE)
  )

  #aceAutocomplete(nali$editor)

  tagList(
    button.row,
    bsAlert(nali$alertOut),
    edit.row
  )
}

make.chunk.output.ui = function(ck, opts = ps.opts()) {
  restore.point("make.chunk.output.ui")
  nali = ck$nali
  code = ck$state$stud.code

  if (isTRUE(opts$show.save.btn)) {
    saveBtn = bsButton(nali$saveBtn, "save", size="extra-small")
  } else {
    saveBtn = NULL
  }
  if (isTRUE(opts$show.data.exp)) {
    dataBtn = bsButton(nali$dataBtn, "data", size="extra-small")
  } else {
    dataBtn  = NULL
  }

  if (!opts$noeval) {
    button.row = tagList(
      bsButton(nali$editBtn, "edit",size="extra-small"),
      dataBtn,
      saveBtn
    )
  } else {
    button.row = tagList(
      bsButton(nali$editBtn, "edit",size="extra-small")
    )
  }

  is.solved = ck$state$is.solved
  mode = ck$ui.state$mode
  if (is.solved) {
    code = code
    args = ck$args
    
    preknit = 
        # noeval will always be preknit
        opts$noeval | 
        # don't preknit special output or if chunk option replace.sol=FALSE
        (opts$preknit & !(!is.null(args[["output"]]) | is.false(args$replace.sol)))
    
    if (preknit) {
      if (!is.null(args[["output"]])) {
        html = HTML("<p> SPECIAL OUTPUT HERE <p>")
      } else {
        html = HTML(ck$sol.html)
      }
    } else {
      # not preknitted (default)
      if (!is.null(args[["output"]])) {
        html = chunk.special.output(ck=ck)
      } else {
        html = chunk.to.html(ck=ck)
        html = HTML(html)
      }
    }
    
    
  } else {
    
    if ((identical(code, ck$shown.txt) | isTRUE(opts$noeval)) & !is.null(ck$shown.html)) {
      # just show precompiled show
      html = ck$shown.html
    } else {
      # compile no solution again
      if (opts$noeval) {
        ck$state$stud.code = ck$shown.txt
        html = chunk.to.html(ck=ck, eval=FALSE)
      } else {
        html = chunk.to.html(ck=ck, eval=FALSE)
      }
      
    }
    html = HTML(html)
  }
  restore.point("make.chunk.output.ui.2")

  tagList(
    button.row,
    bsAlert(nali$alertOut),
    html
  )
}


make.chunk.handlers = function(ck, nali=ck$nali, opts=ps.opts()) {
  restore.point("make.chunk.handlers")

  buttonHandler(nali$checkBtn, check.shiny.chunk, chunk.ind=chunk.ind)
  aceHotkeyHandler(nali$checkKey, check.shiny.chunk, chunk.ind=chunk.ind)
  buttonHandler(nali$hintBtn, hint.shiny.chunk, chunk.ind=chunk.ind)
  aceHotkeyHandler(nali$hintKey, hint.shiny.chunk, chunk.ind=chunk.ind)
  buttonHandler(nali$saveBtn, save.shiny.chunk, chunk.ind=chunk.ind)

  if (!opts$noeval) {
    buttonHandler(nali$runBtn, run.shiny.chunk, chunk.ind=chunk.ind)
    aceHotkeyHandler(nali$runKey, run.shiny.chunk, chunk.ind=chunk.ind)
    if (isTRUE(opts$show.data.exp))
      buttonHandler(nali$dataBtn, data.shiny.chunk, chunk.ind=chunk.ind)
    aceHotkeyHandler(nali$runLineKey, run.line.shiny.chunk, chunk.ind=chunk.ind)
    aceHotkeyHandler(nali$helpKey, help.shiny.chunk, chunk.ind=chunk.ind)
  }

  if (isTRUE(opts$show.solution.btn))
    buttonHandler(nali$solutionBtn, solution.shiny.chunk, chunk.ind=chunk.ind)

  buttonHandler(nali$editBtn, edit.shiny.chunk, chunk.ind=chunk.ind)
}

run.shiny.chunk = function(ck, envir = ck$stud.env, code=ck$state$stud.code, opts=ps.opts()) {
  restore.point("run.shiny.chunk")
  if (opts$in.R.console) {
    eval.in.console(code, envir=envir)
  } else {
    eval.in.ace.console(code, envir=envir, consoleId=ck$nali$console)
  }
}

run.line.shiny.chunk = function(ck, envir=ck$stud.env, cursor=NULL, selection=NULL, code=ck$state$stud.code,...) {
  restore.point("run.line.shiny.chunk")

  if (selection == "") {
    txt = sep.lines(ps$code)
    txt = txt[ps$cursor$row+1]
  } else {
    txt = selection
  }
  if (opts$in.R.console) {
    eval.in.console(txt, envir=envir)
  } else {
    eval.in.ace.console(txt, envir=envir, consoleId=ps$nali$console,session=ps$session)
  }
}

check.shiny.chunk = function(ck, internal=FALSE, max.lines=300, store.output=FALSE, opts=ps.opts(), app=getApp()) {
  restore.point("check.shiny.chunk")
  if (opts$use.secure.eval) {
    ret = secure.check.chunk(ck=ck,store.output=store.output)
  } else {
    if (!is.false(ps$catch.errors)) {
      ret = tryCatch(check.chunk(ck=ck, store.output=store.output), error = function(e) {ps$failure.message <- as.character(e);return(FALSE)})
    } else {
      ret = check.chunk(ck=ck,store.output=store.output)
    }
  }
  
  
  # Don't yet know how we deal with this
  # ps$prev.check.chunk.ind = chunk.ind
  if (!ret)
    ck$state$is.solved = FALSE
  
  if (!internal) {
    if (!ret) {
      txt = merge.lines(c(ck$log$success, ck$log$failure.message,"Press Ctrl-H to get a hint."))
      updateAceEditor(app$session, ck$nali$console, value=txt, mode="text")
      ck$state$is.solved = FALSE
    } else {
      #restore.point("success test shiny chunk")
      
      if (NROW(ck$log$chunk.console.out)>max.lines) {
        txt = merge.lines(
          c("You successfully solved the chunk!",
            ck$log$chunk.console.out[1:max.lines],
            paste0("\n...", NROW(ck$log$chunk.console.out)-max.lines," lines ommited...")))
      } else {
        txt = merge.lines(c("You successfully solved the chunk!",
                            ck$log$chunk.console.out))
      }
      updateAceEditor(app$session, ck$nali$console, value=txt,mode="r")
      proceed.with.successfuly.checked.chunk(ck)
    }
  }

  #cat("\nend check.shiny.chunk.ui\n")
  return(ret)
}



proceed.with.successfuly.checked.chunk = function(ck,opts=ps.opts()) {
  restore.point("proceed.with.successfuly.checked.chunk")

  ck$state$is.solved = TRUE

  # If we have precomp=TRUE, it is often sensible to replace 
  # user solution with sample solution 
  # A replace.sol chunk option takes precedence over global problem set option
  if (!is.null(ck$args[["replace.sol"]])) {
    replace.sol = ck$args[["replace.sol"]]
  } else {
    replace.sol = isTRUE(opts$replace.sol)
  }
  
  if (isTRUE(replace.sol)) {
    ck$stat$stud.code = ck$sol.txt
  }
  
  # if (is.last.chunk.of.ex(chunk.ind)) {
  #   ex.ind = ps$cdt$ex.ind[chunk.ind]
  #   if (!isTRUE(ps$precomp))
  #     ps$edt$ex.final.env[[ex.ind]] = copy.stud.env(ps$stud.env)
  # }
  
  ck$ui.state$mode = "output"
  update.chunk.ui(ck)

  # # set the next chunk to edit mode
  # if (chunk.ind < NROW(ps$cdt)) {
  #   if (ps$cdt$ex.ind[chunk.ind] == ps$cdt$ex.ind[chunk.ind+1] &
  #      !ps$cdt$is.solved[chunk.ind+1]) {
  # 
  #     #cat("update next chunk...")
  #     ps$cdt$mode[chunk.ind+1] = "input"
  #     update.chunk.ui(chunk.ind+1)
  #   }
  # }

}


hint.shiny.chunk = function(chunk.ind, ...,session=ps$session, ps=get.ps()) {
  restore.point("hint.shiny.chunk")
  set.shiny.chunk(chunk.ind, ps=ps)
  envir=ps$stud.env; in.R.console=is.null(ps$nali$console)
  
  # If the current chunk has not been checked. Check it again
  #if (!identical(chunk.ind,ps$prev.check.chunk.ind))
  if (!isTRUE(ps$hint.noeval))
    check.shiny.chunk(chunk.ind, internal=TRUE)
  
  txt = tryCatch(merge.lines(capture.output(hint(ps=ps))),
         error = function(e) {merge.lines(as.character(e))})
  txt = paste0("Hint: ", txt)
  updateAceEditor(ps$session, ps$nali$console, value=txt, mode="text")
}

help.shiny.chunk = function(chunk.ind, cursor=NULL, selection="",...,session=ps$session, ps=get.ps()) {
  set.shiny.chunk(chunk.ind, cursor=cursor, selection=selection)
  envir=ps$stud.env; in.R.console=is.null(ps$nali$console)
  restore.point("help.shiny.chunk")

  if (ps$selection == "") {
    txt = sep.lines(ps$code)
    txt = txt[ps$cursor$row+1]
    txt = word.at.pos(txt, pos=ps$cursor$column+1)
  } else {
    txt = ps$selection
  }
  if (is.null(txt) | isTRUE(nchar(txt)==0)) {
    updateAceEditor(ps$session, ps$nali$console, value="No R command selected to show help for.", mode="text")
    return()
  }
  
  help = get.help.txt(txt)
  # To do: replace special characters in a better manner
  help = iconv(help, to='ASCII//TRANSLIT')
  #Encoding(help) = "UTF8"
  updateAceEditor(ps$session, ps$nali$console, value=help, mode="text")

  return()
}

restore.shiny.chunk = function(chunk.ind=ps$chunk.ind,...,session=ps$session,ps=get.ps()) {
  restore.point("restore.shiny.chunk")
  set.shiny.chunk(chunk.ind)

  ps$cdt$stud.code[[chunk.ind]] = ps$cdt$shown.txt[[chunk.ind]]
  ps$cdt$is.solved[[chunk.ind]] = FALSE
  ps$stud.code = ps$cdt$stud.code[[chunk.ind]]

  updateAceEditor(ps$session, ps$nali$editor, value=ps$stud.code, mode="r")
  updateAceEditor(ps$session, ps$nali$console, value="restored originally shown code...", mode="text")
}


solution.shiny.chunk = function(chunk.ind=ps$chunk.ind,...,session=ps$session,ps=get.ps()) {
  restore.point("restore.shiny.chunk")
  set.shiny.chunk(chunk.ind)

  ps$cdt$stud.code[[chunk.ind]] = ps$cdt$sol.txt[[chunk.ind]]
  #ps$cdt$is.solved[[chunk.ind]] = FALSE
  ps$stud.code = ps$cdt$stud.code[[chunk.ind]]

  updateAceEditor(ps$session, ps$nali$editor, value=ps$stud.code, mode="r")
  updateAceEditor(ps$session, ps$nali$console, value="Sample solution shown", mode="text")
}


output.shiny.chunk = function(chunk.ind, ...,session=ps$session, ps=get.ps()) {
  restore.point("output.shiny.chunk")
  set.shiny.chunk(chunk.ind)
  update.chunk.ui(chunk.ind, mode="output")
}

# edit button is pressed
edit.shiny.chunk = function(ck, opts = ps.opts(),...) {
  restore.point("edit.shiny.chunk")
  #browser()
  if (can.chunk.be.edited(ck)) {
    update.chunk.ui(ck=ck, mode="input")
  } else {
    nali = ck$nali
    rtutorAlert(session,nali$alertOut,
        title = "Cannot edit chunk",
        message= ck$log$failure.message,
        type = "info", append=FALSE
    )
  }
}

data.shiny.chunk = function(chunk.ind=ps$chunk.ind,session=ps$session,
                            ...,ps=get.ps()) {
  restore.point("data.shiny.chunk")
  set.shiny.chunk(chunk.ind, from.data.btn = TRUE)

  if (FALSE) {
    RRprofStart()
    update.data.explorer.ui()
    RRprofStop()
  # Uncomment to open the report
    RRprofReport()

    Rprof(tmp <- tempfile())
    update.data.explorer.ui()
    Rprof()
    summaryRprof(tmp)
    unlink(tmp)
  }
  update.data.explorer.ui()
  updateTabsetPanel(session, inputId="exTabsetPanel",
                    selected = "dataExplorerTabPanel")
}

save.shiny.chunk = function(chunk.ind=ps$chunk.ind,session=ps$session,
                            ...,ps=get.ps()) {
  restore.point("data.shiny.chunk")
  #set.shiny.chunk(chunk.ind)
  save.sav()
  nali = ps$cdt$nali[[chunk.ind]]

  createAlert(session, nali$alertOut,
    title = paste0("Saved as ", ps$sav.file),
    content = "",
    style = "info", append=FALSE
  )
}

# update.all.chunk.ui = function(ps=get.ps()) {
#   restore.point("update.all.chunks")
#   for (chunk.ind in ps$cdt$chunk.ps.ind) {
#     update.chunk.ui(chunk.ind, ps=ps)
#   }
# }


chunk.to.html = function(ck,txt = ck$stud.code, opts=ps.opts(), envir=get.chunk.env(ck), eval=TRUE, success.message=isTRUE(ck$state$is.solved), echo=TRUE, nali=NULL, quiet=TRUE) {
  restore.point("chunk.to.html")
  if (is.null(txt))
    return("")


  # Adapt output text
  if (paste0(txt,collapse="\n") == "")
    txt = "# Press 'edit' to enter your code."

  if (ck$num.e>0) {
    if (success.message) {
      add = c("# Great, solved correctly!")
      if (opts$show.points) {
        points = ck$max.points
        if (points==1) {
          add = paste0(add, " (1 point)")
        } else if (points>0) {
          add = paste0(add, " (",points, " points)")
        }
      }
      txt = c(add,txt)
    } else {
      txt = c("# Not yet solved...",txt)
      echo = TRUE
    }
  }
  
  # Get output arguments
  args = opts$chunk.out.args
  if (length(ck$args)>0) {
    args[names(ck$args)] = ck$args
  }
  args$eval = eval
  args$echo = echo
  header = paste0("```{r '",ck$id,"'",chunk.opt.list.to.string(opt,TRUE),"}")

  library(knitr)
  library(markdown)
  txt = c(header,sep.lines(txt),"```")
  #all.parent.env(stud.env)
  html ="Evaluation error!"
  if (opts$use.secure.eval) {
    html = try(
      RTutor::rtutor.eval.secure(quote(
        knitr::knit2html(text=txt, envir=stud.env,fragment.only = TRUE,quiet = quiet)
      ), envir=environment())
    )
  } else {
    html = try(
      knitr::knit2html(text=txt, envir=stud.env,fragment.only = TRUE,quiet = quiet)
    )
  }
  
  if (is(html, "try-error")) {
    html = as.character(html)
  }
  restore.point("chunk.to.html.knit2html")

  # Add syntax highlightning
  if (!is.null(nali$chunkUI)) {
    html = paste0(paste0(html,collapse="\n"),"\n",
     "<script>$('#",nali$chunkUI," pre code').each(function(i, e) {hljs.highlightBlock(e)});</script>")
  }

  html
}

default.chunk.out.args = function() {
  list(fig.width=6.5, fig.height=4.5, fig.align='center', "warning"=FALSE, cache=FALSE, collapse=TRUE, comment=NA)
}
