set.nali.names = function(x, nali) {
  restore.point("set.nali.names")
  ind = match(names(x), names(nali))
  names(x)[!is.na(ind)] = unlist(nali[ind[!is.na(ind)]])
  x
}

# update a chunk.ui to the specified mode
update.chunk.ui = function(uk, mode=uk$mode,app=getApp()) {
  restore.point("update.chunk.ui")
  ck = uk$ck
  uk$mode = mode
  ui = get.chunk.ui(uk)
  setUI(ck$nali$chunkUI, ui)
}

# returns the ui for a chunk based on its current mode
# mode can be "input", "output", or "inactive"
get.chunk.ui = function(uk) {
  restore.point("get.chunk.ui")
  mode = uk$mode
  if (mode=="input") {
    return(make.chunk.input.ui(uk=uk))
  } else if (mode=="output") {
    return(make.chunk.output.ui(uk=uk))
  } else if (mode=="inactive") {
    HTML("You must first solve the earlier chunks...")
  } else  {
    HTML("Not shown")
  }
}

make.chunk.input.ui = function(uk, theme="textmate", height=NULL, code.lines=NULL, fontSize=12, console.height=height, opts=rt.opts()) {
  restore.point("make.chunk.input.ui")

  ck = uk$ck
  nali = ck$nali
  code = merge.lines(uk$stud.code)

  if (is.null(code.lines))
    code.lines = max(length(sep.lines(code)), length(sep.lines(ck$sol.txt)))+1

  if (is.null(height)) {
    height = max((fontSize * 1.25) * code.lines,30)+35
  }
  if (is.null(console.height)) {
    console.code.lines = min(code.lines,10)
    console.height = (fontSize * 1.25) * console.code.lines + 50
  }

  if (uk$solved) {
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
  if (!opts$noeval) {
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
    aceEditor(nali$editor, code, mode="r",theme=theme, height=height, fontSize=13,hotkeys = keys, wordWrap=TRUE, debounce=1, showLineNumbers=isTRUE(opts$show.line.numbers)),
    aceEditor(nali$console, "", mode="r",theme="clouds", height=console.height, fontSize=13,hotkeys = NULL, wordWrap=TRUE, debounce=1, showLineNumbers=FALSE,highlightActiveLine=FALSE)
  )

  #aceAutocomplete(nali$editor)

  tagList(
    button.row,
    bsAlert(nali$alertOut),
    edit.row
  )
}

make.chunk.output.ui = function(uk, opts = rt.opts()) {
  restore.point("make.chunk.output.ui")
  ck = uk$ck; nali = ck$nali
  code = uk$stud.code

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

  solved = uk$solved
  mode = uk$mode
  if (solved) {
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
        html = chunk.special.output(uk=uk)
      } else {
        html = chunk.to.html(uk=uk)
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
        uk$stud.code = ck$shown.txt
        html = chunk.to.html(uk=uk, eval=FALSE)
      } else {
        html = chunk.to.html(uk=uk, eval=FALSE)
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

make.global.chunk.hotkey.handlers = function(opts=rt.opts()) {
  aceHotkeyHandler("checkKey", shiny.chunk.hotkey)
  aceHotkeyHandler("hintKey", shiny.chunk.hotkey)
  if (!opts$noeval) {
    aceHotkeyHandler("runKey", shiny.chunk.hotkey)
    aceHotkeyHandler("runLineKey", shiny.chunk.hotkey)
    aceHotkeyHandler("helpKey", shiny.chunk.hotkey)
  }
}

shiny.chunk.hotkey = function(keyId,editorId,selection,cursor,text,...,app=getApp(),ps=app$ps, opts=rt.opts()) {
  args = list(...)
  restore.point("shiny.chunk.hotkey")
  bi = as.numeric(str.between(editorId,"__","__"))
  chunk.ind = ps$bdf$stype.ind[bi]
  uk = ps$uk.li[[chunk.ind]]
  if (is.null(uk)) {
    restore.point("shiny.chunk.hotkey.null.uk")
    warning("shiny.chunk.hotkey: null uk")
  }
  noeval = opts$noeval
  if (keyId=="checkKey") {
    check.shiny.chunk(uk=uk)
  } else if (keyId=="hintKey") {
    hint.shiny.chunk(uk = uk,code=text)
  } else if (keyId=="runKey" & !noeval) {
    run.shiny.chunk(uk=uk,code=text)  
  } else if (keyId=="runLineKey" & !noeval) {
    run.line.shiny.chunk(uk=uk, cursor=cursor, selection=selection, code=text)
  } else if (keyId=="helpKey" & !noeval) {
    help.shiny.chunk(uk=uk, cursor=cursor, selection=selection, code=text)
  }
}

make.chunk.handlers = function(uk, nali= uk$ck$nali, opts=rt.opts()) {
  restore.point("make.chunk.handlers")

  buttonHandler(nali$checkBtn, check.shiny.chunk, uk=uk)
  buttonHandler(nali$hintBtn, hint.shiny.chunk, uk=uk)
  buttonHandler(nali$saveBtn, save.shiny.chunk, uk=uk)

  if (!opts$noeval) {
    buttonHandler(nali$runBtn, run.shiny.chunk, uk=uk)
    if (isTRUE(opts$show.data.exp))
      buttonHandler(nali$dataBtn, data.shiny.chunk, uk=uk)
  }

  if (isTRUE(opts$show.solution.btn))
    buttonHandler(nali$solutionBtn, solution.shiny.chunk, uk=uk)

  buttonHandler(nali$editBtn, edit.shiny.chunk, uk=uk)
}

run.shiny.chunk = function(uk, envir = uk$stud.env, code=uk$stud.code, opts=rt.opts(),...) {
  restore.point("run.shiny.chunk")
  ck = uk$ck
  if (opts$in.R.console) {
    eval.in.console(code, envir=envir)
  } else {
    eval.in.ace.console(code, envir=envir, consoleId=ck$nali$console)
  }
}

run.line.shiny.chunk = function(uk, envir=uk$stud.env, cursor=NULL, selection=NULL,code=getInputValue(uk$ck$nali$editor),..., app=getApp(), opts=rt.opts()) {
  restore.point("run.line.shiny.chunk")

  uk$stud.code = code

  if (selection == "") {
    txt = sep.lines(code)
    txt = txt[cursor$row+1]
  } else {
    txt = selection
  }
  if (opts$in.R.console) {
    eval.in.console(txt, envir=envir)
  } else {
    eval.in.ace.console(txt, envir=envir, consoleId=uk$ck$nali$console)
  }
}

check.shiny.chunk = function(uk, internal=FALSE, max.lines=300, store.output=FALSE, opts=rt.opts(), app=getApp(),...) {
  uk$stud.code = getInputValue(uk$ck$nali$editor)
  restore.point("check.shiny.chunk")
  ck = uk$ck
  if (!is.false(opts$catch.errors)) {
    ret = tryCatch(check.chunk(uk=uk, store.output=store.output, use.secure.eval=opts$use.secure.eval), error = function(e) {uk$log$failure.message <- as.character(e);return(FALSE)})
  } else {
    ret = check.chunk(uk=uk,store.output=store.output, use.secure.eval=opts$use.secure.eval)
  }

  # Don't yet know how we deal with this
  # ps$prev.check.chunk.ind = chunk.ind

  if (!internal) {
    if (!ret) {
      txt = merge.lines(c(uk$log$success, uk$log$failure.message,"Press Ctrl-H to get a hint."))
      updateAceEditor(app$session, ck$nali$console, value=txt, mode="text")
      uk$solved = FALSE
    } else {
      #restore.point("success test shiny chunk")
      
      if (NROW(uk$log$chunk.console.out)>max.lines) {
        txt = merge.lines(
          c("You successfully solved the chunk!",
            uk$log$chunk.console.out[1:max.lines],
            paste0("\n...", NROW(uk$log$chunk.console.out)-max.lines," lines ommited...")))
      } else {
        txt = merge.lines(c("You successfully solved the chunk!",
                            uk$log$chunk.console.out))
      }
      updateAceEditor(app$session, ck$nali$console, value=txt,mode="r")
      proceed.with.successfuly.checked.chunk(uk)
    }
  }

  #cat("\nend check.shiny.chunk.ui\n")
  return(ret)
}



proceed.with.successfuly.checked.chunk = function(uk,opts=rt.opts()) {
  restore.point("proceed.with.successfuly.checked.chunk")
  ck = uk$ck
  uk$solved = TRUE

  # If we have precomp=TRUE, it is often sensible to replace 
  # user solution with sample solution 
  # A replace.sol chunk option takes precedence over global problem set option
  if (!is.null(ck$args[["replace.sol"]])) {
    replace.sol = ck$args[["replace.sol"]]
  } else {
    replace.sol = isTRUE(opts$replace.sol)
  }
  
  if (isTRUE(replace.sol)) {
    uk$stud.code = ck$sol.txt
  }
  
  # if (is.last.chunk.of.ex(chunk.ind)) {
  #   ex.ind = ps$cdt$ex.ind[chunk.ind]
  #   if (!isTRUE(ps$precomp))
  #     ps$edt$ex.final.env[[ex.ind]] = copy.stud.env(ps$stud.env)
  # }
  
  uk$mode = "output"
  update.chunk.ui(uk)

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


hint.shiny.chunk = function(uk, code=getInputValue(uk$ck$nali$editor), ...,opts=rt.opts(),app=getApp()) {
  restore.point("hint.shiny.chunk")
  
  uk$stud.code = code

  if (!isTRUE(opts$hint.noeval)) {
    if (!identical(uk$stud.code,uk$last.check.code))
      check.chunk(uk,opts=opts)
  }
  
  txt = tryCatch(merge.lines(
      capture.output(run.chunk.hint(uk=uk, opts=opts))
    ),
    error = function(e) {merge.lines(as.character(e))}
  )
  txt = paste0("Hint: ", txt)
  updateAceEditor(app$session, uk$ck$nali$console, value=txt, mode="text")
  update.ups.hint.shown(uk)
}

help.shiny.chunk = function(uk, cursor=NULL, selection="",..., app=getApp()) {
  set.shiny.chunk(chunk.ind, cursor=cursor, selection=selection)
  envir=uk$stud.env; in.R.console=is.null(uk$nali$console)
  restore.point("help.shiny.chunk")

  if (selection == "") {
    txt = sep.lines(ps$code)
    txt = txt[cursor$row+1]
    txt = word.at.pos(txt, pos=cursor$column+1)
  } else {
    txt = selection
  }
  if (is.null(txt) | isTRUE(nchar(txt)==0)) {
    updateAceEditor(app$session, uk$ck$nali$console, value="No R command selected to show help for.", mode="text")
    return()
  }
  
  help = get.help.txt(txt)
  # To do: replace special characters in a better manner
  help = iconv(help, to='ASCII//TRANSLIT')
  #Encoding(help) = "UTF8"
  updateAceEditor(app$session, uk$ck$nali$console, value=help, mode="text")

  return()
}

restore.shiny.chunk = function(uk,...,app=getApp()) {
  restore.point("restore.shiny.chunk")

  uk$stud.code = uk$ck$shown.txt
  uk$solved = FALSE

  updateAceEditor(app$session, uk$ck$nali$editor, value=uk$stud.code, mode="r")
  updateAceEditor(app$session, uk$ck$console, value="restored originally shown code...", mode="text")
}


solution.shiny.chunk = function(uk,...,app=getApp()) {
  restore.point("solution.shiny.chunk")

  uk$stud.code = uk$ck$sol.txt
  
  updateAceEditor(app$session, uk$ck$nali$editor, value = uk$stud.code, mode="r")
  updateAceEditor(app$session, uk$ck$nali$console, value = "Sample solution shown", mode="text")
}


# edit button is pressed
edit.shiny.chunk = function(uk, opts = rt.opts(),...) {
  restore.point("edit.shiny.chunk")
  ck = uk$ck
  #browser()
  #if (can.chunk.be.edited(ck)) {
  if (TRUE) {
    update.chunk.ui(uk=uk, mode="input")
  } else {
    nali = ck$nali
    rtutorAlert(session,nali$alertOut,
        title = "Cannot edit chunk",
        message= uk$log$failure.message,
        type = "info", append=FALSE
    )
  }
}

data.shiny.chunk = function(uk,...,app=getApp()) {
  restore.point("data.shiny.chunk")
  update.data.explorer.ui()
  updateTabsetPanel(session=app$session, inputId="exTabsetPanel",selected = "dataExplorerTabPanel")
}

save.shiny.chunk = function(uk,...,ps=get.ps(),app=getApp()) {
  restore.point("data.shiny.chunk")
  #set.shiny.chunk(chunk.ind)
  save.sav()
  nali = uk$ck$nali[[chunk.ind]]

  createAlert(app$session, nali$alertOut,
    title = paste0("Saved as ", ps$sav.file),
    content = "",
    style = "info", append=FALSE
  )
}

chunk.to.html = function(uk,txt = uk$stud.code, opts=rt.opts(), envir=get.chunk.env(uk), eval=TRUE, success.message=isTRUE(uk$solved), echo=TRUE, nali=NULL, quiet=TRUE) {
  restore.point("chunk.to.html")
  if (is.null(txt))
    return("")
  ck = uk$ck


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
  header = paste0("```{r '",ck$id,"'",chunk.opt.list.to.string(args,TRUE),"}")

  library(knitr)
  library(markdown)
  txt = c(header,sep.lines(txt),"```")
  #all.parent.env(stud.env)
  html ="Evaluation error!"
  if (opts$use.secure.eval) {
    html = try(
      RTutor::rtutor.eval.secure(quote(
        knitr::knit2html(text=txt, envir=envir,fragment.only = TRUE,quiet = quiet)
      ), envir=environment())
    )
  } else {
    html = try(
      knitr::knit2html(text=txt, envir=envir,fragment.only = TRUE,quiet = quiet)
    )
  }
  
  if (is(html, "try-error")) {
    html = as.character(html)
  }
  restore.point("chunk.to.html.knit2html")

  nali = ck$nali
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
