# Parse a traditional RTutor chunk that allows
# interactive user input, hints and check of solution
rtutor.parse.task.chunk  = function(bi,ps,args, opts=ps$opts) {
  restore.point("rtutor.parse.task.chunk")
  
  #code = ps$txt[(br$start+1):(br$end-1)]

  # a task chunk is a container
  ps$bdf$is.container[[bi]] = TRUE
  set.container.div.and.output(bi,ps)
  ps$bdf$always.reload[[bi]] = TRUE
  ps$bdf$is.static[[bi]] = FALSE

  # only assign after container definitions are done
  bdf = ps$bdf; br = bdf[bi,];

   
  # get text of fragments and children
  child.ind = which(bdf$parent == bi)
  res = get.child.and.fragment.txt.li(bi=bi,ps = ps,keep.header.footer = FALSE)
  txt.li = res$txt.li
  is.frag = res$is.frag

  # fragments get block type "code"  
  types = rep("code", length(txt.li))
  types[!is.frag] = bdf$type[child.ind]

  # create a chunk object that stores values during parsing  
  ck = new.env()
  txt.li = txt.li
  ck$args = args
  ck$bi = bi
  ck$id = br$id
  ck$chunk.name = ck$id
  ck$stype = "task_chunk"
  ck$shown.txt = ck$sol.txt = NULL
  
  # parse all blocks and create shown.txt, sol.txt,
  # expression lists and hint and test markers in ck
  for (i in seq_along(txt.li)) {
    add.chunk.block(ck=ck,type=types[i], str=txt.li[[i]],cbi=i,bi=bi,ps=ps)
  }
  
  # Collapse, since txt from aceEditor is also collapsed
  ck$shown.txt = paste0(ck$shown.txt, collapse="\n")
  ck$sol.txt = paste0(ck$sol.txt, collapse="\n")
  
  # add tests and hints
  add.chunk.tests.and.hints(ck)

  
  # specify points
  if (is.null(args$points)) {
    ck$max.points = max(opts$e.points * (ck$num.e-sum(ck$e.shown)) + opts$chunk.points, opts$chunk.min.points)
  } else {
    ck$max.points = args$points
  }
  
  # clean up
  ck$test.hint.marker = NULL
  
  # add info to ps$bdf
  ps$bdf$obj[[bi]]$ck = ck
  ps$bdf$is.task[[bi]] = TRUE

  shown.txt = paste0(ck$shown.txt, collapse="\n")
  sol.txt = paste0(ck$sol.txt, collapse = "\n")
  ps$bdf[bi,c("shown.rmd","sol.rmd","out.rmd")] = c(
    shown.txt, sol.txt, sol.txt  
  )
  ck$nali = make.chunk.nali(id=ck$id, output.id = br$output.id)


  invisible(ck)
}

# init a user chunk
# this holds user specific states for a chunk
make.user.chunk = function(ck) {
  uk = list(
    ck = ck,
    mode = "output", # output mode
    stud.code = ck$shown.txt,
    is.solved = FALSE,
    test.passed = length(ck$test.expr),
    stud.env = NULL,
    log = NULL
  )
  uk
}

init.user.chunk = function(uk) {
  restore.point("init.user.chunk")
  uk = as.environment(as.list(uk))
  uk$log = new.env()
  uk
}

# Names for task chunk shiny widgets
make.chunk.nali = function(prefix=paste0(id,"_"), id, output.id=NULL) {
  restore.point("make.chunk.nali")
  if (output.id =="") stop()
  base.names = c(
    "chunkUI", "editor","console","chunkout",
    "runLineBtn","runBtn","checkBtn","hintBtn","helpBtn","dataBtn",
    "outputBtn", "restoreBtn", "saveBtn",
    "editBtn","solutionBtn","alertOut",
    "inputPanel","outputPanel"
  )
  nali = paste0(prefix,"_",base.names)
  names(nali) =  base.names
  keys = c("runLineKey","runKey","checkKey","hintKey","helpKey")
  names(keys)=keys
  nali = as.list(c(nali,keys))
  if (!is.null(output.id)) nali$chunkUI = output.id
  nali
}

init.ui.state.task.chunk = function(ck) {
  ui.state = as.environment(list(
    
  ))
}

init.state.task.chunk = function(ck) {
  state = as.environment(list(
  ))
}

add.chunk.block = function(ck,type,str, add.enter.code.here=FALSE,cbi, bi,ps) {
  restore.point("add.chunk.block")
  if (is.null(str)) return()
  
  if (type=="code") {
    btxt = str
  } else {
    btxt =  str[-c(1,length(str))]
  }
  str = paste0(str, collapse="\n")
  has.code = nchar(str.trim(str))>0
  if (!has.code) return()
  
  # Add shown code and solution code
  if (type == "code") {
    if (add.enter.code.here) {
      ck$shown.txt = c("\n# enter your code here ...\n", ck$shown.txt)  
    }
    ck$sol.txt = c(ck$sol.txt,btxt)
  } else if (type=="show" | type == "show_notest") {
    ck$shown.txt = c(ck$shown.txt,btxt)
    ck$sol.txt = c(ck$sol.txt,btxt)
  }
  
  # add expressions  
  if (type == "code" || type =="show") {
    ret = parse.text.with.source(paste0(btxt,collapse="\n"))
    e.li = ret$expr
    e.source.li = ret$source
    e.shown = rep(type=="show", length(e.li)) 
    ck$e.li = c(ck$e.li, e.li)
    ck$e.source.li = c(ck$e.source.li, e.source.li)
    ck$e.shown = c(ck$e.shown,e.shown)
  }

  # add marker for hints, test and test_arg
  # they will be associated with the previous expression
  if (type == "test" | type == "test_arg" | type == "hint" | type=="test_hint_arg" | type == "test_calls" | type == "add_to_hint") {

    ck$test.hint.marker = c(ck$test.hint.marker,list(list(
      type=type,bi=bi,cbi=cbi,e.ind=length(ck$e.li),btxt=btxt
    )))
  }
  
}

add.chunk.tests.and.hints = function(ck) {
  restore.point("add.chunk.hints.and.tests")
  
  # create default test.txt and hint.txt
  e.li = ck$e.li
  ck$num.e = length(ck$e.li)
  ck$test.txt = sapply(seq_along(e.li), function(i) test.code.for.e( e.li[[i]]))
  ck$hint.txt = sapply(seq_along(e.li), function(i) hint.code.for.e( e.li[[i]]))
  
  for (ma in ck$test.hint.marker) {
    type = ma$type
    e.ind = ma$e.ind
    btxt = ma$btxt
    e = ck$e.li[[e.ind]]
    if (type == "test") {
      ck$test.txt[e.ind] <- paste0(btxt,collapse="\n")
      # Remove default hint for manual tests
      ck$hint.txt[e.ind] <- ""
    } else if (type == "test_arg") {
      test.txt = test.code.for.e(e, extra.arg = paste0(btxt,collapse=", "))
      ck$test.txt[e.ind] <- test.txt
    } else if (type == "test_calls") {
      test.txt = test.code.for.e(e, extra.arg = paste0(btxt,collapse=", "))
      ck$test.txt[e.ind] <- test.txt
    } else if (type == "test_hint_arg") {
      extra.arg = paste0(btxt,collapse=",")
      test.txt = test.code.for.e(e, extra.arg = extra.arg)
      ck$test.txt[e.ind] <- test.txt
  
      hint.txt = hint.code.for.e(e, extra.arg = extra.arg)
      ck$hint.txt[e.ind] <- hint.txt
    } else if (type == "hint") {
      if (e.ind == 0) {
        ck$chunk.hint.txt =  paste0(btxt,collapse="\n")
      } else {
        ck$hint.txt[e.ind] <- paste0(btxt,collapse="\n")
      }
    } else if (type == "add_to_hint") {
      hint.txt = hint.code.for.e(e,extra.code = btxt)
      ck$hint.txt[e.ind] <- hint.txt
    }
  }

  # Parse tests and hints
  ck$test.expr = lapply(ck$test.txt, parse.text)
  ck$hint.expr = lapply(ck$hint.txt, parse.text)
  
  if (is.null(ck$chunk.hint.txt)) {
    ck$chunk.hint = NULL 
  } else {
    ck$chunk.hint = parse.text(ck$chunk.hint.txt)  
  }
}



examples.test.code.for.e = function() {
  f = function(e) {
    e = substitute(e)
    test.code.for.e(e)
  }

  f(fun <- function(x) {x*x})
}

get.expr.test.args = function(e) {
  restore.point("get.expr.test.args")

  funs = find.funs(e)

  no.value.funs = c("plot","hist","qplot","geom_point","geom_line","geom_smooth","geom_density","lines","points","facet_wrap")
  if (any(funs %in% no.value.funs)) {
    args = "check.arg.by.value=FALSE, allow.extra.arg=TRUE,ok.if.same.val = FALSE"
  } else {
    args = ""
  }
  args

}

test.code.for.e = function(e, extra.arg=get.expr.test.args(e)) {
  restore.point("test.code.for.e")
  if (is.null(e))
    return("")

  extra.arg = ifelse(extra.arg=="","",paste0(",",extra.arg))
  if (is.assignment(e)) {
    var = deparse1(e[[2]],collapse="\n")
    rhs = deparse1(e[[3]],collapse="\n")
    call.name = name.of.call(e[[3]])
    if (call.name == "function") {
      code=paste0("check.function(", var, "<-",rhs,extra.arg,")")
    } else {
      code = paste0("check.assign(", var, "<- ",rhs,extra.arg,")")
    }
  } else {
    estr = deparse1(e)
    code = paste0("check.call(", estr,extra.arg,")")
  }
  code
}

hint.code.for.e = function(e, extra.code = NULL, extra.arg = NULL) {
  restore.point("hint.code.for.e")
  if (is.null(e))
    return("")
  if (!is.null(extra.arg))
    extra.arg =  paste0(",", extra.arg)

  if (!is.null(extra.code)) {
    extra.code = paste0("\n  ",paste0(extra.code,collapse="\n  "))
  }
  estr = deparse1(e)
  if (is.assignment(e)) {
    var = deparse1(e[[2]])
    rhs = deparse1(e[[3]])
    call.name = name.of.call(e[[3]])

    if (call.name == "function") {
      rhs = deparse1(e[[3]], collapse="\n")
      code = paste0("hint.for.function(",var ,"<-",rhs, extra.arg,")",
                    extra.code)
    } else {
      code = paste0("hint.for.assign(",var ,"<-",rhs,extra.arg,")",
                    extra.code)
    }
  } else {
    code = paste0("hint.for.call(",estr,extra.arg,")", extra.code)
  }
  code
}

test.code.for.compute = function(code, var, extra.arg="") {
  restore.point("test.code.for.compute")
  code.txt = paste0("{\n", paste0(code, collapse="\n"),"\n",var,"\n}")
  test.txt = paste0("check.variable('", var,"',",code.txt,extra.arg,")")
  test.txt
}

hint.code.for.compute = function(code, var, extra.code = NULL) {
  restore.point("hint.code.for.compute")
  ec = parse.expr.and.comments(code, comment.start="## ")
  comments = lapply(ec$comments, function(str) {
    ret=gsub('"',"'",str, fixed=TRUE)
    if (length(ret)==0)
      ret=""
    ret
  })
  comment.code = paste0("list(",paste0('"',comments,'"', collapse=", "),")")

  code = paste0(code, collapse="\n")
  com = paste0("hint.for.compute({\n",code,"\n},",comment.code,", var= '",var,"'",
               extra.code,"\n)")
  com
}
