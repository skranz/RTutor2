# #< quiz
# parts:
#   - question: What is 20*20?
#     choices:
#         - 100
#         - 200
#         - 400*
#         - 500
#     multiple: FALSE
#     success: Great, your answer is correct!
#     failure: Try again.
#   - question: State pi up to 2 digits
#     answer: 3.14
#     roundto: 0.01
# #>



rtutor.widget.quiz = function() {
  list(
    clicker = rtutor.clicker.widget.quiz(),
    is.task = TRUE,
    parse.fun = rtutor.quiz.block.parse,
    make.org.task.state = rtutor.quiz.make.org.task.state,
    init.task.state = rtutor.quiz.init.task.state,
    init.handlers = rtutor.quiz.init.handlers,
    ui.fun = rtutor.quiz.shiny.ui,
    rmd.fun = rtutor.quiz.rmd
  )
}

rtutor.quiz.make.org.task.state = function(wid) {
  list(
    part.solved=rep(FALSE,length(wid$parts)),
    solved=FALSE,
    points=0,
    wid=wid
  )
}

rtutor.quiz.init.task.state = function(ts,ups, task.ind=ts$task.ind,...) {
  if (is.null(ups)) return(ts)
  ts$solved = ups$utt$was.solved[task.ind]
  ts$part.solved = rep(ts$solved)
  ts
}

rtutor.quiz.rmd = function(ts, ...) {
  sol.md = quiz.md(ts$wid, solution=TRUE)
  list(
    "rmd" =sol.md,
    "sol" = sol.md,
    "shown" = quiz.md(ts$wid, solution=FALSE)  
  )
}

rtutor.quiz.shiny.ui = function(ts, wid=ts$wid, ...) {
  restore.point("rtutor.quiz.shiny.ui")
  
  quiz.ui(wid, solution=isTRUE(ts$solved))  
}

rtutor.quiz.init.handlers = function(wid=ts$wid,ps=get.ps(), app=getApp(),ts=NULL,...) {
  add.quiz.handlers(qu=wid, quiz.handler=rtutor.quiz.handler)    
}

rtutor.quiz.block.parse = function(inner.txt,type="quiz",name="",id=paste0("addon__",type,"__",name),args=NULL, bdf=NULL, bi=NULL, ps=get.ps(),...) {
  restore.point("rtutor.quiz.block.parse")
  whiskers =NULL
  if (isTRUE(ps$opts$use.whiskers)) {
    whiskers = ps$pre.env$.whiskers
  }
  
  qu = shinyQuiz(id = id,yaml = merge.lines(inner.txt), bdf = NULL,add.handler = FALSE, whiskers=whiskers)
  qu
}

rtutor.quiz.handler = function(qu, part.solved, solved, ts=get.ts(qu$task.ind),app=getApp(),...) {
  restore.point("rtutor.quiz.handler")


      
  ts$solved = solved
  ts$part.solved = part.solved
  ts$points = (sum(part.solved) / length(part.solved))*qu$max.points
  process.checked.widget(ts=ts)
}

examples.quiz = function() {
    yaml = '
parts:
  - question: What is 20*20?
    choices:
        - 100
        - 200
        - 400*
        - 500
    multiple: FALSE
    success: Great, your answer is correct!
    failure: Try again.
  - question: State pi up to 2 digits
    answer: 3.14
    roundto: 0.01
award:
  title: Quiz master
  text: You solved the quiz!

  '
  app = eventsApp()

  qu = parse.quiz.yaml(yaml)
  qu$ui = quiz.ui(qu)
  app$ui = qu$ui
  add.quiz.handlers(qu)
  
  runEventsApp(app, launch.browser=rstudioapi::viewer)
  
}

quizDefaults = function(lang="en") {
  if (lang=="de") {
    list(
      success = "Richtig!",
      failure= "Leider noch nicht richtig.",
      success_color = "black",
      failure_color = "red",
      points_txt = "Punkte",
      point_txt = "Punkt"
    )
  } else {
    list(
      success = "Great, you answered correctly!",
      failure= "Sorry, not yet correct.",
      success_color = "black",
      failure_color = "red",
      points_txt = "points",
      point_txt = "point"
    )
  }
}

#' Create a shiny quiz widget
#'
#' @param id the id of the quiz
#' @param qu a list that contains the quiz fields as would have
#'        been parsed by read.yaml from package YamlObjects
#' @param yaml alternatively to qu, is yaml a string that specifies the quiz
#' @param quiz.handler a function that will be called if the quiz is checked.
#'        The boolean argument solved is TRUE if the quiz was solved
#'        and otherwise FALSE
shinyQuiz = function(id=paste0("quiz_",sample.int(10e10,1)),qu=NULL, yaml, blocks.txt=NULL, bdf=NULL, quiz.handler=NULL, add.handler=TRUE, defaults=quizDefaults(lang=lang), lang="en", whiskers=NULL,add.check.btn=TRUE) {
  restore.point("shinyQuiz")

  if (is.null(qu)) {
    yaml = enc2utf8(yaml)
    
    qu = try(mark_utf8(parse.hashdot.yaml(yaml)), silent=TRUE)
    if (is(qu,"try-error")) {
      err = paste0("When importing quiz:\n",paste0(yaml, collapse="\n"),"\n\n",as.character(qu))
      stop(err,call. = FALSE)
    }

  }

  if (is.null(qu[["id"]])) {
    qu$id = id
  }
  if (is.null(qu$parts)) {
    qu$parts = list(qu)
  }


  qu$checkBtnId = paste0(qu$id,"__checkBtn")
  qu$parts = lapply(seq_along(qu$parts), function(ind) init.quiz.part(qu$parts[[ind]],ind,qu,whiskers=whiskers,add.check.btn=add.check.btn))
  
  if (!is.null(qu$explain))
    qu$explain.html = md2html(qu$explain)
  
  
  np = length(qu$parts)
  
  qu$max.points = sum(sapply(qu$parts, function(part) part[["points"]]))
  
  qu$ui = quiz.ui(qu, add.check.btn=add.check.btn)

  if (add.handler)
    add.quiz.handlers(qu, quiz.handler)
  qu
}

init.quiz.part = function(part=qu$parts[[part.ind]], part.ind=1, qu, defaults=quizDefaults(), whiskers=list(), add.check.btn=TRUE) {
  restore.point("init.quiz.part")

  part = copy.into.missing.fields(dest=part, source=defaults)

  if (!is.null(part[["sc"]])) {
    part$choices = part$sc
    part$multiple = FALSE
    #part$type = "sc"
  } else if (!is.null(part[["mc"]])) {
    part$choices = part$mc
    part$multiple = TRUE
    #part$type = "mc"
  }


  if (!is.null(part$choices)) {
    correct.choices = which(str.ends.with(part$choices,"*"))
    if (is.null(part$multiple)) {
      part$multiple = length(correct.choices) != 1
    }
    part$correct.choices = correct.choices
    part$choices[correct.choices] = str.remove.ends(part$choices[correct.choices],right=1)
    
    part$choices = lapply(part$choices, replace.whiskers,values=whiskers)
    
    part$answer = unlist(part$choices[correct.choices])
    names(part$choices) =NULL
    if (part$multiple) {
      part$type = "mc"
    } else {
      part$type = "sc"
    }
  } else if (!is.null(part$answer)) {
    if (is.numeric(part$answer)) {
      part$type = "numeric"
      if (is.null(part$roundto)) part$roundto=1e-7
    } else {
      part$type = "text"
    }
  } else {
    stop(paste0("The quiz with question ", part$question, " has neither defined the field 'answer' nor the field 'choices'."))
  }

  if (is.null(part[["points"]])) {
    part$points = 1
  }
  part$question = md2html(replace.whiskers(part$question,values=whiskers))

  if (!is.null(part$explain))
    part$success = paste0(part$success,"\n\n", part$explain)
  
  txt = replace.whiskers(part$success,whiskers)
  
  if (part$points==1) {
    txt = paste0(txt," (", part$points, " ", defaults$point_txt,")")
  } else if (part$points > 0 ) {
    txt = paste0(txt," (", part$points, " ", defaults$points_txt,")")
  }
  txt = colored.html(txt, part$success_color)
  part$success =  md2html(text=txt, fragment.only=TRUE)

  if (!is.null(part$explain))
    part$failure = paste0(part$failure,"\n\n", part$explain)

  
  txt = replace.whiskers(part$failure, whiskers)
  txt = colored.html(txt, part$failure_color)
  
  
  part$failure =  md2html(text=txt, fragment.only=TRUE)

  part$id = paste0(qu$id,"__part", part.ind)
  part$answerId = paste0(part$id,"__answer")
  part$resultId = paste0(part$id,"__resultUI")
  part$ui = quiz.part.ui(part,add.button = add.check.btn & !is.null(part$checkBtnId))
  part$solved = FALSE

  if (is.null(part$points)) {
    part$points = 1
  }
  
  part
}

quiz.ui = function(qu, solution=FALSE, add.check.btn=TRUE) {
  restore.point("quiz.ui")
  pli = lapply(seq_along(qu$parts), function(i) {
    restore.point("quiz.ui.inner")
    
    part = qu$parts[[i]]
    if (i < length(qu$parts)) {
      hr = hr()
    } else {
      hr = NULL
    }

    if (solution) {
      if (is.null(part$sol.ui)) {
        part$sol.ui = quiz.part.ui(part, solution=TRUE)
      }
      setUI(part$resultId,HTML(part$success))

      return(list(part$sol.ui,hr))
    } else {
      return(list(part$ui,hr))
    }
  })
  if (!is.null(qu$checkBtnId) & add.check.btn) {
    ids = sapply(qu$parts, function(part) part$answerId)
    pli = c(pli, list(submitButton(qu$checkBtnId,label = "check",form.ids = ids),br()))
  }

  with.mathjax(pli)
}

quiz.part.ui = function(part, solution=FALSE, add.button=!is.null(part$checkBtnId)) {
  head = list(
    HTML(part$question)
  )
  if (solution) {
    if (part$type=="numeric") {
      answer = textInput(part$answerId, label = NULL,value = part$answer)
    } else if (part$type =="text") {
      answer = textInput(part$answerId, label = NULL,value = part$answer)
    } else if (part$type=="mc") {
      answer = checkboxGroupInput(part$answerId, label=NULL,part$choices,selected = part$answer)
    } else if (part$type=="sc") {
      answer = radioButtons(part$answerId, label=NULL,part$choices, selected=part$answer)
    }
    #setUI(part$resultId,HTML(part$success))

  } else {
    if (part$type=="numeric") {
      answer = textInput(part$answerId, label = NULL,value = "")
    } else if (part$type =="text") {
      answer = textInput(part$answerId, label = NULL,value = "")
    } else if (part$type=="mc") {
      answer = checkboxGroupInput(part$answerId, label=NULL,part$choices)
    } else if (part$type=="sc") {
      answer = radioButtons(part$answerId, label=NULL,part$choices, selected=NA)
    }
  }

  if (add.button) {
    button = submitButton(part$checkBtnId,label = "check", form.ids = part$answerId)
  } else {
    button = NULL
  }
  list(head,answer,uiOutput(part$resultId),button)
}

quiz.md = function(qu, solution=FALSE) {
  restore.point("quiz.md")
  li = lapply(seq_along(qu$parts), function(i) {
    part = qu$parts[[i]]
    quiz.part.md(part, solution=solution)
  })
  paste0(li, collapse="\n")
}


quiz.part.md = function(part, solution=FALSE) {
  restore.point("quiz.part.md")
  
  head = paste0("\nQuiz: ",part$question,"\n")
  if (solution) {
    if (part$type=="numeric" | part$type == "text") {
      answer = paste0("Answer: ", part$answer)
    } else if (part$type=="mc" | part$type=="sc") {
      ans = part$choices
      mark = rep("[ ]", length(ans))
      mark[ans %in% part$answer] =  "[x]"
      answer = paste0("- ", ans, " ", mark,"\n", collapse="\n")
    }
  } else {
    if (part$type=="numeric" | part$type == "text") {
      answer = "Answer: "
    } else if (part$type=="mc" | part$type=="sc") {
      ans = part$choices
      answer = paste0("- ", ans, "[   ]\n", collapse="\n")
    }
  }
  paste0(head,"\n", answer)
}

submitButton = function (inputId, label, icon = NULL, width = NULL, form.ids = NULL, form.sel = ids2sel(form.ids), ...) {
  restore.point("submitButton")
  
  actionButton(inputId,label,icon, width, "data-form-selector"=form.sel)
}

add.quiz.handlers = function(qu, quiz.handler=NULL, id=qu$id){
  restore.point("add.quiz.handlers")
  cat("\n**************************************")
  cat("\nadd.quiz.handlers...")
  cat("\n**************************************")
  app = getApp()
  if (is.null(app)) {
    cat("\nCannot add quiz handlers since no shinyEvents app object is set.")
    return()
  }

  buttonHandler(qu$checkBtnId,fun = click.check.quiz, qu=qu, quiz.handler=quiz.handler)
}

check.quiz.part = function(part.ind,qu, values=NULL, app=getApp()) {
  part = qu$parts[[part.ind]]
  answer = getInputValue(part$answerId)
  #answer = values[[part$answerId]]
  restore.point("check.quiz.part")

  if (part$type =="numeric") {
    answer = as.numeric(answer)
    correct = is.true(abs(answer-part$answer)<part$roundto)
  } else {
    correct = setequal(answer,part$answer)
  }
  if (correct) {
    #cat("Correct!")
    setUI(part$resultId,HTML(part$success))
  } else {
    #cat("Wrong")
    setUI(part$resultId,HTML(part$failure))
  }
  return(correct)
}

click.check.quiz = function(app=getApp(), qu, quiz.handler=NULL, formValues, ...) {
  restore.point("click.check.quiz")
  part.solved = sapply(seq_along(qu$parts), check.quiz.part, qu=qu,app=app, values=formValues) 
  solved = all(part.solved)
  if (!is.null(quiz.handler)) {
    quiz.handler(app=app, qu=qu, part.solved=part.solved, solved=solved)
  }
  solved
}
