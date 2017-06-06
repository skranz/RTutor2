rtutor.clicker.widget.quiz = function() {
  list(
    #server.ct.nowrite.fields = "wid",
    parse.fun = quiz.clicker.parse,
    server = list(
      init.handlers = NULL,
      init.ct = NULL,
      start.ct = NULL,
      load.sub.data = NULL,
      transform.sub.data = "quiz.clicker.transform.sub.data",
      ui.fun = "quiz.clicker.server.ui.fun",
      show.results = "quiz.clicker.server.show.results"
    ),
    client = list(
      init.handlers = "quiz.clicker.client.init.handlers",
      init.ct = NULL,
      init.cc = NULL,
      ui.fun = NULL
    )
  )
}

quiz.clicker.server.ui.fun = function(wid,ct=NULL,...,app=getApp(), opts=rt.opts()) {
  default.clicker.server.ui.fun(wid=wid, above.ui=wid$ui, stop.in=first.non.null(opts$clicker.stop.in,5))  
}

quiz.clicker.server.show.results = function(ct, ...) {
  show.quiz.task.results(ct=ct)  
}

quiz.clicker.parse = function(inner.txt,type="quiz",name="",id=paste0("quiz_",bi),args=NULL, bdf=NULL, bi=NULL, ps=get.ps(),opts = ps$opts,...) {
  task.id = paste0(ps$name,"__",id)
  restore.point("quiz.clicker.parse")
  whiskers =NULL
  if (isTRUE(opts$use.whiskers)) {
    whiskers = ps$pre.env$.whiskers
  }

  qu = shinyQuiz(id = task.id,yaml = merge.lines(inner.txt), bdf = NULL,add.handler = FALSE, whiskers=whiskers, add.check.btn=FALSE)
  client.ui = quiz.clicker.client.ui(qu)
  qu$ct = list(
    type = "quiz",
    task.id = task.id,
    client.ui = client.ui,
    qu = qu
  )
  qu
}


quiz.clicker.client.ui = function(qu) {
  restore.point("quiz.clicker.client.ui")
  pli = lapply(seq_along(qu$parts), function(i) {
    restore.point("quiz.clicker.client.ui")
    part = qu$parts[[i]]
    part.ui = quiz.clicker.client.part.ui(part)
    if (i < length(qu$parts)) {
      hr = hr()
    } else {
      hr = NULL
    }
    return(list(part.ui,hr))
  })
  if (!is.null(qu$checkBtnId)) {
    ids = sapply(qu$parts, function(part) part$answerId)
    pli = c(pli, list(submitButton(qu$checkBtnId,label = "Send",form.ids = ids),br()))
  }

  withMathJax(pli)
}

quiz.clicker.client.part.ui = function(part) {
  restore.point("quiz.clicker.client.part.ui")
  
  head = list(
    HTML(part$question)
  )
  if (part$type=="numeric") {
    answer = textInput(part$answerId, label = NULL,value = "")
  } else if (part$type =="text") {
    answer = textInput(part$answerId, label = NULL,value = "")
  } else if (part$type=="mc") {
    answer = wellCheckboxGroupInput(part$answerId, label=NULL,part$choices)
  } else if (part$type=="sc") {
    answer = wellRadioButtons(part$answerId, label=NULL,part$choices, selected=NA)
  }
  list(head,answer,uiOutput(part$resultId))
}

quiz.clicker.client.init.handlers = function(ct=NULL,qu=ct$qu){
  restore.point("quiz.clicker.client.init.handlers")
  buttonHandler(qu$checkBtnId, function(...) {
    part = qu$parts[[1]]
    answer = getInputValue(part$answerId)
    clicker.submit(values=list(answer=answer))
  })
}

