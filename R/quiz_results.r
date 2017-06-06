show.quiz.task.results = function(ct, app=getApp(),outputId = NS(ct$qu$id,"resultsUI"),clicker.tag="latest",...) {
  restore.point("show.quiz.task.results")
  dat = load.sub.data(ct=ct, clicker.tag=clicker.tag)

  if (is.null(dat)) {
    ui = p("No answers submitted.")
    setUI(outputId,ui)
    return()
  }

  qu = ct$qu
  part =qu$parts[[1]]
  if (isTRUE(part$type=="sc")) {
    show.clicker.quiz.sc.results(dat=dat,qu=qu,outputId = outputId)
  } else if (isTRUE(part$type=="mc")) {
    show.clicker.quiz.mc.results(dat=dat,qu=qu,outputId = outputId)
  } else if (isTRUE(part$type=="numeric")) {
    show.clicker.quiz.numeric.results(dat=dat,qu=qu,outputId = outputId)
  }


  return()
}

show.clicker.quiz.sc.results = function(dat, qu,part = qu$parts[[1]], show.sol=TRUE, outputId = NULL, opts=NULL,do.plot=TRUE, app=getApp()) {
  restore.point("clicker.quiz.sc.results.ui")

  choices = unlist(part$choices)
  var = names(dat)[[3]]
  counts = count.choices(dat[[var]], choices)
  shares = round(100*counts / max(1,sum(counts)))

  answer = if (show.sol) part$answer else NULL

  nans = NROW(dat)

  if (do.plot) {


    choice.labels = choices
    if (show.sol) {
      rows = choices == part$answer
      choice.labels[rows] = paste0("*", choice.labels[rows])
    }

    plot = choices.barplot(values=dat[[var]], choices, answer=answer, choice.labels=choice.labels)
    # need random string to correctly rerender plot
    plotId = paste0(outputId,"_Plot_",random.string(nchar=8))
    ui = tagList(
      div(style="height=14em",
        highchartOutput(plotId, height="14em")
      ),
      p(nans," replies.")
    )
    setUI(outputId,ui)
    dsetUI(outputId,ui)
    app$session$output[[plotId]] = renderHighchart(plot)
  } else {
    n = length(choices)
    bg.color = rep("#fff",n)
    if (show.sol) {
      rows = choices == part$answer
      bg.color[rows] = "#aaf"

    }
    df = data_frame(counts, paste0("(",shares,"%)"), choices)

    html = html.result.table(df,colnames=c("","","",""), font.size="120%", align=c("center","center","left"),bg.color = bg.color)

    ui = tagList(HTML(html))
    setUI(outputId, ui)
  }
  invisible(ui)
}


show.clicker.quiz.mc.results = function(dat, qu,part = qu$parts[[1]], show.sol=TRUE, outputId = NULL, opts=NULL,do.plot=TRUE, app=getApp()) {
  restore.point("clicker.quiz.mc.results.ui")

  choices = unlist(part$choices)

  sum = group_by(dat,answer) %>% summarize(yes=sum(checked),no=sum(!checked))


  ind = match(choices,sum$answer)
  sum = sum[ind,]
  yes = sum$yes
  no = sum$no

  nans = length(unique(dat$userid))
  #yes = lapply(yes, function(y) list(y=y, color="#777"))
  #no = lapply(yes, function(y) list(y=y, color="#d35400"))

  if (show.sol) {
    correct = choices %in% part$answer
    pre = post = ""
    #pre = ifelse(correct,"* ","")
    #post = ifelse(correct," (Answer: Yes)", " (Answer: No)")
    pre = ifelse(correct,"(A: Yes) ","(A: No) ")
    choices = paste0(pre, choices,post)
  }

  plot = highchart() %>%
  hc_chart(type = "column") %>%
  hc_plotOptions(
    column=list(
      dataLabels=list(enabled=TRUE)
    )
  ) %>%
  hc_xAxis(categories = choices) %>%
  hc_add_series(data = yes,name = "Yes", color="#2980b9") %>%
  hc_add_series(data = no,name = "No",color="#d35400")

  # need random string to correctly rerender plot
  plotId = paste0(outputId,"_Plot_",random.string(nchar=8))
  ui = tagList(
    div(style="height=14em",
      highchartOutput(plotId, height="14em")
    ),
    p("Total: ",nans," replies."),
    if (show.sol & !is.null(qu$explain.html))
      HTML(qu$explain.html)
  )
  setUI(outputId,ui)
  dsetUI(outputId,ui)
  app$session$output[[plotId]] = renderHighchart(plot)

  invisible(ui)
}



show.clicker.quiz.numeric.results = function(dat, qu,part = qu$parts[[1]], show.sol=TRUE, outputId = NULL, opts=NULL,do.plot=TRUE, app=getApp()) {
  restore.point("clicker.quiz.numeric.results.ui")
  answer = as.numeric(part$answer)
  var = names(dat)[[3]]
  val = dat[[var]]

  pos.dev = (val/answer-1)
  neg.dev = (answer/val-1)
  neg = neg.dev > pos.dev

  dev = pmax(pos.dev,neg.dev) * (-1)^neg


  res = relative.deviation.breaks(size="5")
  br = res$br
  lab = res$lab


  int = findInterval(dev, br)

  choices = lab
  values = lab[int]
  counts = rep(0, length(choices))
  names(counts) = choices
  cc = table(values)
  counts[names(cc)] = cc
  names(counts) = NULL


  plot = highchart() %>%
  hc_chart(type = "column") %>%
  hc_plotOptions(
    column=list(dataLabels=list(enabled=TRUE),colorByPoint = TRUE,colors=res$colors),
    colors=res$colors,
    series=list(
        pointPadding= 0,
        groupPadding= 0,
        borderWidth= 0.5,
        borderColor= 'rgba(255,255,255,0.5)'
    )
  ) %>%
  hc_xAxis(categories = lab) %>%
  hc_add_series(data = counts,name="Counts",showInLegend=FALSE)

  # need random string to correctly rerender plot
  plotId = paste0(outputId,"_Plot_",random.string(nchar=8))
  ui = tagList(
    p("Results from ",NROW(dat)," replies:"),
    div(style="height=14em",
      highchartOutput(plotId, height="14em")
    ),
    if (show.sol) p("Correct answer: ", answer)
  )
  setUI(outputId,ui)
  dsetUI(outputId,ui)
  app$session$output[[plotId]] = renderHighchart(plot)


  invisible(ui)
}


normalize.clicker.tag = function(ct, clicker.tag) {
  restore.point("normalize.clicker.tag")

  if (length(clicker.tag)==0) return(NULL)

  if ("none" %in% clicker.tag) return(NULL)

  if ("all" %in% clicker.tag | "latest" %in% clicker.tag) {
    dirs = get.clicker.tags(ct=ct)
    if ("all" %in% clicker.tag) return(dirs)
    if ("latest" %in% clicker.tag) {
      nums = na.omit(as.numeric(dirs))
      if (length(nums)>0) {
        clicker.tag = union(clicker.tag, as.character(max(nums)))
      } else {
        return(NULL)
      }
    }
  }
  clicker.tag

}

# transform submission data into simpler format
quiz.clicker.transform.sub.data = function(dat, ct) {
  restore.point("transform.sub.data")
  if (isTRUE(ct$type=="quiz")) {
    part = ct$qu$parts[[1]]
    if (isTRUE(part$type=="mc")) {
      dat = transform.to.mc.data(dat, part$choices)
    }
  }
  dat
}

transform.to.mc.data = function(dat, choices) {
  restore.point("transform.to.mc.data")
  choices = unlist(choices)

  library(tidyr)
  mc = expand.grid(answer=choices,userid=unique(dat$userid))
  jd = suppressWarnings(left_join(mc, dat,by=c("userid","answer")))
  jd$checked = !is.na(jd$submitTime)

  jd = select(jd,submitTime,userid,answer,checked)
  jd
}
