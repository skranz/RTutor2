data.explorer.ui = function() {
  ui = tagList(
    uiOutput("radioDataExplorerUI"),
    tabsetPanel(
      tabPanel("Data",DT::dataTableOutput("tableDataExplorer")),
      tabPanel("Description",uiOutput("variablesDescrUI")),
      #tabPanel("Summary", uiOutput("dataSummariseUI")),
      tabPanel("Plot", uiOutput("dataPlotUI"))
    )
  )
  ui
}

data.explorer.init.handlers = function() {
  radioBtnGroupHandler("radioDataExplorer", function(value,...,ps=get.ps()) {
    update.data.explorer.data(var=value)
  })
}

update.data.explorer.ui = function(ps=get.ps(), session=ps$session) {
  restore.point("update.data.explorer.ui")
  ts = get.ts()
  #if (is.null(ts)) return()
  task.env = get.task.env(ts=ts)

  vars = get.environment.data.var(task.env)


  if (length(vars)>0) {
    dsetUI("radioDataExplorerUI",
      radioBtnGroup(id="radioDataExplorer",labels=vars, values=vars)
    )
    var = vars[1]
    cat("\nupdate for var ", var)
    update.data.explorer.data(var, env=task.env)
  } else {
    setUI("radioDataExplorerUI", HTML("The current task environment has no data frames or matrixes.")
)
    setUI("variablesDescrUI",NULL) 
    updateDataTable(session,"tableDataExplorer",NULL) 
  }
}

update.data.explorer.data  = function(var, env=get.task.env(ps$task.ind), ps=get.ps(), session=app$session, app=getApp()) {
  restore.point("update.data.explorer.data")

  data = set.data.explorer.data(var,env = env)
  updateDataTable(session,"tableDataExplorer",signif.cols(data,4),
      options = list(orderClasses = TRUE,
                     lengthMenu = c(5, 10, 25,50,100),
                     pageLength = 5)) 
  setUI("variablesDescrUI",make.var.descr.ui(data)) 
  setUI("dataPlotUI",data.plot.ui(data)) 
  
}


get.environment.data.var=function(env=get.task.env(ps$task.ind), ps=get.ps()) {
  vars = ls(env)
  dvars = unlist(lapply(vars, function(var) {
    x = env[[var]]
    if(is.matrix(x) | is.data.frame(x))
      return(var)
    return(NULL)
  }))
  dvars
}

set.data.explorer.data  = function(var=NULL, env = get.task.env(ps$task.ind), ps=get.ps()) { 
  if (is.null(var)) {
    var = getInputValue("radioDataExplorer")
  }
  restore.point("set.data.explorer.data")
  ret = NULL
  if (length(var)>0 & is.character(var)) {
    if (exists(var, env)) {
      ret = get(var,env)
      if (is.matrix(ret)) {
        ret = as.data.frame(ret)
      }
    }
  }
  set.plugin.state("dataexplorer",list(var=var, data=ret))
  return(invisible(ret))
}

data.summarise.ui = function(data,ps=get.ps()) {
  if (is.null(data))
    return(NULL)
  
  vars = colnames(data)
  group_by_ui = selectizeInput("groupByInput",label="group by", choices=vars, multiple=TRUE)
  col_select_ui = selectizeInput("colSelectInput",label="variables", choices=vars, multiple=TRUE) 

  changeHandler("groupByInput",update.data.explorer.summarise)
  changeHandler("colSelectInput",update.data.explorer.summarise)
  update.data.explorer.summarise(data)

  tagList(
    group_by_ui,
    col_select_ui,
    DT::dataTableOutput("tableDataSummarise")
  )
  
}

update.data.explorer.summarise = function(data=get.plugin.state("dataexplorer")$data,ps = get.ps(), session=app$session,app=getApp(),...) {
  restore.point("update.data.explorer.summarise")
  updateDataTable(session,"tableDataSummarise",{
    #browser()
    cat("\ntrigger new summarise...")
    groups = getInputValue("groupByInput")
    if (length(groups)>0)
      data = s_group_by(data, groups)
    cols =  getInputValue("colSelectInput")
    if (length(cols)==0)
      cols = colnames(dat)
        
    out = xsummarise_each_(dat,funs(xmean,xsd,xmax,xmin), cols)
    out
  }, options = list(lengthMenu = c(5, 10, 25,50,100),pageLength = 25)
  )
}


data.plot.ui = function(data,session=app$session, ps=get.ps(), app=getApp()) {
  return(NULL)
  if (is.null(data))
    return(NULL)
  
  vars = c("",colnames(data))
  
  if (FALSE) {
    plot_type_ui = selectizeInput("plotTypeInput",label="plot type", choices=c("point","line"), multiple=FALSE)
    xvar_ui = selectizeInput("xvarInput",label="x", choices=vars, multiple=FALSE)
    yvar_ui = selectizeInput("yvarInput",label="y", choices=vars, multiple=FALSE)
    xfacet_ui = selectizeInput("xfacetInput",label="x facet", choices=vars, multiple=FALSE)
    yfacet_ui = selectizeInput("yfacetInput",label="y facet", choices=vars, multiple=FALSE)
  
    colorvar_ui = selectizeInput("colorvarInput",label="color group", choices=vars, multiple=FALSE)
    
    buttonHandler("showPlotBtn",update.data.explorer.plot)
    
    chunk.fluidRow(
      chunk.fluidRow(
        column(4,plot_type_ui,colorvar_ui),
        column(4,xvar_ui,xfacet_ui),
        column(4,yvar_ui,yfacet_ui)
      ),
      actionButton("showPlotBtn","Show Plot"),
      plotOutput("plotData")
    )
  }
  
  vars = c(colnames(data))  
  xvar_ui = selectizeInput("xvarInput",label="x", choices=vars, multiple=FALSE)
  yvar_ui = selectizeInput("yvarInput",label="y", choices=c("_none_",vars), multiple=FALSE)
  groupby_ui = selectizeInput("groupbyInput",label="group by", choices=vars, multiple=FALSE)
  buttonHandler("showPlotBtn",update.data.explorer.plot)
  
  chunk.fluidRow(
    chunk.fluidRow(
      column(3,xvar_ui),
      column(3,yvar_ui),
      #column(3,groupby_ui),
      column(3,actionButton("showPlotBtn","Show"))
    ),
    plotOutput("plotData")
  )
  
  
}


update.data.explorer.plot = function(dat=ps$de.dat,input=ps$session$input,ps = get.ps(),...) {
#   updatePlot(ps$session,"plotData", {
#     code = isolate(make.ggplot.code(data.name="dat",type=input$plotTypeInput, xvar=input$xvarInput, yvar=input$yvarInput, colorVar = input$colorvarInput))
#     eval(code)
#   })
  x = input$xvarInput
  y = input$yvarInput
  if (isTRUE(y=="_none_")) y = NULL
  #by = input$groupbyInput
  by = NULL
  restore.point("update.data.explorer.plot")
  setPlot("plotData",
    DescBy(x=x,y=y,by=by,data=dat,plotit=TRUE)  
  )
  
}


make.ggplot.code = function(data.name="dat",type="line",xvar="",yvar="",colorvar="",xfacet="",yfacet="",...) {
  restore.point("make.ggplot.code")
  li = c(x=xvar,y=yvar,color=colorvar)
  str = NULL
  for (i in seq_along(li)) {
    if (nchar(li[[i]])>0) {
      str = paste0(str,",", names(li)[i],"=",li[i])
    }        
  }
  geom = type
  
  com = paste0("qplot(data=",data.name,str,",geom='",geom,"')")
  base::parse(text=com,srcfile=NULL)  
}

old.make.var.descr.ui = function(dat) {
 
  restore.point("make.var.descr.html")
  dt = get.var.descr.dt(dat=dat)

  if (!is.null(dt)) {
    dupl = duplicated(dt$var)
    dt = dt[!dupl,]
    title =  paste0(dt$var,": ", dt$descr)
  } else {
    title = colnames(dat)
  }
  body = lapply(dat, function(v) {
    HTML(var.summary.html(v))
  })
  panels = lapply(seq_along(title), function(i) {
    bsCollapsePanel(title[i], body[[i]])    
  })
  do.call(bsCollapse, c(panels,list(id="dataExplorerVarDescrCollapse")))
  #changeHandler("dataExplorerVarDescrCollapse",change.var.descr.collapse)
}

make.var.descr.ui = function(dat) {
 
  restore.point("make.var.descr.html")
  dt = get.var.descr.dt(dat=dat)

  if (!is.null(dt)) {
    dupl = duplicated(dt$var)
    dt = dt[!dupl,]
    title =  paste0(dt$var,": ", dt$descr)
  } else {
    title = colnames(dat)
  }
  
  dyncoll = dynCollapse(id="dataExplorerVarDescrCollapse",labels=paste0(colnames(dat),": ", dt$descr), values=colnames(dat), panel.fun = open.var.descr.collapse)
  #changeHandler("dataExplorerVarDescrCollapse",change.var.descr.collapse)
}

open.var.descr.collapse = function(collapseValue,app=getApp(), ps=get.ps(), ...) {
  args = list(...)
  restore.point("open.var.descr.collapse")
  cat("open.var.descr.collapse")
  col = collapseValue
  dat = ps$de.dat
  v = dat[[col]]
  html = HTML(var.summary.html(v))
  if (require(DescTools)) {
    id = paste0("dataExplorer__",col)
    plotUI = plotOutput(id, height="300px")
    setPlot(id, Desc(v,plotit=TRUE, main=""))
    return(list(html, plotUI))
  }
  return(html)
}


var.summary.html = function(v,...) {
  restore.point("var.summary.html ")
  #return("")
  
  library(hwriter)
  dli = describe.var(v)  
  lab1 = paste0(names(dli)[1:3],":")
  val1 = as.character(dli[1:3])

  use.top = length(v)<1000
  use.top = TRUE
  if (!use.top) {
    if (length(dli)>3) {
      dli = c(dli,list("","",""))
      lab2 = paste0("  ",names(dli)[4:6],":")
      val2 = as.character(xsignif(dli[4:6],4))
      df = data.frame(lab1,val1,lab2,val2)
      colnames(df)=c("",""," Summary","         ")
    } else {
      df = data.frame(lab1,val1)
      colnames(df)=c("","")
    }
  } else {
    
    
    top = get.top.x.obs(v,3)
    top.var = c(as.character(top$var), rep("",3))
    top.share = c(paste0(round(top$share*100,1),"%"),rep("",3))
    
    if (length(dli)>3) {
      dli = c(dli,list("","",""))
      lab2 = paste0("  ",names(dli)[4:6],":")
      val2 = as.character(xsignif(dli[4:6],4))
      lab3 = paste0("  ",1:3, ": ",top.var[1:3])
      val3 = top.share[1:3]
      df = data.frame(lab1,val1,lab2,val2,lab3,val3)
      colnames(df)=c("",""," Summary","         ","Most common values","Share")
    } else {
      lab3 = paste0("  ",top.var[1:3])
      val3 = top.share[1:3] 
      df = data.frame(lab1,val1,lab3,val3)
      colnames(df)=c("","","value","share")
    }
  }
  out = hwrite(df, NULL, border=0, row.names=FALSE, cellpadding=10)
  out
}

xround = function(x,digits=0) {
  if (is.list(x))
    return(lapply(x, xround, digits=digits))
  if (is.numeric(x))
    x = round(x, digits)
  x
}


xsignif = function(x,digits=6) {
  if (is.list(x))
    return(lapply(x, xsignif, digits=digits))
  if (is.numeric(x))
    x = signif(x, digits)
  x
}

xmean = function(..., na.rm=TRUE) {
  tryCatch(suppressWarnings(mean(..., na.rm=na.rm)),
           error = function(e){return(NA)})
}
xmax = function(..., na.rm=TRUE) {
  tryCatch(suppressWarnings(max(..., na.rm=na.rm)),
           error = function(e){return(NA)})
}
xmin = function(..., na.rm=TRUE) {
  tryCatch(suppressWarnings(min(..., na.rm=na.rm)),
           error = function(e){return(NA)})
}
xsd = function(..., na.rm=TRUE) {
  tryCatch(suppressWarnings(sd(..., na.rm=na.rm)),
           error = function(e){return(NA)})
}


DescBy = function(x,y=NULL,by=NULL, data, plotit=FALSE, catit=TRUE) {
  restore.point("DescBy")
  if (length(y)==0) y=NULL
  if (isTRUE(nchar(y)==0)) y=NULL
  if (length(by)==0) by=NULL
  if (isTRUE(nchar(by)==0)) by=NULL
  
  use.value = FALSE
  if (is.character(x)) {
    if (is.null(y)) {
      use.value = TRUE
    } else {
      form.str = paste0(paste0(x, collapse="+"),"~", paste0(y,collapse="+"))
      form = as.formula(form.str)
    }
  } else {
    form = x
  }
  if (is.null(by)) {
    if (use.value) {
      value = data[[x]]
      txt = capture.output(Desc(value, main=x, plotit=plotit))
    } else {
      txt = capture.output(Desc(form, data=data, plotit=plotit))
    }
    #Desc(driver ~ operator, data=d.pizza, plotit=TRUE)
  } else {
    txt = NULL
    col = by
    vals = setdiff(unique(d[[col]]),NA)
    
    for (val in vals) {
      if (use.value) {
        value = data[data[[col]]==val,,drop=FALSE][[x]]
        txt = capture.output(Desc(value, main=x, plotit=plotit))
      } else {
        df = data[data[[col]]==val,,drop=FALSE]
        str = capture.output(Desc(form, data=df, plotit=plotit))
      }
      txt = c(
        txt,
        paste0("\n\n", col," = ", val,"\n"),
        str
      )
    }
    
  }
  if (catit) {
    cat("\n",paste0(txt,collapse="\n"))
  }
  invisible(txt)
}
