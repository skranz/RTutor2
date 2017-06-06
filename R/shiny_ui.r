make.view.ui = function(view.ind, ps=get.ps()) {
  restore.point("make.view.ui")  
  shiny.dt = ps$shiny.dt
  cdt = ps$cdt

  if (view.ind==1) {
    rows = which(shiny.dt$view.ind == view.ind | shiny.dt$view.ind == 0)
  } else {
    rows = which(shiny.dt$view.ind == view.ind)      
  }
  ui.li = lapply(rows, function(i) {
    #restore.point("hdkgkdhighoifhg")
    if (shiny.dt$type[i]=="chunk") {
      chunk.ind = which(cdt$chunk.name==shiny.dt$chunk.name[i])
      ui=make.initial.chunk.ui(chunk.ind)
      
      award.name = cdt$award.name[chunk.ind]
      if (!is.na(award.name)) {
        award.ui.id = get.award.ui.id(award.name)
        return(list(ps$cdt$ui[[chunk.ind]],uiOutput(award.ui.id)))
      }
      return(ps$cdt$ui[[chunk.ind]])
    } else if (shiny.dt$type[i]=="widget") {
      wid = ps$rps$widgets[[ shiny.dt$widget.id[i] ]]
      Widget = ps$rps$Widgets[[wid$rta$type]]
      html = Widget$shiny.ui.fun(wid)
      
      row = which(ps$rps$wid.dt$id == wid$rta$id)
      award.name = ps$rps$wid.dt$award.name[row]
      if (!is.na(award.name)) {
        award.ui.id = get.award.ui.id(award.name)
        return(list(mathJaxRTutor(html),uiOutput(award.ui.id)))
      }
      return(mathJaxRTutor(html))      
    } else {
      #return(shiny.dt$html[[i]])

      return(mathJaxRTutor(shiny.dt$html[[i]]))
    }
  })
  
  ui.li = adapt.view.li.for.notes(ui.li,shiny.dt, rows)

  
  #do.call("fluidRow", ui.li)
  w = 12-ps$left.margin-ps$right.margin
  my.ui = do.call("column", c(list(width=w, offset=ps$left.margin),ui.li))
  fluidRow(my.ui)
}

# Make the default ui for each view and add it view.ui.li to ps
make.view.ui.li = function(view.inds = NULL,ps=get.ps()) {
  
  restore.point("make.view.ui.li")  
  shiny.dt = ps$shiny.dt
  if (is.null(view.inds))
    view.inds = setdiff(unique(ps$shiny.dt$view.ind),0)
  
  #make.view.ui(1)
  view.ui.li = lapply(view.inds, make.view.ui) 
  ps$view.ui.li = view.ui.li
  invisible(view.ui.li)  
}

make.ex.ui = function(ex.ind, ps = get.ps(), session=ps$session, view.in.container=isTRUE(ps$view.in.container)) {
  restore.point("make.ex.ui")
  shiny.dt = ps$shiny.dt
  cdt = ps$cdt  

  if (ex.ind==1) {
    rows = which(shiny.dt$ex.ind == ex.ind | shiny.dt$ex.ind == 0)
  } else {
    rows = which(shiny.dt$ex.ind == ex.ind)      
  }
  view.inds = unique(shiny.dt$view.ind[rows])
  ex.name = ps$edt$ex.name[ex.ind]
  
  if (view.in.container) {
    li = lapply(view.inds, function(view.ind) {
      outputName = paste0("viewUI",view.ind)
      uiOutput(outputName)
    })
  } else {
    li = ps$view.ui.li[view.inds]
  }
  
  
  # Button for next exercise
  if (ex.ind < max(ps$cdt$ex.ind)) {
    btnId = paste0("nextExBtn", ex.ind)
    nextExBtn = actionButton(btnId,"Go to next exercise...")
    li = c(li, list(nextExBtn))    
    buttonHandler(btnId, ex.ind=ex.ind, function(session,ex.ind,...) {
      cat("\nnextExBtn pressed...")
      updateTabsetPanel(session, inputId="exTabsetPanel", selected = paste0("exPanel",ex.ind+1))
    })
  }
  
  do.call("tabPanel", 
    c(list(title=ex.name, value=paste0("exPanel",ex.ind)), li)
  )
}

adapt.view.li.for.notes = function(view.li, shiny.dt, rows) {
  note.inds = setdiff(unique(shiny.dt$note.ind[rows]),0)
  if (length(note.inds)==0) return(view.li)
  
  restore.point("adapt.view.li.for.notes")  
  remove.rows = NULL
  note.ind = 1
  for (note.ind in note.inds) {
    nrows =  which(shiny.dt$note.ind[rows] == note.ind)
    note.name = shiny.dt$note.label[rows[nrows[1]]]
    
    #nli = view.li[nrows]
    collapseId = paste0("collapse_note_",note.ind)
    collapsePanelId = paste0("collapse_panel_note_",note.ind)
    panel = do.call("bsCollapsePanel", 
                    c(list(title=note.name, value =  collapsePanelId),
                      view.li[nrows]))    
    ui = bsCollapse(open = NULL, id = collapseId, panel)
    view.li[[nrows[1]]] = ui 
    remove.rows = c(remove.rows, nrows[-1])
  }
  if (length(remove.rows)>0)
    view.li = view.li[-remove.rows]
  view.li
}

make.ex.ui.li = function(ex.inds = NULL, ps = get.ps()) {
  restore.point("make.ex.ui.li")
  shiny.dt = ps$shiny.dt
  cdt = ps$cdt 
  edt = ps$edt
  
  if (is.null(ex.inds)) {
    ex.inds = setdiff(unique(edt$ex.ind),0)
    if (!is.null(ps$shiny.ex.inds))
      ex.inds = intersect(ex.inds, ps$shiny.ex.inds)    
  }
 
  ps$ex.ui.li = lapply(ex.inds, make.ex.ui)
  invisible(ps$ex.ui.li)
}

make.rtutor.page.ui = function(inner, ps = get.ps(), title="RTutor") {
  
  # WARNING: If highlightjs cannot be loaded, whole problem set
  # fails to work (very hard to detect bug)
  # Link to local highlightjs version
  dir = paste0(system.file('www', package='RTutor2'),"/highlightjs")
  addResourcePath('highlightjs', paste0(system.file('www', package='RTutor2'),"/highlightjs"))

  ret = navbarPage(title, header=
    tags$head(
      tags$script(src = 'highlightjs/highlight.min.js',
                  type = 'text/javascript'),
      tags$script(src = 'highlightjs/languages/r.min.js',
                  type = 'text/javascript'),
      tags$link(rel = 'stylesheet', type = 'text/css',
                href = 'highlightjs/styles/github.min.css')
    ),
    tabPanel(ps$ps.name, mathJaxRTutor(inner))
  )
  
}

highlight.code.script = function() {
  tags$script("$('pre code.r').each(function(i, e) {hljs.highlightBlock(e)});")
}

rtutor.html.ressources = function() {
  # WARNING: If highlightjs cannot be loaded, whole problem set
  # fails to work (very hard to detect bug)
  # Link to local highlightjs version
  dir = paste0(system.file('www', package='RTutor2'),"/highlightjs")
  addResourcePath('highlightjs', paste0(system.file('www', package='RTutor3'),"/highlightjs"))

  tagList(
    tags$head(tags$link(rel = 'stylesheet', type = 'text/css',href = 'highlightjs/styles/mycode.css')),
    tags$head(tags$script(src = 'highlightjs/highlight.min.js',type = 'text/javascript')),
    tags$head(tags$script(src = 'highlightjs/languages/r.min.js',type = 'text/javascript'))
    #tags$head(tags$script(src = 'highlightjs/highlight.pack.js',type = 'text/javascript')),
    #tags$head(tags$script("hljs.initHighlightingOnLoad();"))
    
#     HTML('
#     <link rel="stylesheet" href="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.1.0/styles/default.min.css">
# <script src="//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.1.0/highlight.min.js"></script>
# <script>hljs.initHighlightingOnLoad();</script>
#    ')
    
  )
}

make.rtutor.ui = function(shiny.dt = ps$shiny.dt,cdt=ps$cdt, ps=get.ps(), just.inner=FALSE) {
  restore.point("make.rtutor.ui")
  
  view.ui.li = make.view.ui.li(ps=ps)
  ex.ui.li = make.ex.ui.li(ps=ps)
  #shown.html(view.ui.li[[3]])
  
  li = list()
  if (isTRUE(ps$show.data.exp)) {
    li[[length(li)+1]] = tabPanel("Data Explorer",value="dataExplorerTabPanel", data.explorer.ui())
  }
  li[[length(li)+1]] = tabPanel(" ",value="statsPanel", uiOutput("uiProblemSetStats"), icon=icon(name="tasks", lib="font-awesome"))

  if (isTRUE(ps$show.load.savel.panel)) {
    li[[length(li)+1]] = tabPanel("",value="loadSaveTabPanel", load.save.ui(), icon=icon(name="folder-open", lib="font-awesome"))
  }
  if (isTRUE(ps$show.export.panel)) {
    li[[length(li)+1]] = tabPanel("",value="exportTabPanel", export.ui(), icon=icon(name="download", lib="font-awesome"))
  }

  
  doc=  do.call("tabsetPanel", c(
      list(id="exTabsetPanel"),ex.ui.li,li
  ))

  inner = doc 
  if (just.inner) return(inner)
  
  ret = make.rtutor.page.ui(inner,ps=ps)
  
  return(ret)
}

# Show a view ui
show.view.ui = function(view.ind, ps = get.ps(), session=ps$session) {
  restore.point("show.view.ui")
  if (view.ind==3)
    restore.point("show.view.ui.3")
    
  id = paste0("viewUI",view.ind)
  ui = ps$view.ui.li[[view.ind]]
  #browser()
  updateUI(session,id, ui)
}

get.view.ui.of.ex = function(ex.ind, ps=get.ps()) {
  restore.point("get.view.ui.of.ex")
  
  if (ex.ind==1) {
    rows = which(ps$shiny.dt$ex.ind == ex.ind | ps$shiny.dt$ex.ind == 0)
  } else {
    rows = which(ps$shiny.dt$ex.ind == ex.ind)      
  }
  view.inds = setdiff(unique(ps$shiny.dt$view.ind[rows]),0)
  ps$view.ui.li[view.inds]
}

show.view.ui.of.ex = function(ex.ind, ps = get.ps()) {
  restore.point("show.view.ui.of.ex")
  
  if (ex.ind==1) {
    rows = which(ps$shiny.dt$ex.ind == ex.ind | ps$shiny.dt$ex.ind == 0)
  } else {
    rows = which(ps$shiny.dt$ex.ind == ex.ind)      
  }
  view.inds = setdiff(unique(ps$shiny.dt$view.ind[rows]),0)
  for (view.ind in view.inds)
    show.view.ui(view.ind, ps)
}

show.ex.ui = function(ex.ind, ps=get.ps(), view.in.container=isTRUE(ps$view.in.container)) {
  restore.point("show.ex.ui")
  if (!view.in.container)
    show.view.ui.of.ex(ex.ind)
  chunk.inds = which(ps$cdt$ex.ind == ex.ind)
  for (chunk.ind in chunk.inds) {
    update.chunk.ui(chunk.ind)
  }
}