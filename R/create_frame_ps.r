examples.frame.ps = function() {
  library(EconCurves)
  setwd("D:/libraries/RTutor2")
  txt = readLines("ex1.Rmd")
  frame.ind = NULL
  te$lang = "de"
  te = rtutor.make.frame.ps.te(txt, bdf.filter=bdf.frame.filter(frame.ind=frame.ind),catch.errors = FALSE)
  bdf = te$bdf
  show.frame.ps(te)
  
  ui = make.te.ui(te=te)
  view.html(ui=ui)
  
}

rtutor.make.frame.ps.te = function(txt,addons="quiz",bdf.filter = NULL,dir=getwd(), figure.dir=paste0(dir,"/figure"),catch.errors=TRUE,ps.id = "",opts=default.ps.opts(),...) {
  restore.point("rtutor.make.frame.ps.te")

  te = new.env()
  te$addons = addons
  te$Addons = make.addons.list(addons)
  te$dir = dir
  te$figure.dir = figure.dir
  te$opts = opts
  
  if (length(txt)==1)  
    txt = sep.lines(txt)

  Encoding(txt) = "UTF8"
  txt = mark_utf8(txt)

  
  
  # Only capture elements between the lines <!-- START --> and <!-- END -->
  res = rmd.between.start.end.lines(txt,return.start.end = TRUE)
  te$txt.start = res$start; te$txt.end = res$end
  txt = res$txt

  txt = fast.name.rmd.chunks(txt)
  te$txt = txt
  
  dot.levels = rtutor.dot.levels()
  df = find.rmd.nested(txt, dot.levels)
  df = adapt.for.back.to.blocks(df,te=te)
  df$stype = df$type
  df$is.addon = df$type %in% addons
  
  
  df$parent_addon = get.levels.parents(df$level, df$is.addon)
  parent.types = c("frame","row", "column","chunk","preknit","precompute","knit","compute","info")
  pt = get.levels.parents.by.types(df$level, df$type, parent.types)
  bdf = cbind(data.frame(index = 1:NROW(df)),df,pt) %>% as_data_frame
  bdf$obj = bdf$ui = bdf$inner.ui = vector("list", NROW(bdf))
  bdf = mutate(bdf,
    stype.ind = 0,
    id = paste0(type,"__",index,"__",ps.id),    
    has.handler = FALSE,
    is.task = FALSE,task.ind = 0,
    prefixed=FALSE,
    is.container=FALSE,container.ind = 0, always.reload=FALSE,
    div.id = "",output.id=  "",   
    
 
    shown.rmd = "", out.rmd = "",sol.rmd = ""

  )
  
  
  # Filter bdf if only a subset of elements shall be compiled / shown
  if (!is.null(bdf.filter)) {
    bdf = bdf.filter(bdf=bdf, te=te)
    bdf = shorten.bdf.index(bdf)
    #bdf$index = 1:NROW(bdf)
  }

  te$bdf = bdf
  
  # Create env
  ps.baseenv = new.env(parent=parent.env(globalenv()))
  te$pre.env = ps.baseenv
  te$env = new.env(parent=parent.env(globalenv()))
  

  
  # Go through blocks and chunks, ordered by end
  binds = order(bdf$end, -bdf$start)
  bi = binds[1]
  for (bi in binds) {
    restore.point("inner.make.te")
    if (catch.errors) {
      res = try(rtutor.parse.block(bi,te), silent=TRUE)
      if (is(res,"try-error")) {
        br = te$bdf[bi,]
        msg = paste0("Error when parsing ",br$type," block lines ", br$start+te$txt.start-1, " to ", br$end+te$txt.start-1,"\n\n", paste0(as.character(res), collape="\n"))
        stop(msg)
      }
    } else {
      res = rtutor.parse.block(bi,te)  
    }
  }
  te$bdf$task.ind = cumsum(te$bdf$is.task) * te$bdf$is.task
  # store task.chunks in a list ck.li and the
  # corresponding user chunks in uk
  chunk.rows = which(te$bdf$stype == "task_chunk")
  te$bdf$stype.ind[chunk.rows] = seq_along(chunk.rows)
  te$ck.li = lapply(chunk.rows, function(bi) {
    te$bdf$obj[[bi]]$ck
  })
  te$org.uk.li = lapply(te$ck.li, function(ck) {
    make.user.chunk(ck)
  })
  
  # specify containers
  te$bdf$container.ind = cumsum(te$bdf$is.container) * te$bdf$is.container
  
  # TO DO: repair get.levels.parents, only returns 0.
  te$bdf$parent_container = get.levels.parents(bdf$level, bdf$is.container) 
  
  
  te
}

default.ps.opts = function(
  is.shiny = TRUE,
  catch.errors = TRUE,
  # parameters related to ups
  ups.save=default.ups.save(),
  # parameters related to chunk points
  e.points = 1,
  min.chunk.points=0,
  chunk.points=0,      
  show.points = TRUE,
  # relevant for shiny_chunk
  show.line.numbers = TRUE,
  check.whitelist = FALSE,
  use.secure.eval = FALSE,
  noeval = FALSE,
  preknit = FALSE,
  precomp = FALSE,
  replace.sol = FALSE,
  show.solution.btn=FALSE,
  show.data.exp=FALSE,
  show.save.btn=FALSE,
  in.R.console=FALSE,
  chunk.out.args = default.chunk.out.args(),
  # Turn off graphics when checking chunk
  use.null.device = TRUE,
  
  ...
) {
  args = c(as.list(environment()),list(...))
  args
}

set.ps.opts = function(...,opts=ps.opts(), ps=get.ps()) {
  args = list(...)
  opts[names(args)] = args
  ps[["rtutor.opts"]] = opts
}

# Default problem set options
ps.opts = function(..., ps=get.ps()) {
  opts = ps[["rtutor.opts"]]
  if (is.null(opts)) {
    opts = default.ps.opts()
  }
  args = list(...)
  opts[names(args)] = args
  opts
}

adapt.for.back.to.blocks = function(bdf,te, line.start=te$txt.start) {
  restore.point("adapt.for.back.to.blocks")
  
  is_backto = str.starts.with(bdf$type,"back_to_")
  bttype = str.right.of(bdf$type[is_backto],"back_to_")
  rows = which(is_backto)
  if (length(rows)==0) return(bdf)
  i = 1
  for (i in seq_along(rows)) {
    row = rows[i]
    parent = bdf$parent[row]
    do.stop = parent == 0
    if (!do.stop) do.stop = (bdf$type[parent] != bttype[i])
    if (do.stop) {
      msg = paste0("In line ", bdf$start[row]+line.start-1, " you have a ", bdf$type[row], " statement without a ", bttype[row]," parent.")
      stop(msg)
    }
    bdf$parent[bdf$parent==row] = parent 
  }
  # remove from txt
  te$txt[bdf$start[rows]] = ""
  
  # adapt parent indexes for removed rows
  old.ind = 1:NROW(bdf)
  new.ind = c(0,old.ind - cumsum(is_backto))
  bdf$parent = new.ind[bdf$parent+1]
  
  bdf = bdf[-rows,]
  
  bdf
}



shorten.bdf.index = function(bdf, new = 1:NROW(bdf), old = bdf$index) {
  restore.point("shorten.bdf.index")
  
  long = rep(NA_integer_,max(old))
  long[old] = new
  bdf$index = new
  cols = c("parent",colnames(bdf)[str.starts.with(colnames(bdf),"parent_")])
  pmat = as.matrix(bdf[,cols])
  pvec = as.numeric(pmat)
  pvec[pvec==0] = NA_integer_
  nmat = matrix(long[as.numeric(pvec)],NROW(pmat),ncol(pmat))
  nmat[pmat==0] = 0
  
  bdf[,cols] = nmat
  bdf
}

rtutor.dot.levels = function() {
  dot.levels = c(
    exercise = 0,
    frame = 1,
    row = 2,
    references = 2,
    column = 3,
    success = 3,
    when = 3
  )
  backto = dot.levels+1
  names(backto) = paste0("back_to_",names(dot.levels))
  c(dot.levels, backto)  
}

rtutor.parse.block = function(bi,te) {
  restore.point("rtutor.parse.block")
  
  # Don't parse blocks inside chunks here
  if (te$bdf$parent_chunk[[bi]] >0) return()
  if (te$bdf$parent_addon[[bi]] >0) return()

  
  type = te$bdf$type[[bi]]
  if (type %in% te$addons) {
    bdf = te$bdf; br = bdf[bi,];
    str = te$txt[br$start:br$end]
    Ao = te$Addons[[type]]
    
    ao = Ao$parse.fun(str, id = paste0(type,"__",bi))
    ui = Ao$shiny.ui.fun(ao)
    set.bdf.ui(ui,bi,te)
    te$bdf$obj[[bi]] = list(ao=ao)
    te$bdf$is.task[[bi]] = Ao$is.task
    return()
  }
  
  
  fun.call = parse(text=paste0("rtutor.parse.",type))
  
  fun = try(eval(fun.call), silent=TRUE)
  if (is(fun,"try-error")) {
    cat(paste0("\nWe don't have a function ",paste0("rtutor.parse.",type)," to deal with block type ", type))
    bdf = te$bdf; br = bdf[bi,];
    str = te$txt[br$start:br$end]
    set.bdf.ui(HTML(str),bi,te)
    return()
  }
  fun(bi,te)
}

rtutor.parse.chunk = function(bi,te) {
  restore.point("rtutor.parse.chunk")
  bdf = te$bdf; br = bdf[bi,]; str = te$txt[br$start:br$end]
  args = parse.chunk.args(arg.str = br$arg.str)
  
  chunk.precompute = br$parent_precompute >0 | isTRUE(args$precompute)
  chunk.preknit = isTRUE(args$preknit) | br$parent_info | br$parent_preknit
  code = str[-c(1,length(str))]
  mstr = merge.lines(str) 
  if (chunk.precompute) {
    te$bdf$stype[[bi]] = "precompute_chunk"
    expr = parse(text=code)
    res = eval(expr,te$pre.env)
    te$bdf$obj[[bi]]$pre.env = copy.env(te$pre.env)
    te$bdf$prefixed[[bi]] = TRUE
    te$bdf[bi,c("shown.rmd","sol.rmd","out.rmd")] = mstr
  } else if (chunk.preknit) {
    te$bdf$stype[[bi]] = "preknit_chunk"    
    ui = knit.rmd(str,envir = te$pre.env,out.type="shiny")
    set.bdf.ui(ui, bi,te)
    te$bdf[bi,c("shown.rmd","sol.rmd","out.rmd")] = mstr
  } else {
    te$bdf$stype[[bi]] = "task_chunk"
    chunk.ind = sum(te$bdf$stype[1:bi]=="task_chunk")
    te$bdf$id[[bi]] = paste0("tchunk_",chunk.ind)
    # a task chunk is the classic RTutor chunk
    rtutor.parse.task.chunk(bi=bi,te=te,args=args, chunk.ind=chunk.ind)
    
  }
}

rtutor.parse.preknit = function(bi,te) {
  restore.point("rtutor.parse.preknit")
  bdf = te$bdf; br = bdf[bi,];
  
  # new code: children are knitted
  children = bdf$parent == bi 
  res = get.children.and.fragments.ui.list(bi,te, keep.null=FALSE, children=children)
  ui.li = res$ui.li
  set.bdf.ui(ui.li,bi,te)
  te$bdf[bi,c("shown.rmd","sol.rmd","out.rmd")] = c(merge.lines(res$shown.rmd),merge.lines(res$sol.rmd),merge.lines(res$out.rmd))
  return()
  
  # old code knit everything
  bdf = te$bdf; br = bdf[bi,];
  str = te$txt[(br$start+1):(br$end-1)]
  
  ui = knit.rmd(str,envir = te$pre.env,out.type="shiny")
  set.bdf.ui(ui,bi,te)
}

rtutor.parse.precompute = function(bi,te) {
  # all work is done in the chunks inside
}


rtutor.parse.portrait = function(bi,te) {
  restore.point("rtutor.parse.image")
  bdf = te$bdf; br = bdf[bi,];
  
  args = get.yaml.block.args(bi,te)
  if (is.null(args$height)) args$height = "auto"

  if (is.null(args$width)) args$width = "70px"
  if (is.null(args$height)) args$height = "auto"
  if (is.null(args$align)) args$align = "left"
  
  if (!is.null(args$file)) {
    file.path = paste0(te$figure.dir,"/",args$file)
    has.file = file.exists(file.path)
  } else {
    has.file = FALSE
  }
  wh = NULL
  if (!is.null(args$width)) wh = paste0(wh," width ='",args$width,"'")
  if (!is.null(args$height)) wh = paste0(wh," height ='",args$height,"'")

   if (has.file) {
    html = paste0('<img src=figure/',args$file,wh,'>')
  } else if (!is.null(args$url)) {
    html = paste0('<img src=',args$url,wh,'>')
  } else {
    html('<p>...figure here...</p>')
  }
  if (!is.null(args$link)) {
    html = paste0("<a href='",args$link,"' target='_blank'>",html,"</a>")
  }
  
  if (is.null(args$name)) {
    name.html = ""
  } else {
    args$name = gsub("\n","<br>",args$name, fixed=TRUE)
    name.html = paste0("<tr><td style='padding-bottom: 2px;padding-left: 5px;padding-right: 5px;text-align: center;white-space: normal;word-wrap: break-all;'><font size=1>",args$name,"</font></td></tr>")
  }
  tab = paste0("<table  align='",args$align,"'><tr><td STYLE='padding-left:5px;padding-right:5px;'>",html,'</td></tr>',name.html,"</table>")
  set.bdf.ui(HTML(tab),bi,te)
}

rtutor.parse.image = function(bi,te, download.image=TRUE) {
  restore.point("rtutor.parse.image")
  bdf = te$bdf; br = bdf[bi,];
  
  args = get.yaml.block.args(bi,te)
  if (is.null(args$height)) args$height = "auto"

  if (!is.null(args$file)) {
    file.path = paste0(te$figure.dir,"/",args$file)
    has.file = file.exists(file.path)
  } else {
    has.file = FALSE
  }
  wh = NULL
  if (!is.null(args$width)) wh = paste0(wh," width ='",args$width,"'")
  if (!is.null(args$height)) wh = paste0(wh," height ='",args$height,"'")

  if (has.file) {
    html = paste0('<img src=figure/',args$file,wh,' >')
  } else if (!is.null(args$url)) {
    html = paste0('<img src=',args$url,wh,'>')
  } else {
    html('<p>...figure here...</p>')
  }
  set.bdf.ui(HTML(html),bi,te)
}

 
rtutor.parse.solved = function(bi,te) {
  restore.point("rtutor.parse.solved")
  rtutor.parse.as.container(bi,te)
}


rtutor.parse.column = function(bi,te) {
  restore.point("rtutor.parse.column")
  bdf = te$bdf; br = bdf[bi,];
  args = parse.block.args(arg.str = br$arg.str)
  if (is.null(args$width)) args$width = 6 
  if (is.null(args$offset)) args$offset = 0 
  ui.fun = function(ui) {
    column(width = args$width, offset=args$offset,ui)
  }
  rtutor.parse.as.container(bi,te, is.static=TRUE, ui.fun=ui.fun)
}

rtutor.parse.row = function(bi,te) {
  restore.point("rtutor.parse.row")
  rtutor.parse.as.container(bi,te, is.static=TRUE, ui.fun=fluidRow)
}


rtutor.parse.frame = function(bi,te) {
  restore.point("rtutor.parse.frame")
  bdf = te$bdf; br = bdf[bi,];
  args = parse.block.args(arg.str = te$bdf$arg.str[[bi]])
  rtutor.parse.as.container(bi,te,args = args, rmd.prefix="## Frame")
  if (is.null(args$title.offset)) args$title.offset=0
  if (!is.null(args$name)) {
    title = fluidRow(column(offset=args$title.offset,width=12-args$title.offset,h4(args$name)))
  } else {
    title = NULL
  }
  te$bdf$obj[[bi]] = list(title = args$name, args=args)
}


rtutor.parse.as.container = function(bi, te,args=NULL, inner.ui = NULL, rmd.li=NULL, highlight.code = !is.static, always.reload=FALSE, is.static=FALSE, rmd.head=NULL, rmd.prefix="", rmd.postfix="", ui.fun=NULL) {
  restore.point("rtutor.parse.as.container")
  bdf = te$bdf; br = bdf[bi,];
  if (is.null(inner.ui) | is.null(rmd.li)) {
    res = get.children.and.fragments.ui.list(bi,te,keep.null=TRUE)
    if (is.null(inner.ui))
      inner.ui = res$ui.li
    if (is.null(rmd.li)) rmd.li = res
  }
  if (!is.null(ui.fun)) {
    inner.ui = ui.fun(inner.ui)
  }
  if (!is.null(inner.ui)) {
    inner.ui = tagList(
      inner.ui,
      if (highlight.code) highlight.code.script() else NULL
    )
  }
  
  set.bdf.rmd(bi, te, rmd.li=rmd.li, rmd.prefix=rmd.prefix, rmd.postfix=rmd.postfix)
  
  # A dynamic container will be loaded in an uiOutput
  if (!is.static) {
    te$bdf$inner.ui[[bi]] = inner.ui
    set.container.div.and.output(bi,te)
  
  # A static container will not be loaded in a uiOutput
  } else {
    te$bdf$div.id[[bi]] = div.id = paste0(te$prefix, br$id,"_div")
    div.class = "rtutor-static-container-div"
    te$bdf$ui[[bi]] = div(id=div.id,class=div.class, 
      inner.ui
    )
  }
  te$bdf$is.container[[bi]] = TRUE
}

set.container.div.and.output = function(bi, te, always.reload=FALSE) {
  bdf = te$bdf; br = bdf[bi,];
  
  te$bdf$div.id[[bi]] = div.id = paste0(te$prefix, br$id,"_div")
  te$bdf$output.id[[bi]] = output.id = paste0(te$prefix, br$id,"_output")
  te$bdf$always.reload[[bi]] = always.reload
  div.class = "rtutor-container-div"
  te$bdf$ui[[bi]] = div(id=div.id,class=div.class, 
    uiOutput(output.id)
  )
  
}

get.container.default.rmd = function(bi,te) {
  title = paste0("## Frame ", args$name)

}

parse.container.inner.ui.and.rmd = function(bi, te) {
  restore.point("rtutor.parse.frame")
  #stop()
  bdf = te$bdf; br = bdf[bi,];
  args = parse.block.args(arg.str = br$arg.str)
  
  children = bdf$parent == bi 
  res = get.children.and.fragments.ui.list(bi,te, children=children, keep.null=TRUE)
  
  ui.li = res$ui.li
  is.child = !res$is.frag

  te$bdf[bi,c("shown.rmd","sol.rmd","out.rmd")] = c(merge.lines(c(title,res$shown.rmd)),merge.lines(c(title,res$sol.rmd)),merge.lines(c(title,res$out.rmd)))
  
  if (is.null(args$title.offset)) args$title.offset=0
  if (!is.null(args$name)) {
    title = fluidRow(column(offset=args$title.offset,width=12-args$title.offset,h4(args$name)))
  } else {
    title = NULL
  }
  ui = tagList(
    ui.li,
    highlight.code.script()
  )
  te$bdf$obj[[bi]] = list(title = args$name, args=args)
  set.bdf.ui(ui,bi,te)
  
}


rtutor.parse.info = function(bi,te) {
  restore.point("rtutor.parse.info")
  parse.as.collapse(bi,te,title.prefix="Info")
}

rtutor.parse.note = function(bi,te) {
  restore.point("rtutor.parse.note")
  parse.as.collapse(bi,te)
}

rtutor.parse.award = function(bi,te) {
  restore.point("rtutor.parse.award")

  res = get.children.and.fragments.ui.list(bi,te, keep.null=FALSE)
  out.rmd = merge.lines(c("---\n### Award",res$out.rmd,"---"))
  rmd.li = c("","",out.rmd)
  parse.as.collapse(bi,te, rmd.li=rmd.li, title.prefix="Award ", is.static=TRUE)
}

rtutor.parse.references = function(bi,te) {
  restore.point("references.block.render")
  title = "References"
  if (isTRUE(te$lang=="de")) {
    title = "Referenzen"
  }
  parse.as.collapse(bi,te, title=title, is.static=TRUE)
}

rtutor.parse.as.collapse  =  function(bi,te,title.prefix=NULL, title=NULL, rmd.head=paste0("### ", title), rmd.foot="---",is.static=TRUE,always.reload=FALSE,...) {
  restore.point("rtutor.parse.as.collapse")
  #stop()
  bdf = te$bdf; br = bdf[bi,];
  args = parse.block.args(arg.str = br$arg.str)
  children = bdf$parent == bi 
  res = get.children.and.fragments.ui.list(bi,te, children=children, keep.null=TRUE)
  
  ui.li = res$ui.li
  is.child = !res$is.frag

  if (is.null(title)) title = paste0(title.prefix, " ",args$name)
  if (is.null(title)) title = bs$type[[bi]]
  inner.ui = make.rtutor.collapse.note(id=paste0(te$bdf$type[[bi]],"_collapse_",bi),content=ui.li, title=title)  
  rmd.li = list(
    shown.rmd = merge.lines(c(rmd.head,res$shown.rmd,rmd.foot),
    sol.rm  = merge.lines(c(rmd.head,res$sol.rmd,rmd.foot)),
    out.rmd = merge.lines(c(rmd.head,res$out.rmd,rmd.foot)))
  )
  rtutor.parse.as.container(bi,te,args=args, inner.ui=inner.ui, rmd.li=rmd.li, is.static=is.static, always.reload=always.reload,...)
}


set.bdf.rmd = function(bi, te, shown.rmd=rmd.li$shown.rmd, sol.rmd=rmd.li$sol.rmd, out.rmd = rmd.li$out.rmd, rmd.li=NULL, rmd.prefix="", rmd.postfix) {
  te$bdf[bi,c("shown.rmd","sol.rmd","out.rmd")] = c(
    merge.lines(c(rmd.prefix,shown.rmd,rmd.postfix)),
    merge.lines(c(rmd.prefix,sol.rmd,rmd.postfix)),
    merge.lines(c(rmd.prefix,out.rmd,rmd.postfix))
  )
}

get.bi.te.str = function(bi,te, remove.header.footer=TRUE) {
  restore.point("get.bi.te.str")
  
  start = (te$bdf$start[bi]+1)
  end = te$bdf$end[bi]-1*(te$bdf$form[[bi]]=="block")
  if (end<start) return(NULL)
  str = te$txt[start:end]
  str
}

make.rtutor.collapse.note = function(id, html, title="Note", content=NULL) {
  if (is.null(content))
    content = HTML(paste0(html, collapse="\n"))
  
  shinyBS::bsCollapse(id =id, shinyBS::bsCollapsePanel(title=title,content))
  
}

set.in.bdf = function(bi,te,...) {
  args = list(...)
  restore.point("set.in.bdf")

  cols = match(names(args),colnames(te$bdf))
  if (is(te$bdf,"data.table")) {
    for (j in seq_along(cols)) {
      set(te$bdf,bi,cols[j],args[j])
    }
  } else {
    for (j in seq_along(cols)) {
      te$bdf[bi,cols[j]]=args[[j]]
    }
  }
}


set.bdf.ui = function(ui,bi,te) {
  te$bdf$ui[[bi]] = ui
  #te$bdf$has.ui[[bi]] = TRUE
}

get.children.and.fragments.ui.list = function(bi,te,bdf=te$bdf, keep.null=TRUE, empty.as.null=FALSE, children=te$bdf$parent == bi ) {
  restore.point("get.children.and.fragments.ui.list")
  
  res = get.non.children.fragments(bi,te, child.ind = which(children))
  is.frag = res$is.frag
  is.child = !is.frag
  ui = sol.rmd = shown.rmd = out.rmd = res$frag

  ui[is.frag] = lapply(ui[is.frag], function(txt) {
    HTML(md2html(txt, fragment.only = TRUE))
  })
  
  ui[is.child] = lapply(which(children), function(ind) {
    bdf$ui[[ind]]
  })
  sol.rmd[is.child] = bdf$sol.rmd[children]
  shown.rmd[is.child] = bdf$shown.rmd[children]
  out.rmd[is.child] = bdf$out.rmd[children]

    
  if (!keep.null) {
    null.ui = sapply(ui, is.null)
    ui = ui[!is.null(ui)]
    is.frag = is.frag[null.ui]
  }
  names(ui) = NULL
  list(ui.li=ui, sol.rmd=sol.rmd,shown.rmd=shown.rmd,out.rmd=out.rmd, is.frag=is.frag)
}

get.child.and.fragment.txt.li = function(bi,te,bdf=te$bdf, child.ind = which(bdf$parent == bi), keep.header.footer=FALSE) {
  restore.point("get.child.and.fragment.txt.li")
  
  cpos = cbind(bdf$start[child.ind],bdf$end[child.ind])
  has.footer = bdf$form[[bi]] != "dotblock"
  start = bdf$start[bi]+ (1-keep.header.footer)
  end = bdf$end[bi] - (1- (keep.header.footer | !has.footer))
  
  pos = pos.complement(cpos, is.sorted=TRUE, keep.pos=TRUE, start=start, end = end)
  is.frag = attr(pos,"complement")
  
  # we may have one end line too much
  valid = pos[,1]<=pos[,2]
  pos = pos[valid,,drop=FALSE]
  is.frag = is.frag[valid]
  
  txt.li = lapply(1:NROW(pos), function(row) {
    te$txt[pos[row,1]:pos[row,2]]
  })
  list(txt.li = txt.li, is.frag=is.frag)
}

get.non.children.fragments = function(bi,te,bdf=te$bdf, child.ind = which(bdf$parent == bi), keep.header.footer=FALSE) {
  restore.point("get.non.children.fragments")
  
  cpos = cbind(bdf$start[child.ind],bdf$end[child.ind])
  has.footer = bdf$form[[bi]] != "dotblock"
  start = bdf$start[bi]+ (1-keep.header.footer)
  end = bdf$end[bi] - (1- (keep.header.footer | !has.footer))
  
  pos = pos.complement(cpos, is.sorted=TRUE, keep.pos=TRUE, start=start, end = end)
  is.frag = attr(pos,"complement")
  
  # we may have one end line too much
  valid = pos[,1]<=pos[,2]
  pos = pos[valid,,drop=FALSE]
  is.frag = is.frag[valid]
  
  if (NROW(pos)==0) return(NULL)
  
  frag.li = lapply(1:NROW(pos), function(row) {
    if (!is.frag[row]) return(NULL)
    te$txt[pos[row,1]:pos[row,2]]
  })
  list(frags = frag.li, is.frag=is.frag)
}


copy.env = function(env) {
  new = as.environment(as.list(env))
  parent.env(new) = parent.env(env)
  new
}

#' Parse the name of a knitr chunk and its arguments
#' @export
parse.chunk.args = function(header, arg.str=NULL) {
  restore.point("parse.chunk.opt.and.name")
  if (!is.null(arg.str)) {
    if (is.na(arg.str)) return(list())
    return(knitr:::parse_params(arg.str))
  }

  str = str.right.of(header,"r ",not.found = NA)
  if (is.na(str)) return(list())
  knitr:::parse_params(str.left.of(str,"}"))
}


make.te.ui = function(te, bdf=te$bdf) {
  rows = which(bdf$parent == 0)
  
  ui.li = bdf$ui[rows]
  
}

fast.name.rmd.chunks = function(txt,prefix = "chunk_", chunk.lines = NULL, method="number") {
  restore.point("name.rmd.chunks.by.number")
  
  if (is.null(chunk.lines)) {
    chunk.lines = which(str.starts.with(txt,"```{r"))
  }
  if (length(chunk.lines)==0) return(txt)
  str = str.right.of(txt[chunk.lines],'```{r')

  comma.pos = str.locate.first(str,",")[,1]
  eq.pos = str.locate.first(str,"=")[,1]
  brace.pos = str.locate.first(str,"}")[,1]
  
  has.eq = !is.na(eq.pos) & !is.true(comma.pos < eq.pos)
  has.comma = !is.na(comma.pos) & !has.eq
  
  if (method=="number") {
    name = paste0('"chunk_',seq_along(str),'"')
  } else {
    name = random.string(length(str),nchar = 14)
  }
  right = str
  right[has.eq] = paste0(", ",str[has.eq])
  right[has.comma] = substring(str[has.comma],comma.pos[has.comma])
  right[!has.comma & !has.eq] = "}"
  
  new =  paste0("```{r ",name,right)
  txt[chunk.lines] = new
  txt
}

get.yaml.block.args = function(bi,te) {
  restore.point("get.yaml.block.args")
  
  args = parse.block.args(arg.str = te$bdf$arg.str[[bi]])
  yaml = get.bi.te.str(bi,te)
  if (!is.null(yaml)) {
    yaml.arg = yaml.load(paste0(yaml,collapse="\n"))
    args[names(yaml.arg)] = yaml.arg
  }
  args
}
