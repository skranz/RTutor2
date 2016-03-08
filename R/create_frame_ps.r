examples.frame.ps = function() {
  library(EconCurves)
  setwd("D:/libraries/RTutor2")
  txt = readLines("ex1.Rmd")
  
  setwd("D:/libraries/RTutor2/examples/auction")
  txt = readLines("auction_new_sol.Rmd")
  popts=default.ps.opts(
    show.solution.btn = TRUE,
    slides = FALSE,
    static.type = NULL,
    lang = "de"
  )
  ps = rtutor.make.frame.ps(txt,catch.errors = FALSE, priority.opts=popts)

  bdf = ps$bdf
  restore.point.options(display.restore.point = TRUE)
  
  app = rtutorApp(ps)
  viewApp(app)

}



rtutor.make.frame.ps = function(txt,addons="quiz",bdf.filter = NULL,dir=getwd(), figure.dir=paste0(dir,"/figure"),catch.errors=TRUE,ps.id = "",opts=default.ps.opts(), priority.opts=list(),  ...) {
  restore.point("rtutor.make.frame.ps")

  ps = new.env()
  ps$addons = addons
  ps$Addons = make.addons.list(addons)
  ps$dir = dir
  ps$figure.dir = figure.dir
  
  if (length(txt)==1)  
    txt = sep.lines(txt)

  #Encoding(txt) = "UTF8"
  txt = mark_utf8(txt)

  
  
  # Only capture elements between the lines <!-- START --> and <!-- END -->
  res = rmd.between.start.end.lines(txt,return.start.end = TRUE)
  ps$txt.start = res$start; ps$txt.end = res$end
  txt = res$txt

  txt = fast.name.rmd.chunks(txt)
  # add outer container
  txt = c("#. ps",txt)
  
  dot.levels = rtutor.dot.levels()
  df = find.rmd.nested(txt, dot.levels)
  
  # all rows that will be deleted 
  # in this precompilation state
  del.lines = NULL
 
  # settings in rmd file overwrite opts
  bis = which(df$type == "settings")
  for (bi in bis) {
    yaml = paste0(txt[(df$start[bi]+1):(df$end[bi]-1)], collapse="\n")
    so = read.yaml(text=yaml, keep.quotes=FALSE)
    opts[names(so)] = so
    del.lines = c(del.lines, df$start[bi]:df$end[bi])
  }

  # priority opts overwrite settings and opts
  opts[names(priority.opts)] = priority.opts
  
  set.rt.opts(opts)
  ps$opts = opts
  ps$static.types = opts$static.types
  ps$slides = opts$slides
  ps$slide.type = opts$slide.type
  
  if (ps$slides & ps$slide.type %in% ps$static.types) {
    ps$hidden.container.types = ps$slide.type 
  }
    
  # remove content in ignore blocks
  ig.rows = which(df$type=="ignore")
  if (length(ig.rows)>0) {
    del.lines = c(del.lines,unlist(lapply(ig.rows, function(ig.row) df$start[ig.row]:df$end[ig.rows])))
  }  

  if (length(del.lines)>0) {
    del.lines =unique(del.lines)
    txt = txt[-del.lines]
    df = find.rmd.nested(txt, dot.levels)
  }
  
  ps$txt = txt
  
  df = adapt.for.back.to.blocks(df,ps=ps)
  df$stype = df$type
  df$is.addon = df$type %in% addons
  
  
  df$parent_addon = get.levels.parents(df$level, df$is.addon)
  parent.types = c("frame","row", "column","chunk","preknit","precompute","knit","compute","info","section","subsection")
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
    is.static = TRUE,
    div.id = "",output.id=  "",   
    
 
    shown.rmd = "", out.rmd = "",sol.rmd = ""

  )
  
  
  # Filter bdf if only a subset of elements shall be compiled / shown
  if (!is.null(bdf.filter)) {
    bdf = bdf.filter(bdf=bdf)
    bdf = shorten.bdf.index(bdf)
    #bdf$index = 1:NROW(bdf)
  }

  ps$bdf = bdf
  
  # Create env
  ps.baseenv = new.env(parent=parent.env(globalenv()))
  ps$pre.env = ps.baseenv
  ps$env = new.env(parent=parent.env(globalenv()))
  

  # Additional information for slides
  if (ps$slides) {
    ps$num.slides = sum(ps$bdf$type==ps$slide.type)
  }

  
  # Go through blocks and chunks, ordered by end
  binds = order(bdf$end, -bdf$start)
  bi = binds[1]
  for (bi in binds) {
    restore.point("inner.make.ps")
    if (catch.errors) {
      res = try(rtutor.parse.block(bi,ps), silent=TRUE)
      if (is(res,"try-error")) {
        br = ps$bdf[bi,]
        msg = paste0("Error when parsing ",br$type," block lines ", br$start+ps$txt.start-1, " to ", br$end+ps$txt.start-1,"\n\n", paste0(as.character(res), collape="\n"))
        stop(msg)
      }
    } else {
      res = rtutor.parse.block(bi,ps)  
    }
  }
  ps$bdf$stype.ind = compute.value.index(ps$bdf$stype)

  
  ps$bdf$task.ind = cumsum(ps$bdf$is.task) * ps$bdf$is.task
  # store task.chunks in a list ck.li and the
  # corresponding user chunks in uk
  chunk.rows = which(ps$bdf$stype == "task_chunk")
  ps$ck.li = lapply(chunk.rows, function(bi) {
    ps$bdf$obj[[bi]]$ck
  })
  ps$org.uk.li = lapply(ps$ck.li, function(ck) {
    make.user.chunk(ck)
  })
  
  # specify containers
  ps$bdf$container.ind = cumsum(ps$bdf$is.container) * ps$bdf$is.container
  
  ps$bdf$parent_container = get.levels.parents(ps$bdf$level, ps$bdf$is.container) 
  
  
  ps$navbar.ui = rtutor.navbar(ps=ps, nav.levels = opts$nav.levels)
  
  ps
}

adapt.for.back.to.blocks = function(bdf,ps, line.start=ps$txt.start) {
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
  ps$txt[bdf$start[rows]] = ""
  
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
    ps = -1000,
    section = -3,
    subsection = -2,
    subsubsection = -1,
    frame = 1,
    row = 2,
    references = 2,
    column = 3,
    success = 3,
    when = 3
  )
  backto = dot.levels+1
  names(backto) = paste0("back_to_",names(dot.levels))
  lev = c(dot.levels, backto)  
  lev = rank(lev,ties.method = "min")
  lev
}

rtutor.parse.block = function(bi,ps) {
  restore.point("rtutor.parse.block")
  
  # Don't parse blocks inside chunks here
  if (ps$bdf$parent_chunk[[bi]] >0) return()
  if (ps$bdf$parent_addon[[bi]] >0) return()

  
  type = ps$bdf$type[[bi]]
  if (type %in% ps$addons) {
    bdf = ps$bdf; br = bdf[bi,];
    str = ps$txt[br$start:br$end]
    Ao = ps$Addons[[type]]
    
    ao = Ao$parse.fun(str, id = paste0(type,"__",bi))
    ui = Ao$shiny.ui.fun(ao)
    set.bdf.ui(ui,bi,ps)
    ps$bdf$obj[[bi]] = list(ao=ao)
    ps$bdf$is.task[[bi]] = Ao$is.task
    return()
  }
  fun.name = paste0("rtutor.parse.",type)
  if (!exists(fun.name)) {
    cat(paste0("\nWe don't have a function ",fun.name," to deal with block type ", type))
    bdf = ps$bdf; br = bdf[bi,];
    str = ps$txt[br$start:br$end]
    set.bdf.ui(HTML(str),bi,ps)
    return()
  }
  
  fun.call = parse(text=paste0("rtutor.parse.",type,"(bi,ps)"))
  res = eval(fun.call)
  res
  #fun(bi,ps)
}

rtutor.parse.chunk = function(bi,ps) {
  restore.point("rtutor.parse.chunk")
  bdf = ps$bdf; br = bdf[bi,]; str = ps$txt[br$start:br$end]
  args = parse.chunk.args(arg.str = br$arg.str)
  
  chunk.precompute = br$parent_precompute >0 | isTRUE(args$precompute)
  chunk.preknit = isTRUE(args$preknit) | br$parent_info | br$parent_preknit
  code = str[-c(1,length(str))]
  mstr = merge.lines(str) 
  if (chunk.precompute) {
    restore.point("parse.precompute.chunk")
    ps$bdf$stype[[bi]] = "precompute_chunk"
    expr = parse(text=code)
    res = eval(expr,ps$pre.env)
    ps$bdf$obj[[bi]]$pre.env = copy.env(ps$pre.env)
    ps$bdf$prefixed[[bi]] = TRUE
    ps$bdf[bi,c("shown.rmd","sol.rmd","out.rmd")] = mstr
  } else if (chunk.preknit) {
    restore.point("parse.preknit.chunk")
    ps$bdf$stype[[bi]] = "preknit_chunk"    
    ui = knit.rmd(str,envir = ps$pre.env,out.type="shiny")
    set.bdf.ui(ui, bi,ps)
    ps$bdf[bi,c("shown.rmd","sol.rmd","out.rmd")] = mstr
  } else {
    ps$bdf$stype[[bi]] = "task_chunk"
    # a task chunk is the classic RTutor chunk
    rtutor.parse.task.chunk(bi=bi,ps=ps,args=args)
    
  }
}


rtutor.parse.preknit = function(bi,ps) {
  restore.point("rtutor.parse.preknit")
  bdf = ps$bdf; br = bdf[bi,];
  
  # new code: children are knitted
  children = bdf$parent == bi 
  res = get.children.and.fragments.ui.list(bi,ps, keep.null=FALSE, children=children)
  ui.li = res$ui.li
  set.bdf.ui(ui.li,bi,ps)
  ps$bdf[bi,c("shown.rmd","sol.rmd","out.rmd")] = c(merge.lines(res$shown.rmd),merge.lines(res$sol.rmd),merge.lines(res$out.rmd))
  return()
  
  # old code knit everything
  bdf = ps$bdf; br = bdf[bi,];
  str = ps$txt[(br$start+1):(br$end-1)]
  
  ui = knit.rmd(str,envir = ps$pre.env,out.type="shiny")
  set.bdf.ui(ui,bi,ps)
}

rtutor.parse.precompute = function(bi,ps) {
  # all work is done in the chunks inside
}


rtutor.parse.portrait = function(bi,ps) {
  restore.point("rtutor.parse.image")
  bdf = ps$bdf; br = bdf[bi,];
  
  args = get.yaml.block.args(bi,ps)
  if (is.null(args$height)) args$height = "auto"

  if (is.null(args$width)) args$width = "70px"
  if (is.null(args$height)) args$height = "auto"
  if (is.null(args$align)) args$align = "left"
  
  if (!is.null(args$file)) {
    file.path = paste0(ps$figure.dir,"/",args$file)
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
  set.bdf.ui(HTML(tab),bi,ps)
}

rtutor.parse.image = function(bi,ps, download.image=TRUE) {
  restore.point("rtutor.parse.image")
  bdf = ps$bdf; br = bdf[bi,];
  
  args = get.yaml.block.args(bi,ps)
  if (is.null(args$height)) args$height = "auto"

  if (!is.null(args$file)) {
    file.path = paste0(ps$figure.dir,"/",args$file)
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
  set.bdf.ui(HTML(html),bi,ps)
}

 
rtutor.parse.solved = function(bi,ps) {
  restore.point("rtutor.parse.solved")
  rtutor.parse.as.container(bi,ps)
}


rtutor.parse.column = function(bi,ps) {
  restore.point("rtutor.parse.column")
  bdf = ps$bdf; br = bdf[bi,];
  args = parse.block.args(arg.str = br$arg.str)
  if (is.null(args$width)) args$width = 6 
  if (is.null(args$offset)) args$offset = 0 
  ui.fun = function(ui) {
    column(width = args$width, offset=args$offset,ui)
  }
  rtutor.parse.as.container(bi,ps, is.static=TRUE, ui.fun=ui.fun)
}

rtutor.parse.row = function(bi,ps) {
  restore.point("rtutor.parse.row")
  rtutor.parse.as.container(bi,ps, is.static=TRUE, ui.fun=fluidRow)
}

rtutor.parse.ps = function(bi,ps) {
  restore.point("rtutor.parse.ps")
  rtutor.parse.as.container(bi,ps, is.static=TRUE)
}


rtutor.parse.frame = function(bi,ps) {
  restore.point("rtutor.parse.frame")
  bdf = ps$bdf; br = bdf[bi,];
  args = parse.block.args(arg.str = ps$bdf$arg.str[[bi]])
  title = first.non.null(args$title, args$name)

  rtutor.parse.as.container(bi,ps,args = args, rmd.prefix="## Frame", title = title)
  if (is.null(args$title.offset)) args$title.offset=0
  ps$bdf$obj[[bi]] = list(title = args$name, args=args)
}

rtutor.parse.section = function(bi,ps) {
  restore.point("rtutor.parse.section")
  rtutor.parse.as.section(bi,ps,type="section", rmd.prefix="# Section")
}

rtutor.parse.subsection = function(bi,ps) {
  restore.point("rtutor.parse.subsection")
  rtutor.parse.as.section(bi,ps,type="subsection", rmd.prefix="## Subsection")
}


rtutor.parse.subsubsection = function(bi,ps) {
  restore.point("rtutor.parse.subsection")
  rtutor.parse.as.section(bi,ps,type="subsubsection", rmd.prefix="###  Subsubsection")
}

rtutor.parse.frame = function(bi,ps) {
  restore.point("rtutor.parse.frame")
  rtutor.parse.as.section(bi,ps,type="frame", rmd.prefix="### Frame") }

rtutor.parse.as.section = function(bi, ps, type="section", rmd.prefix="# Section") {
  restore.point("rtutor.parse.as.section")
  bdf = ps$bdf; br = bdf[bi,];
  args = parse.block.args(arg.str = ps$bdf$arg.str[[bi]])
  title = first.non.null(args$title, args$name)

  rtutor.parse.as.container(bi,ps,args = args, rmd.prefix=rmd.prefix, title = title)
  if (is.null(args$title.offset)) args$title.offset=0
  ps$bdf$obj[[bi]] = list(title = args$name, args=args)
}


rtutor.parse.as.container = function(bi, ps,args=NULL, inner.ui = NULL, rmd.li=NULL, highlight.code = !is.static, always.reload=FALSE, is.static=ps$bdf$type[[bi]] %in% ps$static.types, rmd.head=NULL, rmd.prefix="", rmd.postfix="", ui.fun=NULL, title = ps$bdf$obj[[bi]]$title) {
  restore.point("rtutor.parse.as.container")
  bdf = ps$bdf; br = bdf[bi,];
  if (is.null(inner.ui) | is.null(rmd.li)) {
    res = get.children.and.fragments.ui.list(bi,ps,keep.null=TRUE)
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
  
  
  type = ps$bdf$type[[bi]]
  
  # Add slide header
  if (ps$slides & identical(ps$slide.type,type)) {
    slide.ind = sum(ps$bdf$type[1:bi]==ps$slide.type)
    header = slide.title.bar.ui(title = title,slide.ind=slide.ind,num.slides = ps$num.slides) 
    
  # Add title as header
  } else {
    header = container.title.html(title = title)  
  } 
  if (!is.null(header)) {
    inner.ui = list(header, inner.ui)
  }
  
  set.bdf.rmd(bi, ps, rmd.li=rmd.li, rmd.prefix=rmd.prefix, rmd.postfix=rmd.postfix)
  ps$bdf$is.static[[bi]] = is.static
  
  
  
  # A dynamic container will be loaded in an uiOutput
  if (!is.static) {
    ps$bdf$inner.ui[[bi]] = inner.ui
    set.container.div.and.output(bi,ps)
  
  # A static container will not be loaded in a uiOutput
  } else {
    style = ""
    if (type %in% ps$hidden.container.types) style = "display: none;"
    
    ps$bdf$div.id[[bi]] = div.id = paste0(ps$prefix, br$id,"_div")
    div.class = "rtutor-static-container-div"
    ps$bdf$ui[[bi]] = div(id=div.id,class=div.class,  style=style,
      inner.ui
    )
  }
  ps$bdf$is.container[[bi]] = TRUE
}

container.title.html = function(title,type=NULL, ps=NULL) {
  if (is.null(title)) return(NULL)
  h4(title)      
}

set.container.div.and.output = function(bi, ps, always.reload=FALSE) {
  bdf = ps$bdf; br = bdf[bi,];
  
  ps$bdf$div.id[[bi]] = div.id = paste0(ps$prefix, br$id,"_div")
  ps$bdf$output.id[[bi]] = output.id = paste0(ps$prefix, br$id,"_output")
  ps$bdf$always.reload[[bi]] = always.reload
  div.class = "rtutor-container-div"
  ps$bdf$ui[[bi]] = div(id=div.id,class=div.class, 
    uiOutput(output.id)
  )
  
}

get.container.default.rmd = function(bi,ps) {
  title = paste0("## Frame ", args$name)

}

parse.container.inner.ui.and.rmd = function(bi, ps) {
  restore.point("rtutor.parse.frame")
  #stop()
  bdf = ps$bdf; br = bdf[bi,];
  args = parse.block.args(arg.str = br$arg.str)
  
  children = bdf$parent == bi 
  res = get.children.and.fragments.ui.list(bi,ps, children=children, keep.null=TRUE)
  
  ui.li = res$ui.li
  is.child = !res$is.frag

  ps$bdf[bi,c("shown.rmd","sol.rmd","out.rmd")] = c(merge.lines(c(title,res$shown.rmd)),merge.lines(c(title,res$sol.rmd)),merge.lines(c(title,res$out.rmd)))
  
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
  ps$bdf$obj[[bi]] = list(title = args$name, args=args)
  set.bdf.ui(ui,bi,ps)
  
}


rtutor.parse.info = function(bi,ps) {
  restore.point("rtutor.parse.info")
  rtutor.parse.as.collapse(bi,ps,title.prefix="Info")
}

rtutor.parse.note = function(bi,ps) {
  restore.point("rtutor.parse.note")
  rtutor.parse.as.collapse(bi,ps)
}

rtutor.parse.award = function(bi,ps) {
  restore.point("rtutor.parse.award")

  res = get.children.and.fragments.ui.list(bi,ps, keep.null=FALSE)
  out.rmd = merge.lines(c("---\n### Award",res$out.rmd,"---"))
  rmd.li = c("","",out.rmd)
  rtutor.parse.as.collapse(bi,ps, rmd.li=rmd.li, title.prefix="Award ", is.static=TRUE)
}

rtutor.parse.references = function(bi,ps) {
  restore.point("references.block.render")
  title = "References"
  if (isTRUE(ps$opts$lang=="de")) {
    title = "Referenzen"
  }
  rtutor.parse.as.collapse(bi,ps, title=title, is.static=TRUE)
}

rtutor.parse.as.collapse  =  function(bi,ps,title.prefix=NULL, title=NULL, rmd.head=paste0("### ", title), rmd.foot="---",is.static=TRUE,always.reload=FALSE,...) {
  restore.point("rtutor.parse.as.collapse")
  #stop()
  bdf = ps$bdf; br = bdf[bi,];
  args = parse.block.args(arg.str = br$arg.str)
  children = bdf$parent == bi 
  res = get.children.and.fragments.ui.list(bi,ps, children=children, keep.null=TRUE)
  
  ui.li = res$ui.li
  is.child = !res$is.frag

  if (is.null(title)) title = paste0(title.prefix, " ",args$name)
  if (is.null(title)) title = bs$type[[bi]]
  inner.ui = make.rtutor.collapse.note(id=paste0(ps$bdf$type[[bi]],"_collapse_",bi),content=ui.li, title=title)  
  rmd.li = list(
    shown.rmd = merge.lines(c(rmd.head,res$shown.rmd,rmd.foot)),
    sol.rmd  = merge.lines(c(rmd.head,res$sol.rmd,rmd.foot)),
    out.rmd = merge.lines(c(rmd.head,res$out.rmd,rmd.foot))
  )
  rtutor.parse.as.container(bi,ps,args=args, inner.ui=inner.ui, rmd.li=rmd.li, is.static=is.static, always.reload=always.reload,...)
}


set.bdf.rmd = function(bi, ps, shown.rmd=rmd.li$shown.rmd, sol.rmd=rmd.li$sol.rmd, out.rmd = rmd.li$out.rmd, rmd.li=NULL, rmd.prefix="", rmd.postfix) {
  ps$bdf[bi,c("shown.rmd","sol.rmd","out.rmd")] = c(
    merge.lines(c(rmd.prefix,shown.rmd,rmd.postfix)),
    merge.lines(c(rmd.prefix,sol.rmd,rmd.postfix)),
    merge.lines(c(rmd.prefix,out.rmd,rmd.postfix))
  )
}

get.bi.ps.str = function(bi,ps, remove.header.footer=TRUE) {
  restore.point("get.bi.ps.str")
  
  start = (ps$bdf$start[bi]+1)
  end = ps$bdf$end[bi]-1*(ps$bdf$form[[bi]]=="block")
  if (end<start) return(NULL)
  str = ps$txt[start:end]
  str
}

make.rtutor.collapse.note = function(id, html, title="Note", content=NULL) {
  if (is.null(content))
    content = HTML(paste0(html, collapse="\n"))
  
  shinyBS::bsCollapse(id =id, shinyBS::bsCollapsePanel(title=title,content))
  
}

set.in.bdf = function(bi,ps,...) {
  args = list(...)
  restore.point("set.in.bdf")

  cols = match(names(args),colnames(ps$bdf))
  if (is(ps$bdf,"data.table")) {
    for (j in seq_along(cols)) {
      set(ps$bdf,bi,cols[j],args[j])
    }
  } else {
    for (j in seq_along(cols)) {
      ps$bdf[bi,cols[j]]=args[[j]]
    }
  }
}


set.bdf.ui = function(ui,bi,ps) {
  ps$bdf$ui[[bi]] = ui
  #ps$bdf$has.ui[[bi]] = TRUE
}

get.children.and.fragments.ui.list = function(bi,ps,bdf=ps$bdf, keep.null=TRUE, empty.as.null=FALSE, children=ps$bdf$parent == bi ) {
  restore.point("get.children.and.fragments.ui.list")
  
  res = get.non.children.fragments(bi,ps, child.ind = which(children))
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

get.child.and.fragment.txt.li = function(bi,ps,bdf=ps$bdf, child.ind = which(bdf$parent == bi), keep.header.footer=FALSE) {
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
    ps$txt[pos[row,1]:pos[row,2]]
  })
  list(txt.li = txt.li, is.frag=is.frag)
}

get.non.children.fragments = function(bi,ps,bdf=ps$bdf, child.ind = which(bdf$parent == bi), keep.header.footer=FALSE) {
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
    ps$txt[pos[row,1]:pos[row,2]]
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


make.ps.ui = function(ps, bdf=ps$bdf) {
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

get.yaml.block.args = function(bi,ps) {
  restore.point("get.yaml.block.args")
  
  args = parse.block.args(arg.str = ps$bdf$arg.str[[bi]])
  yaml = get.bi.ps.str(bi,ps)
  if (!is.null(yaml)) {
    yaml.arg = yaml.load(paste0(yaml,collapse="\n"))
    args[names(yaml.arg)] = yaml.arg
  }
  args
}

default.navbar.link.fun = function(title, level, bi=NULL) {
  return(title)
  #paste0("<a role='button'>", title,"</a>")
}

default.navbar.li.fun = function(titles, child.li, levels=NULL,bis=NULL) {
  restore.point("default.navbar.li.fun")
  
  li = lapply(seq_along(titles), function(i) {
    child.ui = child.li[[i]]
    tabPanel(title = titles[i],value = paste0("rtutor_menu_tab_",bis[i]), child.ui)
  })
  ui = do.call(tabsetPanel,li)
  return(ui)
  #inner = paste0("<li>", titles, "\n", child.li, "</li>")
  #paste0("<ul>", paste0(inner, collapse="\n"),"</ul>")

}

default.navbar.outer.fun = function(inner) {
  inner
}

rtutor.navbar = function(ps, nav.levels = c("section","subsection","frame"), link.fun = default.navbar.link.fun, li.fun=default.navbar.li.fun, outer.fun=default.navbar.outer.fun) {
  restore.point("rtutor.navbar")
  
  bdf = ps$bdf
  nav.levels = intersect(nav.levels, bdf$type)
  
  get.level.li = function(nav.levels=nav.levels, parent.bi=NULL, level=1) {
    restore.point("rtutor.navbar.get.level.li")
    
    if (is.null(parent.bi)) {
      bis = which(bdf$type == nav.levels[1])
    } else {
      bis = which(bdf$parent_container==parent.bi)
    }
    types = bdf$type[bis]
    ignore = !types %in% nav.levels
    bis = bis[!ignore]
    types = types[!ignore]
    bi.levels = match(types, nav.levels)
  
    
    child.li = vector("list",length(bis))
    if (level < length(nav.levels)) {
      for (i in seq_along(bis)) {
        bi = bis[i]
        if (isTRUE(bi.levels[i]<length(nav.levels))) {
          child.li[[i]] = get.level.li(nav.levels=nav.levels, parent.bi = parent.bi, level=bi.levels[i])
        }
      }
    }
    titles = lapply(seq_along(bis), function(i) {
      bi = bis[i]
      obj = bdf$obj[[bi]]
      if (is.null(obj$title)) {
        title = paste0(bdf$type[[bi]]," ",bdf$stype.ind[[bi]])
      } else {
        title = obj$title
      }
      link.fun(title, bi.levels[i],bi=bi[[i]])
    })
    li.fun(titles, child.li,levels=bi.levels, bis=bis)
  }
  
  inner = get.level.li(nav.levels=nav.levels)
  outer.fun(inner)
  
} 

slide.title.bar.ui = function(title, slide.ind, num.slides) {
 
   div(class="rtutor-slide-title-bar",
    HTML("<table width='100%'><tr><td>"),
    h4(title),
    HTML("</td><td align='right' valign='top' nowrap>"),
    HTML("<table><tr><td valign='center' nowrap>"),    
    rtutor.navigate.btns(),
    HTML("</td><td valign='center' nowrap style='padding-left: 5px'>"),
    HTML(paste0(slide.ind, " of ",num.slides)),      
    HTML("</td></tr></table>"),
    HTML("</td></tr></table>")
  )  
}

