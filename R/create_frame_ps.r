examples.frame.ps = function() {
  library(EconCurves)
  setwd("D:/libraries/RTutor2")
  txt = readLines("test.rmd")
  frame.ind = NULL
  te = rtutor.make.frame.ps.te(txt, bdf.filter=bdf.frame.filter(frame.ind=frame.ind))
  te$lang = "de"
  bdf = te$bdf
  ui = make.te.ui(te=te)
  view.html(ui=ui)
  
}

rtutor.make.frame.ps.te = function(txt,addons="quiz",bdf.filter = NULL,dir=getwd(), figure.dir=paste0(dir,"/figure"),...) {
  restore.point("rtutor.make.frame.ps.te")

  te = new.env()
  te$addons = addons
  te$Addons = make.addons.list(addons)
  te$dir = dir
  te$figure.dir = figure.dir
  
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
  parent.types = c("frame","row", "column","chunk","preknit","precompute","knit","compute","info")
  pt = get.levels.parents.by.types(df$level, df$type, parent.types)

  bdf = cbind(data.frame(index = 1:NROW(df)),df,pt) %>% as_data_frame
  bdf$obj = bdf$ui = vector("list", NROW(bdf))
  bdf$prefixed = bdf$is.addon = bdf$has.handler =  FALSE
  bdf$task.rmd = bdf$out.rmd = bdf$sol.rmd = ""
  
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
    res = try(rtutor.parse.block(bi,te), silent=TRUE)
    if (is(res,"try-error")) {
      br = te$bdf[bi,]
      msg = paste0("Error when parsing ",br$type," block lines ", br$start+te$txt.start-1, " to ", br$end+te$txt.start-1,"\n\n", as.character(res))
      stop(msg)
    }
  }
  te
}


adapt.for.back.to.blocks = function(bdf,te, line.start=te$txt.start) {
  restore.point("adapt.for.back.to.blocks")
  
  is_backto = str.starts.with(bdf$type,"back_to_")
  bttype = str.right.of(bdf$type[is_backto],"back_to_")
  rows = which(is_backto)
  if (length(rows)==0) return(bdf)
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

  type = te$bdf$type[[bi]]
  if (type %in% te$addons) {
    bdf = te$bdf; br = bdf[bi,];
    str = te$txt[br$start:br$end]
    
    ao = te$Addons[[type]]$parse.fun(str, id = paste0(type,"__",bi))
    ui = te$Addons[[type]]$shiny.ui.fun(ao)
    set.bdf.ui(ui,bi,te)
    te$bdf$obj[[bi]] = list(ao=ao)
    te$bdf$is.addon[[bi]] = te$bdf$has.handler[[bi]] = TRUE
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
    expr = parse(text=code)
    res = eval(expr,te$pre.env)
    te$bdf$obj[[bi]]$pre.env = copy.env(te$pre.env)
    te$bdf$prefixed[[bi]] = TRUE
    te$bdf[bi,c("task.rmd","sol.rmd","out.rmd")] = mstr
  } else if (chunk.preknit) {
    ui = knit.rmd(str,envir = te$pre.env,out.type="shiny")
    set.bdf.ui(ui, bi,te)
    te$bdf[bi,c("task.rmd","sol.rmd","out.rmd")] = mstr
  } else {
    # chunk that user must enter
    set.bdf.ui(uiOutput(paste0("chunkUI__",bi)),bi,te)
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
  te$bdf[bi,c("task.rmd","sol.rmd","out.rmd")] = c(merge.lines(res$task.rmd),merge.lines(res$sol.rmd),merge.lines(res$out.rmd))
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


rtutor.parse.column = function(bi,te) {
  restore.point("rtutor.parse.column")
  bdf = te$bdf; br = bdf[bi,];
  
  children = bdf$parent == bi 
  res = get.children.and.fragments.ui.list(bi,te, keep.null=FALSE, children=children)
  te$bdf[bi,c("task.rmd","sol.rmd","out.rmd")] = c(merge.lines(res$task.rmd),merge.lines(res$sol.rmd),merge.lines(res$out.rmd))
  
  
  ui.li = res$ui.li
  args = parse.block.args(arg.str = br$arg.str)

  if (is.null(args$width)) args$width = 6 
  if (is.null(args$offset)) args$offset = 0 
  ui = column(width = args$width, offset=args$offset,
    ui.li
  )
  
  set.bdf.ui(ui,bi,te)
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

  if (is.null(args$link))
  if (has.file) {
    html = paste0('<img src=figure/',args$file,wh,'>')
  } else if (!is.null(args$url)) {
    html = paste0('<img src=',args$url,wh,'>')
  } else {
    html('<p>...figure here...</p>')
  }
  if (is.null(args$name)) {
    name.html = ""
  } else {
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
  restore.point("rtutor.parse.success")
  res = get.children.and.fragments.ui.list(bi,te, keep.null=FALSE)
  ui.li = res$ui.li
  te$bdf[bi,c("task.rmd","sol.rmd","out.rmd")] = c(merge.lines(res$task.rmd),merge.lines(res$sol.rmd),merge.lines(res$out.rmd))

  id = paste0("solved_block__",bi)
  te$bdf$obj[[bi]] = list(ui = ui.li, id=id)
  set.bdf.ui(uiOutput(id),bi,te)
}


rtutor.parse.row = function(bi,te) {
  restore.point("rtutor.parse.row")
  bdf = te$bdf; br = bdf[bi,];
  
  children = bdf$parent == bi 
  res = get.children.and.fragments.ui.list(bi,te, keep.null=FALSE, children=children)
  te$bdf[bi,c("task.rmd","sol.rmd","out.rmd")] = c(merge.lines(res$task.rmd),merge.lines(res$sol.rmd),merge.lines(res$out.rmd))
  
  ui.li = res$ui.li
  ui = fluidRow(
    ui.li
  )
  set.bdf.ui(ui,bi,te)
}


rtutor.parse.frame = function(bi,te) {
  restore.point("rtutor.parse.frame")
  #stop()
  bdf = te$bdf; br = bdf[bi,];
  args = parse.block.args(arg.str = br$arg.str)
  
  children = bdf$parent == bi 
  res = get.children.and.fragments.ui.list(bi,te, children=children, keep.null=TRUE)
  title = paste0("## Frame ", args$name)
  te$bdf[bi,c("task.rmd","sol.rmd","out.rmd")] = c(merge.lines(c(title,res$task.rmd)),merge.lines(c(title,res$sol.rmd)),merge.lines(c(title,res$out.rmd)))
  
  ui.li = res$ui.li
  is.child = !res$is.frag

  if (is.null(args$title.offset)) args$title.offset=0
  if (!is.null(args$name)) {
    title = fluidRow(column(offset=args$title.offset,width=12-args$title.offset,h4(args$name)))
  } else {
    title = NULL
  }
  ui = tagList(
    title,
    ui.li
  )
  
  set.bdf.ui(ui,bi,te)
}

rtutor.parse.info = rtutor.parse.note =  function(bi,te) {
  restore.point("rtutor.parse.info")
  #stop()
  bdf = te$bdf; br = bdf[bi,];
  args = parse.block.args(arg.str = br$arg.str)
  
  children = bdf$parent == bi 
  res = get.children.and.fragments.ui.list(bi,te, children=children, keep.null=TRUE)
  
  ui.li = res$ui.li
  is.child = !res$is.frag

  if (!is.null(args$name)) {
    title = args$name
  } else {
    title = "Info"
  }
  ui = make.rtutor.collapse.note(id=paste0("info_collapse_",bi),content=ui.li, title=title)  
  set.bdf.ui(ui,bi,te)
  
  head = paste0("### ", title)
  foot = paste0("---")
  te$bdf[bi,c("task.rmd","sol.rmd","out.rmd")] = c(merge.lines(c(head,res$task.rmd,foot)),merge.lines(c(head,res$sol.rmd,foot)),merge.lines(c(head,res$out.rmd,foot)))

}


rtutor.parse.references = function(bi,te) {
  restore.point("references.block.render")
  title = "References"
  if (isTRUE(te$lang=="de")) {
    title = "Referenzen"
  }
  str = get.bi.te.str(bi,te)
  html = md2html(str)
  ui = make.rtutor.collapse.note(id=paste0("ref_collapse_",bi),html=html, title)
  set.bdf.ui(ui,bi,te)  

  head = paste0("### ", title)
  foot = paste0("---")
  te$bdf[bi,c("task.rmd","sol.rmd","out.rmd")] = c(merge.lines(c(head,res$task.rmd,foot)),merge.lines(c(head,res$sol.rmd,foot)),merge.lines(c(head,res$out.rmd,foot)))

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
  ui = sol.rmd = task.rmd = out.rmd = res$frag

  ui[is.frag] = lapply(ui[is.frag], function(txt) {
    HTML(md2html(txt, fragment.only = TRUE))
  })
  
  ui[is.child] = lapply(which(children), function(ind) {
    bdf$ui[[ind]]
  })
  sol.rmd[is.child] = bdf$sol.rmd[children]
  task.rmd[is.child] = bdf$task.rmd[children]
  out.rmd[is.child] = bdf$out.rmd[children]

    
  if (!keep.null) {
    null.ui = sapply(ui, is.null)
    ui = ui[!is.null(ui)]
    is.frag = is.frag[null.ui]
  }
  names(ui) = NULL
  list(ui.li=ui, sol.rmd=sol.rmd,task.rmd=task.rmd,out.rmd=out.rmd, is.frag=is.frag)
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
  right[has.comma] = substring(str[has.comma],comma.pos)
  right[!has.comma & !has.eq] = "}"
  
  new =  paste0("```{r ",name,right)
  txt[chunk.lines] = new
  txt
}

bdf.frame.filter = function(line=NULL,frame.ind=NULL,type.ind=frame.ind,bdf.ind=NULL,type,keep.precompute=TRUE) {
  bdf.type.filter(line,type.ind,bdf.ind,type="frame", keep.precompute=keep.precompute)
}

bdf.type.filter = function(line=NULL,type.ind=NULL,bdf.ind=NULL,type,keep.precompute=TRUE) {
  outer.type = type
  
  types.to.keep = NULL
  if (keep.precompute) {
    types.to.keep = "precompute"
  }
  function(bdf, te=NULL) {
    restore.point("in.bdf.type.filer")
    
    bdf.ind = get.bdf.ind(line=line,type.ind=type.ind,bdf.ind=bdf.ind,bdf=bdf,te=te,type=type)
    if (is.null(bdf.ind)) return(bdf)
    
    child.ind = which(bdf[,paste0("parent_",type)] == bdf.ind)
    keep = bdf$type %in% types.to.keep & bdf$index <= bdf.ind
    for (ktype in types.to.keep) {
      keep = keep | (bdf[,paste0("parent_",ktype)] >0 & bdf$index <= bdf.ind)
    }
    keep.ind = which(keep)
    
    rows = sort(unique(c(keep.ind,bdf.ind,child.ind)))
    bdf[rows,,drop=FALSE]
  }
  
}

get.bdf.ind = function(line=NULL,type.ind=NULL, bdf.ind=NULL, bdf=NULL, type=NULL, te=NULL) {
  if (!is.null(bdf.ind)) return(bdf.ind)
  if (!is.null(type.ind)) {
    return(bdf$index[bdf$type==type][type.ind]) 
  }
  if (!is.null(line)) {
    return(line.to.bdf.ind(line=line,bdf=bdf, type=type,te=te))
  }
  return(NULL)
}

line.to.bdf.ind = function(line,bdf,type=NULL,txt.start = if (is.null(te$txt.start)) 1 else te$txt.start, te=NULL) {
  restore.point("line.to.bdf.ind")
  
  line = line-txt.start+1
  if (!is.null(type)) {
    df = bdf[bdf$type==type,,drop=FALSE]
  } else {
    df = bdf
  }
  rows = which(df$start <= line & df$end >= line)
  if (length(rows)>1) {
    return(df$index[rows[which.max(df$start[rows])]])
  }
  return(df$index[rows])
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
