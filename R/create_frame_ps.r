examples.frame.ps = function() {
  library(yaml)
  setwd("D:/libraries/RTutor2/examples/sporer")
  txt = readLines("RTutorEnvironmentalRegulations_sol.Rmd")
  setwd("D:/libraries/RTutor2/examples/examples")
  txt = readLines("Example2_sol.Rmd")
  
  #setwd("D:/libraries/RTutor2/examples/auction")
  #txt = readLines("auction_sol.Rmd")
  popts=default.ps.opts(
    ps.type = "rmd",
    show.solution.btn = TRUE,
    slides = FALSE,
    static.type = c("section","subsection"),
    lang = "de")
  ps = rtutor.make.frame.ps(txt,catch.errors = FALSE, priority.opts=popts, ps.type="rmd")

  write.ps.rmd(ps)
  
  bdf = ps$bdf
  rmc = ps$rmc
  
  restore.point.options(display.restore.point = !TRUE)

  app = rtutorApp(ps)
  viewApp(app,launch.browser = TRUE)

}

rtutor.builtin.types = function() {
  c(
    "chunk","frame","section","subsection","subsubsection",
    "exercise",
    "preknit","precompute","portrait", "image", "solved",
    "column","row","ps","info","note","award","references",
    "show","notest","show_notest","hint","test","test_args",
    "settings","css","head","layout",
    "gv"
  )
}



rtutor.make.frame.ps = function(txt,bdf.filter = NULL,dir=getwd(), figure.dir=paste0(dir,"/",figure.sub.dir), figure.sub.dir = "figure", plugins=c("stats","export","dataexplorer"),catch.errors=TRUE,ps.name = "ps", ps.id=ps.name, opts=default.ps.opts(ps.type=ps.type, use.memoise=use.memoise), priority.opts=list(), figure.web.dir = "figure", filter.line=NULL, filter.type="auto", show.line=NULL, source.file="main", libs=NULL, check.old.rtutor.sol=TRUE, extra.code.file=NULL, use.memoise = FALSE, ps.type=c("shiny","slides","rmd") , ...) {
  restore.point("rtutor.make.frame.ps")

  ps = new.env()
  
  ps$version = 0.1
  ps$figure.web.dir = figure.web.dir
  ps$figure.sub.dir = figure.sub.dir
  if (!dir.exists(figure.dir)) {
    dir.create(figure.dir)
  }
  ps$ps.name = ps.name
  ps$ps.id = ps.id
  ps$Addons = list()
  ps$dir = dir
  ps$figure.dir = figure.dir
  ps$plugins = plugins
  ps$css = ps$head = NULL
  
  if (length(txt)==1)  
    txt = sep.lines(txt)

  if (is.old.rtutor.sol(txt)) {
    cat(paste0('\nYour solution file looks like old RTutor format. You can update it by calling the function\n
      translate.old.rtutor.sol()'))
    txt = translate.old.rtutor.sol(txt = txt)
  }
  
  cat("\n\nCompile problem set", ps.name, "...")
  
  #Encoding(txt) = "UTF8"
  txt = mark_utf8(txt)
  

  adapt.ignore.include(ps=ps,txt=txt, source.file=source.file)
  txt = ps$txt
  
  # add outer container
  txt = c("#. ps",txt)
  ps$text.info = rbind(data_frame(line=NA,source=NA),ps$text.info)
  
  txt = fast.name.rmd.chunks(txt)
  
  dot.levels = rtutor.dot.levels()
  df = find.rmd.nested(txt, dot.levels)
  
 
  # settings in rmd file overwrite opts
  bis = which(df$type == "settings")
  for (bi in bis) {
    yaml = paste0(txt[(df$start[bi]+1):(df$end[bi]-1)], collapse="\n")
    so = read.yaml(text=yaml, keep.quotes=FALSE)
    opts[names(so)] = so
  }

  # priority opts overwrite settings and opts
  opts[names(priority.opts)] = priority.opts
  set.rt.opts(opts)
  ps$opts = opts

  ps$static.types = opts$static.types


  df = find.rmd.nested(txt, dot.levels)
    
  # remove blocks inside addons
  # those will be dealt by the addons themselves
  builtin.types = rtutor.builtin.types()
  df$is.addon = !(df$type %in% builtin.types)
  parent_addon = get.levels.parents(df$level,df$is.addon)
  del.rows = which(parent_addon>0)
  df = del.rows.and.adapt.refs(df,del.rows,ref.cols = "parent")
  

  ps$slides = opts$slides
  ps$slide.type = opts$slide.type
  
  if (ps$slide.type=="auto") 
    ps$slide.type = find.bdf.auto.slide.type(df)
  
  if (ps$slides) {
    if (ps$slide.type %in% ps$static.types) {
      ps$hidden.container.types = ps$slide.type 
    }
  } else {
    ps$hidden.container.types = opts$menu.levels
  }

  ps$txt = txt
  
  df = adapt.for.back.to.blocks(df,ps=ps)
  df$stype = df$type
  
  ps$addons = unique(df$type[df$is.addon])
  ps$Addons = list()
  for (addon in ps$addons) {
    rtutor.init.Addon(ps=ps,addon=addon)
  }

  parent.types = c("frame","row", "column","chunk","preknit","precompute","knit","compute","info","note", "section","subsection","exercise")
  pt = get.levels.parents.by.types(df$level, df$type, parent.types)
  bdf = cbind(data.frame(index = 1:NROW(df)),df,pt) %>% as_data_frame
  bdf$obj = bdf$ui = bdf$inner.ui = vector("list", NROW(bdf))
  bdf = mutate(bdf,
    stype.ind = 0,
    id = paste0(type,"__",index,"__",ps.id),
    name = NA_character_,
    has.handler = FALSE,
    is.task = FALSE,task.ind = 0,
    is.container=FALSE,container.ind = 0,
    is.static = TRUE,
    div.id = "",
    output.id=  "",   
    shown.rmd = "", out.rmd = "",sol.rmd = "",
    # determines required computation order of chunks / tasks
    task.line = NA_character_,
    task.in = vector("list", NROW(bdf)),
    task.listeners = vector("list", NROW(bdf)),
    
    # These arguments deal with task.envs
    need.task.env = FALSE,
    change.task.env = FALSE,
    presolve.task = opts$presolve
  )
  
  
  # Filter bdf if only a subset of elements shall be compiled / shown
  if (!is.null(filter.line)) {
    # filter problem set
    line = source.line.to.line(filter.line,ps = ps,source = 1)
    bdf = bdf.part.filter(line=line)(bdf=bdf)
    bdf = shorten.bdf.index(bdf)
    lines = c(unlist(lapply(2:NROW(bdf), function(row) bdf$start[row]:bdf$end[row])))
    ps$txt[-lines] = ""
  } else if (!is.null(show.line) & isTRUE(ps$slides)) {
    # set start slide to current source line
    bi = source.line.to.bi(line = show.line,source = 1,bdf=bdf,ps=ps,type = ps$slide.type)
    ps$start.slide = sum(seq_len(NROW(bdf))<=bi & bdf$stype == ps$slide.type)
  }

  ps$bdf = bdf
  
  # Load libs and create envs
  ps$opts$libs = unique(c(ps$opts$libs,libs))
  load.ps.libs(ps$opts$libs)
  
  ps$init.env = new.env(parent=parent.env(globalenv()))
  ps$pre.env = new.env(parent=ps$init.env)
  ps$env = new.env(parent=ps$init.env)

  if (isTRUE(ps$opts$use.memoise))
    ps$memoise.fun.li = memoise.fun.li(ps$opts$memoise.funs)

    
  source.extra.code.file(extra.code.file = extra.code.file, ps=ps)

  # set knitr output for data frames
  do.call(set.knit.print.opts, c(list(output="html"),ps$opts$knit.print.param))
  
  # Additional information for slides
  if (ps$slides) {
    ps$num.slides = sum(ps$bdf$type==ps$slide.type)
    ps$slide.bis = which(ps$bdf$type==ps$slide.type)
    
  }

  # Preparse blocks from outer to inner,
  # i.e. ordered by start
  binds = order(bdf$start)
  bi = binds[1]
  for (bi in binds) {
    if (catch.errors) {
      res = try(rtutor.preparse.block(bi,ps), silent=TRUE)
      if (is(res,"try-error")) {
        br = ps$bdf[bi,]
        source = block.source.msg(bi=bi,ps=ps)
        msg = paste0("Error when preparsing ",br$type," block. Source: \n",source, "\n\n", paste0(as.character(res), collape="\n"))
        stop(msg)
      }
    } else {
      res = rtutor.preparse.block(bi,ps)  
    }
  }
  

  
  # Go through blocks and chunks, ordered by end
  binds = order(bdf$end, -bdf$start)
  bi = binds[1]
  for (bi in binds) {
    #restore.point("inner.make.ps")
    if (catch.errors) {
      res = try(rtutor.parse.block(bi,ps), silent=TRUE)
      if (is(res,"try-error")) {
        br = ps$bdf[bi,]
        source = block.source.msg(bi=bi,ps=ps)
        msg = paste0("Error when parsing ",br$type," block. Source: \n",source, "\n\n", paste0(as.character(res), collape="\n"))
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
  create.ps.tasks(ps=ps)
  
  create.ps.rmc(ps=ps)
  
  # specify containers
  ps$bdf$container.ind = cumsum(ps$bdf$is.container) * ps$bdf$is.container
  
  ps$bdf$parent_container = get.levels.parents(ps$bdf$level, ps$bdf$is.container) 
  
  
  ps$navbar.ui = rtutor.navbar(ps=ps, nav.levels = opts$nav.levels)
  
  
  remove.existing.ups(ps.name=ps.name, dir=dir)
  write.rps(ps=ps,dir=dir)
  ps
}

write.rps = function(ps, file.name=paste0(dir,"/",ps$ps.name,".rps"), dir=getwd()) {
  restore.point("write.rps")
  
  suppressWarnings(saveRDS(ps, file.name))
}

read.rps = function(file.name=paste0(dir,"/",ps.name,".rps"), dir=getwd(), ps.name="") {
  readRDS(file.name)
}


source.line.to.line = function(line, ps, source=1) {
  restore.point("source.line.to.line")
  lines = (which(ps$txt.lines <= line & ps$txt.source == source))
  if (length(lines)==0) return(1)
  return(max(lines))
}

source.line.to.bi = function(line,bdf=ps$bdf,ps, source=1,type=NULL) {
  restore.point("source.line.to.bi")
  line = source.line.to.line(line,ps,source)

  if (is.null(type)) {
    bi = max(which(bdf$start <= line))
  } else {
    bis = which(bdf$start <= line & bdf$type==type)
    if (length(bis)==0) {
      bi = min(which(bdf$type==type))      
    } else {
      bi = max(bis)
    }
  }
  bi
}

block.source.msg = function(bi, ps) {
  restore.point("block.source.msg")
  
  br = ps$bdf[bi,]
  lines = br$start:br$end
  df = data_frame(line=ps$txt.lines[lines],source=ps$txt.source[lines])
  sdf = summarise(group_by(df, source), start=min(line),end=max(line))
  sdf$file = ps$source.files[sdf$source]
  
  paste0(sdf$file, " lines ",sdf$start, " to ", sdf$end, collapse="\n")

}

# Some default packages for addons
# Can be used to automatically load packages
rtutor.addon.packages = function() {
  c(pane="EconCurves",plotpane="EconCurves",panequiz="EconCurves")
}

rtutor.init.Addon = function(ps, addon) {
  restore.point("rtutor.init.Addon")
  addon.fun = paste0("rtutor.addon.",addon)
  if (exists(addon.fun)) {
    ps$Addons[[addon]] = do.call(addon.fun,list())
  } else {
    pkgs = rtutor.addon.packages()
    if (!addon %in% names(pkgs)) {
      warning(paste0("\nWe don't have a built-in block type ", addon,". Bute I could not find the function ", addon.fun, " that identifies the block as an addon. (Have you loaded the required package with the addon?"))
      ps$addons = setdiff(ps$addons,addon)
      stop()
    } else {
      if (require(pkgs[addon],character.only = TRUE)) {
        warning(paste0("\nI loaded the package ", pkgs[addon], " for the addon ", addon,".")) 
        ps$Addons[[addon]] = do.call(addon.fun,list())
      } else {
        warning(paste0("\nWe don't have a built-in block type ", addon,". This addon is defined in the package ", pkgs[addon]," but you have not installed that package."))
        stop()
      }
    }
    
  }
}

adapt.ignore.include = function(ps, txt=ps$txt, source.file="main") {
  restore.point("adapt.ignore.include")

    # Only capture elements between the lines <!-- START --> and <!-- END -->
  res = rmd.between.start.end.lines(txt,return.start.end = TRUE)
  txt = res$txt
  ps$txt.lines = seq_along(txt)+res$start-1
  ps$txt.source = rep(1, length(ps$txt.lines))
  ps$source.files = source.file

  changed.ig = changed.in = TRUE
  counter = 0
  ps$txt = txt
  while(changed.ig | changed.in) {
    counter = counter+1
    changed.ig = adapt.ignore(ps)
    changed.in = adapt.include(ps)
    if (counter > 300) {
      stop("#. include statements were nested deeper than 300 levels. Probably there is a recursion...")
    }
  }
  invisible()
}

adapt.ignore = function(ps,txt=ps$txt) {
  restore.point("adapt.ignore")
  
  # all rows that will be deleted 
  # in this precompilation state
  del.lines = NULL
  df = find.rmd.blocks(txt)
  
  # remove content in ignore blocks
  ig.rows = which(df$type=="ignore")
  if (length(ig.rows)>0) {
    del.lines = c(del.lines,unlist(lapply(ig.rows, function(ig.row) df$start[ig.row]:df$end[ig.row])))
  }  

  if (length(del.lines)==0) return(FALSE)
  del.lines =unique(del.lines)
  txt = txt[-del.lines]
  ps$txt.lines = ps$txt.lines[-del.lines]
  ps$txt.source = ps$txt.source[-del.lines]
  
  ps$txt = txt
  return(TRUE)
}

adapt.include = function(ps,txt=ps$txt) {
  restore.point("adapt.include")
  lines = which(str.starts.with(txt,"#. include "))
  if (length(lines)==0) return(FALSE)
  
  files = str.trim(str.right.of(txt[lines],"#. include "))
  
  i = 1
  for (i in seq_along(lines)) {
    file = files[i]
    source = match(file, ps$source.files)
    if (is.na(source)) {
      ps$source.files = c(ps$source.files, file)
      source = length(ps$source.files)
    }
    line = lines[i]
    ntxt = readLines(paste0(ps$dir,"/",file),warn=FALSE,encoding = "UTF8")
    ntxt = mark_utf8(ntxt)
    ps$txt = insert.into.vec(txt,ntxt,pos=line, replace=TRUE)
    ps$txt.lines = insert.into.vec(ps$txt.lines,seq_along(ntxt),pos=line, replace=TRUE)
    
    ps$txt.source = insert.into.vec(ps$txt.source,rep(source,length(ntxt)),pos=line, replace=TRUE)    
    lines = lines+length(ntxt)-1
    txt = ps$txt
  }
  return(TRUE)
}

insert.into.vec = function(vec, new, pos, replace=FALSE) {
  restore.point("insert.into.vec")
  
  keep.left = seq_len(min(pos)-1)
  
  if (replace) {
    keep.right = if (max(pos)<length(vec)) (max(pos)+1):length(vec) else integer(0)  
  } else {
    keep.right= if (min(pos)<=length(vec)) (min(pos)):length(vec) else integer(0)    
  }
  
  c(vec[keep.left],new,vec[keep.right])  
  
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
  #pvec[pvec==0] = 0
  nmat = matrix(long[as.numeric(pvec)],NROW(pmat),ncol(pmat))
  nmat[pmat==0] = 0
  nmat[is.na(nmat)] = 0
  
  bdf[,cols] = nmat
  rows = bdf$parent == 0 & bdf$index > 1
  bdf$parent[rows] = 1
  bdf$level[rows] = 2
  
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


rtutor.preparse.block = function(bi,ps, opts = ps$opts) {
  restore.point("rtutor.preparse.block")
  type = ps$bdf$type[[bi]]
  
  if (type %in% c(opts$new.task.line.parts,opts$nested.task.line.parts)) {
    set.task.line(bi=bi,ps=ps, opts=opts)    
  }
}


rtutor.parse.block = function(bi,ps) {
  restore.point("rtutor.parse.block")
  
  # Don't parse blocks inside chunks here
  if (ps$bdf$parent_chunk[[bi]] >0) return()

  type = ps$bdf$type[[bi]]
  fun.name = paste0("rtutor.parse.",type)
  
  if (ps$bdf$is.addon[[bi]]) {
    return(rtutor.parse.addon(bi=bi, ps=ps))
  }
  
  fun.call = parse(text=paste0("rtutor.parse.",type,"(bi,ps)"))
  res = eval(fun.call)
  res
  #fun(bi,ps)
}

rtutor.parse.settings = function(bi, ps,...) {
  
}

rtutor.parse.css = function(bi, ps,...) {
  css = ps$txt[(ps$bdf$start[bi]+1):(ps$bdf$end[bi]-1)]
  ps$css = paste0(c(ps[["css"]],css),collapse="\n")
}


rtutor.parse.head = function(bi, ps,...) {
  head = ps$txt[(ps$bdf$start[bi]+1):(ps$bdf$end[bi]-1)]
  ps$head = paste0(c(ps[["head"]],head),collapse="\n")
}


rtutor.parse.layout = function(bi, ps,...) {
  restore.point("rtutor.parse.layout")
  br = ps$bdf[bi,]
  name = br$arg.str
  ps$bdf$name[bi] = name
  inner = ps$txt[(ps$bdf$start[bi]+1):(ps$bdf$end[bi]-1)]
  li = parse.hashdot.yaml(inner, hashdot="##. ")
  li$name = name
  ps$bdf$obj[[bi]] = li
  if (is.null(ps[["layouts"]]))
    ps$layouts = list()
  
  ps$layouts[[name]] = li
}



rtutor.parse.addon = function(bi, ps, opts=ps$opts) {
  restore.point("rtutor.parse.addon")
  
  bdf = ps$bdf; br = bdf[bi,];
  type = br$type
  Ao = ps$Addons[[type]]
  
  args = parse.block.args(arg.str = br$arg.str)
  if (!is.null(args$name)) ps$bdf$name[[bi]] = args$name
  ao = Ao$parse.fun(
    block.txt = ps$txt[br$start:br$end],
    inner.txt = ps$txt[(br$start+1):(br$end-1)],
    id = paste0(type,"__",bi),
    args = args,
    type = type,
    bdf=bdf,
    bi = bi,
    ps = ps
  )
  if (!is.null(Ao$ui.fun)) {
    if (isTRUE(Ao$is.static)) {
      ui = Ao$ui.fun(ao=ao)
      set.bdf.ui(ui,bi,ps)
      ps$bdf$is.static[[bi]] = TRUE
    } else {
      # the addon will be put inside a container
      ps$bdf$is.container[[bi]] = TRUE
      set.container.div.and.output(bi,ps)
      ps$bdf$is.static[[bi]] = FALSE
    }
  }

  if (isTRUE(Ao$is.task)) {
    ps$bdf$is.task[[bi]] = Ao$is.task
    ao$task.ind = sum(ps$bdf$is.task[1:bi])
    
    create.bi.task.env.info(bi=bi,ps=ps,need.task.env = isTRUE(Ao$need.task.env),change.task.env = isTRUE(Ao$change.task.env),args=list(optional = TRUE),presolve.task = opts$presolve, opts=opts)  

  }
  ps$bdf$obj[[bi]] = list(ao=ao)
  
  
  return()
}

rtutor.parse.chunk = function(bi,ps) {
  restore.point("rtutor.parse.chunk")
  bdf = ps$bdf; br = bdf[bi,]; str = ps$txt[br$start:br$end]
  args = parse.chunk.args(header = str[1])
  
  if (!is.null(args$label))
    ps$bdf$name[bi] = args$label 
  
  chunk.precompute = br$parent_precompute >0 | isTRUE(args$precompute)
  chunk.preknit = isTRUE(args$preknit) | br$parent_info | br$parent_preknit
  chunk.task = isTRUE(args$task_chunk) | (!chunk.preknit & !chunk.precompute)
  
  code = str[-c(1,length(str))]
  mstr = merge.lines(str) 
  if (chunk.precompute) {
    restore.point("parse.precompute.chunk")
    ps$bdf$stype[[bi]] = "precompute_chunk"
    expr = parse(text=code)
    res = eval(expr,ps$pre.env)
    ps$bdf$obj[[bi]]$pre.env = copy.env(ps$pre.env)
    ps$bdf[bi,c("shown.rmd","sol.rmd","out.rmd")] = mstr
  }
  if (chunk.preknit) {
    restore.point("parse.preknit.chunk")
    ps$bdf$stype[[bi]] = "preknit_chunk"
    args$comment = NA
    rmd = code.to.rmd.chunk(code,args=args)
    ui = knit.rmd(rmd,envir = ps$pre.env,out.type="shiny")
    #ui = tagList(ui, highlight.code.script())
    set.bdf.ui(ui, bi,ps)
    ps$bdf[bi,c("shown.rmd","sol.rmd","out.rmd")] = mstr
  }
  if (chunk.task) {
    ps$bdf$stype[[bi]] = "task_chunk"
    # a task chunk is the classic RTutor chunk
    rtutor.parse.task.chunk(bi=bi,ps=ps,args=args)
  }
}

code.to.rmd.chunk = function(code, args, label=args$label) {
  restore.point("code.to.rmd.chunk")
  
  if (is.null(label)) {
    label = "chunk"
  }
  args = args[setdiff(names(args),"label")]
  args = lapply(args, function(arg) {
    if (is.character(arg)) return(paste0("'",arg,"'"))
    arg
  })
  if (length(args)>0) {
    head = paste0('```{r "',label, '", ',paste0(names(args)," = ", args, collapse=", "),'}')
  } else {
    head = paste0('```{r "',label, '" }')
  }
  c(head,code,"```")
}

rtutor.parse.preknit = function(bi,ps) {
  restore.point("rtutor.parse.preknit")
  bdf = ps$bdf; br = bdf[bi,];
  
  # new code: children are knitted
  children = bdf$parent == bi 
  res = get.children.and.fragments.ui.list(bi,ps, keep.null=FALSE, children=children)
  ui.li = res$ui.li
  set.bdf.ui(ui.li,bi,ps)
  set.bdf.rmd(bi=bi,ps = ps,res$shown.rmd,res$sol.rmd,res$out.rmd)
  return()
  
  # old code knit everything
  bdf = ps$bdf; br = bdf[bi,];
  str = ps$txt[(br$start+1):(br$end-1)]
  
  ui = knit.rmd(str,envir = ps$pre.env,out.type="shiny")
  set.bdf.ui(ui,bi,ps)
}

rtutor.parse.precompute = function(bi,ps) {
  # all work is done in the chunks inside
  rtutor.parse.as.container(bi,ps,is.static = TRUE)

}

rtutor.parse.gv = function(bi, ps) {
  restore.point("rtutor.parse.gv")
  arg.str= ps$bdf$arg.str[[bi]]
  
  bdf = ps$bdf; br = bdf[bi,];
  txt = get.bi.inner.txt(bi=bi, ps=ps)
  library(svgdiagram)
  svg = gv.to.svg(gv=txt, to.clipboard = FALSE)
  ui = HTML(svg)
  set.bdf.ui(ui,bi,ps)
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
  rtutor.parse.as.container(bi,ps, is.static=TRUE, only.children.ui = TRUE)
}



rtutor.parse.exercise = function(bi,ps) {
  restore.point("rtutor.parse.exercise")
  rtutor.parse.as.section(bi,ps,type="exercise", rmd.prefix="## Exercise")
}

rtutor.parse.section = function(bi,ps) {
  restore.point("rtutor.parse.section")
  rtutor.parse.as.section(bi,ps,type="section", rmd.prefix="## Section")
}

rtutor.parse.subsection = function(bi,ps) {
  restore.point("rtutor.parse.subsection")
  rtutor.parse.as.section(bi,ps,type="subsection", rmd.prefix="### Subsection")
}


rtutor.parse.subsubsection = function(bi,ps) {
  restore.point("rtutor.parse.subsection")
  rtutor.parse.as.section(bi,ps,type="subsubsection", rmd.prefix="####  Subsubsection")
}

rtutor.parse.frame = function(bi,ps) {
  restore.point("rtutor.parse.frame")
  rtutor.parse.as.section(bi,ps,type="frame", rmd.prefix="### Frame") 
}

rtutor.parse.as.section = function(bi, ps, type="section", rmd.prefix="# Section") {
  restore.point("rtutor.parse.as.section")
  bdf = ps$bdf; br = bdf[bi,];
  arg.str= ps$bdf$arg.str[[bi]]
  args = parse.block.args(arg.str =arg.str, allow.unquoted.title = TRUE)
  # extract layout in [ ]
  if (str.starts.with(arg.str,"[")) {
    args$layout.name = str.between(args$name,"[","]")
    args$layout = ps$layouts[[args$layout.name]]
    if (is.null(args[["layout"]])) {
      cat("\nWarning could not find layout", args$layout.name)
    } else {
      inner = get.bi.inner.txt(bi,ps=ps)
      args$layout.txt = sep.lines(txt.to.layout(txt=inner,layout=args$layout))
    }
    args$name = str.trim(str.right.of(args$name,']'))
  }
  title = first.non.null(args$title, args$name)
  rtutor.parse.as.container(bi,ps,args = args, rmd.prefix=paste0(rmd.prefix," ",title), title = title)
  if (is.null(args$title.offset)) args$title.offset=0
  button.label = str.left.of(args$name," --")
  ps$bdf$obj[[bi]] = list(title = args$name,button.label = button.label, args=args)
  
}


rtutor.parse.as.container = function(bi, ps,args=NULL, inner.ui = NULL, rmd.li=NULL, highlight.code = !is.static, is.static=ps$bdf$type[[bi]] %in% ps$static.types, rmd.head=NULL, rmd.prefix="", rmd.postfix="", ui.fun=NULL, title = ps$bdf$obj[[bi]]$title, is.hidden = ps$bdf$type[[bi]] %in% ps$hidden.container.types, extra.class = "", only.children.ui = FALSE) {
  restore.point("rtutor.parse.as.container")
  bdf = ps$bdf; br = bdf[bi,];
  if (is.null(inner.ui) | is.null(rmd.li)) {
    if (only.children.ui) {
      res = get.children.ui.list(bi,ps,keep.null=TRUE, layout.txt = args$layout.txt)
    } else {
      res = get.children.and.fragments.ui.list(bi,ps,keep.null=TRUE, layout.txt = args$layout.txt)
    }
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
  
  header = NULL
  # Add slide header
  if (ps$slides & identical(ps$slide.type,type)) {
    slide.ind = sum(ps$bdf$type[1:bi]==ps$slide.type)
    header = slide.title.bar.ui(title = title,slide.ind=slide.ind,num.slides = ps$num.slides) 
    
  # Add title as header
  } else {
    if (!is.null(title))
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
    set.container.div.and.output(bi,ps, is.hidden=is.hidden)
  
  # A static container will not be loaded in a uiOutput
  } else {
    restore.point("jnxjkfhiufhriuhf")
    
    style = ""
    if (is.hidden) style = "display: none;"
    
    ps$bdf$div.id[[bi]] = div.id = paste0(ps$prefix, br$id,"_div")
    
    div.class = paste0("rtutor-static-container-div ",type,"-container-div")
    if (isTRUE(ps$slides)) {
      if (type == ps$slide.type) {
        div.class = paste0(div.class," slide-container-div")
      }
    }
    
    ps$bdf$ui[[bi]] = div(id=div.id,class=div.class,  style=style,
      inner.ui
    )
  }
  ps$bdf$is.container[[bi]] = TRUE
}

container.title.html = function(title,type=NULL, ps=NULL, class=NULL) {
  if (is.null(title)) return(NULL)
  h4(class=class, title)      
}

set.container.div.and.output = function(bi, ps, is.hidden = ps$bdf$type[bi] %in% ps$hidden.container.types) {
  bdf = ps$bdf; br = bdf[bi,];
  
  style = ""
  if (is.hidden) style = "display: none;"

  
  ps$bdf$div.id[[bi]] = div.id = paste0(ps$prefix, br$id,"_div")
  ps$bdf$output.id[[bi]] = output.id = paste0(ps$prefix, br$id,"_output")
  type = br$stype
  div.class = paste0("rtutor-container-div ",type,"-container-div")
  if (isTRUE(ps$slide)) {
    if (type == ps$slide.type) {
      div.class = paste0(div.class," slide-container-div")
    }
  }
  ps$bdf$ui[[bi]] = div(id=div.id,class=div.class, style=style,
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

  title = args$name
  set.bdf.rmd(bi, ps, rmd.li=res,rmd.prefix = title)
  
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
  br = ps$bdf[bi,]
  
  args = parse.block.args(arg.str = ps$bdf$arg.str[[bi]])
  award.name = args$name
  
  res = get.children.and.fragments.ui.list(bi,ps, keep.null=FALSE)
  out.rmd = merge.lines(c("---\n### Award",res$out.rmd,"---"))
  rmd.li = list(shown.rmd="",sol.rmd="",out.rmd=out.rmd)
  content.ui=res$ui.li
  obj = list(award.bi =bi, award.name=award.name, html=as.character(tagList(content.ui)), txt = res$out.rmd)

  title = paste0("Award: ",award.name) 
  
  inner.ui = tagList(br(),shinyBS::bsCollapse(id = paste0("award_collapse_",bi), myCollapsePanel(title=title,header.style="background-color: #DFC463;box-shadow: 2px 2px 2px #888888;",content.ui)))

  rtutor.parse.as.container(bi=bi,ps=ps,args = args, inner.ui=inner.ui,rmd.li = rmd.li,highlight.code = TRUE,is.static = TRUE,title = NULL, is.hidden = TRUE)  
}

rtutor.parse.references = function(bi,ps) {
  restore.point("references.block.render")
  title = "References"
  if (isTRUE(ps$opts$lang=="de")) {
    title = "Referenzen"
  }
  rtutor.parse.as.collapse(bi,ps, title=title, is.static=TRUE)
}

rtutor.parse.as.collapse  =  function(bi,ps,title.prefix=NULL, title=NULL, rmd.head=paste0("### ", title), rmd.foot="---",is.static=TRUE,...) {
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
  rtutor.parse.as.container(bi,ps,args=args, inner.ui=inner.ui, rmd.li=rmd.li, is.static=is.static,...)
}


set.bdf.rmd = function(bi, ps, shown.rmd=rmd.li$shown.rmd, sol.rmd=rmd.li$sol.rmd, out.rmd = rmd.li$out.rmd, rmd.li=NULL, rmd.prefix="", rmd.postfix) {
  restore.point("set.bdf.rmd")
  
  ps$bdf[bi,c("shown.rmd","sol.rmd","out.rmd")] = c(
    merge.lines(c(rmd.prefix,unlist(shown.rmd),rmd.postfix)),
    merge.lines(c(rmd.prefix,unlist(sol.rmd),rmd.postfix)),
    merge.lines(c(rmd.prefix,unlist(out.rmd),rmd.postfix))
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

fragment.to.html = function(txt, bi, ps) {
  restore.point("fragment.to.html")
  
  if (isTRUE(ps$opts$use.whiskers)) {
    .whiskers = ps$pre.env[[".whiskers"]]
    if (length(.whiskers)>0)
      txt = replace.whiskers(paste0(txt,collapse="\n"),.whiskers)
  }
  
  #txt = unicode.html.math(txt)
  
  HTML(md2html(txt, fragment.only = TRUE))  
}

get.children.ui.list = function(bi,ps,bdf=ps$bdf, keep.null=TRUE, empty.as.null=FALSE, children=ps$bdf$parent == bi, layout.txt=NULL) {
  restore.point("get.children.ui.list")
  

  ui = lapply(which(children), function(ind) {
    bdf$ui[[ind]]
  })
  sol.rmd = bdf$sol.rmd[children]
  shown.rmd = bdf$shown.rmd[children]
  out.rmd = bdf$out.rmd[children]

  if (!keep.null) {
    null.ui = sapply(ui, is.null)
    ui = ui[!is.null(ui)]
  }
  names(ui) = NULL
  list(ui.li=ui, sol.rmd=sol.rmd,shown.rmd=shown.rmd,out.rmd=out.rmd, is.frag=rep(FALSE,length(ui)))
}


get.children.and.fragments.ui.list = function(bi,ps,bdf=ps$bdf, keep.null=TRUE, empty.as.null=FALSE, children=ps$bdf$parent == bi, layout.txt=NULL) {
  restore.point("get.children.and.fragments.ui.list")
  
  res = get.non.children.fragments(bi,ps, child.ind = which(children), layout.txt=layout.txt)
  is.frag = res$is.frag
  is.child = !is.frag
  ui = sol.rmd = shown.rmd = out.rmd = res$frag

  ui[is.frag] = lapply(ui[is.frag], function(txt) {
    fragment.to.html(txt=txt, bi=bi, ps=ps)
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

get.non.children.fragments = function(bi,ps,bdf=ps$bdf, child.ind = which(bdf$parent == bi), keep.header.footer=FALSE, layout.txt = NULL) {
  restore.point("get.non.children.fragments")
  
  if (!is.null(layout.txt)) {
    return(get.non.children.fragments.from.layout.txt(bi,ps,bdf, child.ind, keep.header.footer, layout.txt))
  }
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

get.non.children.fragments.from.layout.txt = function(bi,ps,bdf=ps$bdf, child.ind = which(bdf$parent == bi), keep.header.footer=FALSE, layout.txt = NULL) {
  restore.point("get.non.children.fragments.from.layout.txt")
  child.header = ps$txt[bdf$start[child.ind]]
  
  child.start = match(child.header, layout.txt)
  child.len = bdf$end[child.ind]-bdf$start[child.ind]+1
  child.end = child.start + child.len -1
  
  cpos = cbind(child.start, child.end)
  pos = pos.complement(cpos, is.sorted=TRUE, keep.pos=TRUE, start=1, end = length(layout.txt))
  is.frag = attr(pos,"complement")
  
  # we may have one end line too much
  valid = pos[,1]<=pos[,2]
  pos = pos[valid,,drop=FALSE]
  is.frag = is.frag[valid]
  
  if (NROW(pos)==0) return(NULL)
  
  frag.li = lapply(1:NROW(pos), function(row) {
    if (!is.frag[row]) return(NULL)
    layout.txt[pos[row,1]:pos[row,2]]
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

del.rows.and.adapt.refs = function(df, del.rows, ref.cols=NULL) {
  restore.point("del.rows.and.adapt.refs")
  
  if (NROW(df)==0 | NROW(del.rows)==0) return(df)
  if (!is.logical(del.rows)) 
    del.rows = (1:NROW(df)) %in% del.rows
    
  cum.del = cumsum(del.rows)
  #rbind(row = 1:NROW(df), parent = df[[col]], del.rows, cum.del, pcum.del = cum.del[ df[[col]] ] )

  for (col in ref.cols) {
    ref = df[[col]]
    valid = ref %in% 1:NROW(df)
    ref = ref[valid]
    df[[col]][valid] = ref - cum.del[ ref ]
  }
  df = df[!del.rows,,drop=FALSE]
  rownames(df) = NULL
  df  
}

get.bi.inner.txt = function(bi,txt = ps$txt, ps) {
  restore.point("get.bi.inner.txt")
  bdf = ps$bdf
  has.footer = bdf$form[[bi]] != "dotblock"
  lines = (bdf$start[bi]+1):(bdf$end[bi]-has.footer)
  ps$txt[lines]

}

source.extra.code.file = function(extra.code.file, ps) {
  restore.point("source.extra.code.file")
  # Source extra.code
  ps$extra.code.file = extra.code.file
  if (!is.null(extra.code.file)) {
    for (file in extra.code.file)
      source(extra.code.file, local = ps$init.env)
  }
}

# data frame that contains chunk info used
# for rmarkdown based problem sets only
create.ps.rmc = function(ps) {
  restore.point("create.ps.rmc")
  
  bi = which(ps$bdf$stype == "task_chunk")
  task.ind = match(bi,ps$task.table$bi)
  
  shown.code = sapply(ps$bdf$shown.rmd[bi], function(code) {
    txt = sep.lines(code)
    merge.lines(txt[-c(1,length(txt))])
  })
  
  rmc = data_frame(chunk.ind = seq_along(task.ind), bi, task.ind, task.line = ps$task.table$task.line[task.ind], chunk.name = ps$bdf$name[bi], shown.code = shown.code, all.required=vector("list", length(task.ind)))
  ps$rmc = rmc
  
  for (r in seq_len(NROW(rmc))[-1]) {
    ps$rmc$all.required[r] = list(compute.all.required.chunks(r,ps,use.prev.req=TRUE))
  }
}

compute.all.required.chunks = function(chunk.ind, ps, use.prev.req=FALSE) {
  restore.point("compute.all.required.chunks")
  if (!use.prev.req) stop("Only implemented with use.prev.req")
  if (chunk.ind == 1) return(NULL)
  
  rmc = ps$rmc
  task.ind = rmc$task.ind[chunk.ind]
  lines = c(rmc$task.line[chunk.ind],ps$task.table$task.in[[task.ind]] )
  
  all.req = unique(unlist(lapply(lines, function(line) {
    chunks = which(rmc$task.line == line & rmc$chunk.ind < chunk.ind)
    if (length(chunks)==0) return(NULL)
    chunk = max(chunks)
    return(c(chunk,rmc$all.required[[chunk]]))
  })))
  if (!is.null(all.req)) all.req = sort(all.req)
  
  all.req
}