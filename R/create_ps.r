examples.create.ps = function() {
  restore.point.options(display.restore.point = TRUE)

  setwd("D:/libraries/RTutor2/")
  file = "ex1.Rmd"
  ps = create.ps(file=file)
  app = rtutorApp(ps)
  viewApp(app,launch.browser = rstudioapi::viewer)

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
  ps = create.ps(txt,catch.errors = FALSE, priority.opts=popts, ps.type="rmd")

  write.ps.rmd(ps)

  bdf = ps$bdf
  rmc = ps$rmc

  restore.point.options(display.restore.point = !TRUE)

  app = rtutorApp(ps)
  viewApp(app,launch.browser = TRUE)

}


create.ps = function(...) {
  library(armd)
  am = parse.armd(..., rtutor=TRUE)
  restore.point("create.ps")

  ps = armd.to.ps(am)
}


#' Generate a problem set from a solution file
#'
#' Generates  .ps file, and .rmd files for empty ps , sample solution, and output solution
#'
#' @param txt the content of the sol.rmd file as text lines
#' @param file filename of the _sol.rmd file that specifies the problem set
#' @param ps.name the name of the problem set
#' @param dir the directory in which all files are found and wil be saved to
#' @param extra.code.file the name of an r file that contains own functions that will be accessible in the problme set
#' @param var.txt.file name of the file that contains variable descriptions (see thee vignette for an explanation of the file format)
#' @param rps.has.sol shall the sample solution be stored in the .rps file. Set this option to FALSE if you use problem sets in courses and don't want to assess students the sample solution easily
#' @param use.memoise shall functions like read.csv be memoised? Data sets then only have to be loaded once. This can make problem sets run faster. Debugging may be more complicated, however.
#' @export
armd.to.ps = function(am,dir=getwd(), figure.dir=paste0(dir,"/",figure.sub.dir), figure.sub.dir = "figure", figure.web.dir = "figure", filter.line=NULL, filter.type="auto", check.old.rtutor.sol=TRUE, plugins=am$opts$plugins, write.ps.rmd=TRUE, copy.into.global.env=TRUE, ...) {
  restore.point("armd.to.ps")
  library(armd)

  ps = am
  set.rt.opts(ps$opts)
  set.ps(ps)

  ps$is.initialized=FALSE
  ps$version = 0.1
  ps$figure.web.dir = figure.web.dir
  ps$figure.sub.dir = figure.sub.dir
  if (!dir.exists(figure.dir)) {
    dir.create(figure.dir)
  }
  ps$ps.name = am$name
  ps$ps.id = am$am.id
  ps$dir = dir
  ps$figure.dir = figure.dir
  ps$plugins = plugins

  ps$bdf$task.ind = cumsum(ps$bdf$is.task) * ps$bdf$is.task
  # store task.chunks in a list ck.li and the
  # corresponding user chunks in uk
  create.ps.tasks(ps=ps)
  create.ps.rmc(ps=ps)

  ps$navbar.ui = rtutor.navbar(ps=ps, nav.levels = ps$opts$nav.levels)

  ps$ui = make.rtutor.ui(ps=ps)

  remove.existing.ups(ps.name=ps$ps.name, dir=dir)
  ps$rps.time.stamp = Sys.time()
  write.rps(ps=ps,dir=dir)
  if (write.ps.rmd)
    write.ps.rmd(ps)

  
  # copy pre.env and env into global env for convenience
  if (copy.into.global.env) {
    copy.into.env(ps$pre.env,dest = .GlobalEnv)
    copy.into.env(ps$env,dest = .GlobalEnv)
  }

  ps
}

write.rps = function(ps, file.name=paste0(dir,"/",ps$ps.name,".rps"), dir=getwd()) {
  restore.point("write.rps")

  suppressWarnings(saveRDS(ps, file.name))
}

read.rps = function(file.name=paste0(dir,"/",ps.name,".rps"), dir=getwd(), ps.name="") {
  readRDS(file.name)
}


rtutor.preparse.block = function(bi,ps, opts = ps$opts) {
  restore.point("rtutor.preparse.block")
  type = ps$bdf$type[[bi]]

  if (type %in% c(opts$new.task.line.parts,opts$nested.task.line.parts)) {
    set.task.line(bi=bi,ps=ps, opts=opts)
  }
}

rtutor.parse.chunk = function(bi,ps, opts=ps$opts) {
  restore.point("rtutor.parse.chunk")
  bdf = ps$bdf; br = bdf[bi,]; str = ps$txt[br$start:br$end]
  args = parse.chunk.args(header = str[1])

  if (!is.null(args$label))
    ps$bdf$name[bi] = args$label

  chunk.precompute = br$parent_precompute >0 | isTRUE(args$precompute) | opts$chunk.preknit

  chunk.preknit = isTRUE(args$preknit) | br$parent_info | br$parent_preknit | opts$chunk.preknit
  chunk.task = isTRUE(args$task_chunk) | (!chunk.preknit & !chunk.precompute)

  code = str[-c(1,length(str))]
  mstr = merge.lines(str)
  if (chunk.precompute) {
    restore.point("parse.precompute.chunk")
    ps$bdf$stype[[bi]] = "precompute_chunk"
    expr = parse(text=code)
    res = eval(expr,ps$pre.env)
    ps$bdf$obj[[bi]]$pre.env = copy.env(ps$pre.env)
    armd.set.rmd(bi=bi, am=ps, rmd=mstr)
  }
  if (chunk.preknit) {
    restore.point("parse.preknit.chunk")
    ps$bdf$stype[[bi]] = "preknit_chunk"
    args$comment = NA
    rmd = code.to.rmd.chunk(code,args=args)
    ui = knit.rmd(rmd,envir = ps$pre.env,out.type="shiny")
    #ui = tagList(ui, highlight.code.script())
    set.bdf.ui(ui, bi,ps)
    armd.set.rmd(bi=bi, am=ps, rmd=mstr)
  }
  if (!chunk.task) {
    ps$bdf$is.widget[[bi]]= FALSE
    ps$bdf$is.task[[bi]] = FALSE
  }
  if (chunk.task) {
    ps$bdf$stype[[bi]] = "task_chunk"
    # a task chunk is the classic RTutor chunk
    rtutor.parse.task.chunk(bi=bi,ps=ps,args=args)
  }
}

armd.parse.solved = function(bi,ps) {
  restore.point("rtutor.parse.solved")
  rtutor.parse.as.container(bi,ps)
}

# data frame that contains chunk info used
# for rmarkdown based problem sets only
create.ps.rmc = function(ps) {
  restore.point("create.ps.rmc")

  bi = which(ps$bdf$stype == "task_chunk")
  if (length(bi)==0) return(NULL)

  task.ind = match(bi,ps$task.table$bi)

  shown.code = sapply(ps$bdf$rmd[bi], USE.NAMES=FALSE, function(rmd) {
    restore.point("nhdfdf")
    txt = sep.lines(rmd$shown)
    merge.lines(txt[-c(1,length(txt))])
  })


  rmc = fast_df(chunk.ind = seq_along(task.ind), bi=bi, task.ind=task.ind, task.line = ps$task.table$task.line[task.ind], chunk.name = ps$bdf$name[bi], shown.code = shown.code, all.required=vector("list", length(task.ind)))
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

get.chunk.lines = function(txt) {
  restore.point("get.chunk.lines")
  chunk.start = str.starts.with(txt,"```{")
  chunk.end = which(str.starts.with(txt,"```") & !chunk.start)
  chunk.start = which(chunk.start)
  chunk.end = remove.verbatim.end.chunks(chunk.start,chunk.end)

  header = txt[chunk.start]
  chunk.name = sapply(header,USE.NAMES=FALSE, function(str) chunk.opt.string.to.list(str, keep.name=TRUE)[[1]])

  quick.df(chunk.name=chunk.name, start.line=chunk.start, end.line=chunk.end)
}


#' Set default names for the chunks of problem set rmd files
#' @param rmd.file file name
#' @param txt alternative the code as txt file
#' @param only.empy.chunks if FALSE (default) name all chunks.
#'        Otherwise only empty chunks are overwritten
#' @param keep.option if TRUE (default) don't change chunk options;
#'        otherwise clear all chunk options (dangerous)
#'
name.rmd.chunks = function(rmd.file=NULL, txt=readLines(rmd.file), only.empty.chunks=FALSE, keep.options=TRUE, valid.file.name = FALSE) {
  restore.point("name.rmd.chunks")
  ex.name = ""
  part.name = ""
  in.code = FALSE
  i = 2
  counter = 1

  used.chunk.names = NULL

  str = "```{r 'out_chunk_2_1_b', fig.width=5, fig.height=5, eval=FALSE, echo=TRUE}"
  for (i in 1:length(txt)) {
    str = txt[i]


    if (str.starts.with(str, "```{r")) {
      if ((!only.empty.chunks) | str.trim(str) == "```{r }" | str.trim(str) == "```{r}") {
        counter.str = ifelse(counter==1,"", paste0(" ",counter))

        # preserve chunk options
        if (has.substr(str,"=")) {
          rhs.str = paste0(",",chunk.opt.list.to.string(chunk.opt.string.to.list(str)))
        } else {
          rhs.str = ""
        }
        chunk.name = paste0(ex.name,' ',part.name, counter.str)

        chunk.name = str.to.valid.chunk.name(str.trim(chunk.name))

        if (chunk.name %in% used.chunk.names) {
          str = paste0("I generated the chunk name ", chunk.name, " twice. Make sure that you have unique exercise names and don't duplicate exerice parts like a) b) a).")
          warning(str)
          chunk.name = paste0(chunk.name, "___", sample.int(10000000,1))
        }
        used.chunk.names = c(used.chunk.names, chunk.name)

        txt[i] = paste0('```{r "',chunk.name,'"', rhs.str,"}")
      }
      counter = counter+1
    } else if (str.starts.with(str,"## Exercise ")) {
      ex.name = str.right.of(str,"## Exercise ")
      ex.name = gsub("#","", ex.name, fixed=TRUE)
      ex.name = str.left.of(ex.name," --", not.found="all")
      if (!valid.file.name)
        counter = 1
      part.name = ""
    } else if (!is.na(temp.part <- str_extract(str,"^([a-z]|[ivx]*)\\)")[1]  )) {
      part.name = gsub(")","",temp.part, fixed=TRUE)
      if (!valid.file.name)
        counter = 1
    }
  }
  if (!is.null(rmd.file))
    writeLines(txt, rmd.file)
  invisible(txt)
}
