
default.ps.opts = function(
  ps.type = "shiny",
  
  rmd.modes = c("rmd","tex","shown","sol"),
  plugins = c("stats","export","dataexplorer"),
  # slides
  slides = identical(ps.type,"slides"),
  slide.type = "frame",
  # menu
  show.together = "section",
  menu.levels = c("section", "subsection"),
  toc.levels = c("section", "subsection"),
  number.levels = NULL,
  
  menu.placement = c("top","fixed"),

  stats.aggregate.by = "section",
  # task.env
  task.env.together = show.together,
  # which parts create a new comp.line
  new.task.line.parts = c("exercise","section","ps"),
  nested.task.line.parts = c("note"),
  
  # which part types shall have static html
  static.types = c("section", "subsection","subsubsection","frame"),
  nav.levels = c("section","subsection","frame"),  
  is.shiny = TRUE,
  catch.errors = TRUE,
  # parameters related to chunk points
  e.points = 1,
  chunk.preknit = FALSE,
  chunk.precomp = FALSE,
  chunk.min.points=0,
  chunk.points=0,      
  show.points = TRUE,
  # relevant for shiny_chunk
  show.line.numbers = TRUE,
  check.whitelist = FALSE,
  use.secure.eval = FALSE,
  
  noeval = FALSE, # will task_chunks not be evaluated
  preknit = FALSE,
  presolve = FALSE, # shall tasks be solved at compile time 
  
  replace.sol = FALSE,
  show.solution.btn=FALSE,
  show.data.exp=FALSE,
  show.save.btn=FALSE,
  in.R.console=FALSE,
  # Turn off graphics when checking chunk
  use.null.device = TRUE,
  verbose = FALSE,
  # chunk tasks and other tasks
  save.task.times = TRUE,
  save.chunk.user.code = !replace.sol,

  chunk.out.args = default.chunk.out.args(),
  knit.print.params = default.knit.print.params(),
  
  use.memoise = FALSE,
  memoise.funs = rtutor.default.memoise.funs(),
  add.enter.code.here = isTRUE(ps.type == "rmd"),
  
  hide_title = if (slides) c("section","subsection") else NULL, 
  block.packages = c("RTutor3","armd"),  
  name = "ps",
  id = "ps",
  
  use.clicker = FALSE,
  clicker.stop.in = 5,
  ...
) {
  args = c(as.list(environment()),list(...))
  args
}

default.knit.print.params = function(html.data.frame=TRUE,table.max.rows=40, round.digits=8, signif.digits=8) {
  as.list(environment())  
}

set.rt.opts = function(opts) {
  options(.RTUTOR.OPTS=opts)
}

# Default problem set options
rt.opts = function() {
  getOption(".RTUTOR.OPTS")
}
