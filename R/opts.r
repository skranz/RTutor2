
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
