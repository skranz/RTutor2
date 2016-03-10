
default.ps.opts = function(
  # menu
  show.together = "section",
  menu.levels = c("section", "subsection"),
  menu.placement = c("top","fixed"),
  # which part types shall have static html
  static.types = c("section", "subsection","subsubsection","frame"),
  nav.levels = c("section","subsection","frame"),  
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
  verbose = FALSE,
  ...
) {
  args = c(as.list(environment()),list(...))
  args
}

set.rt.opts = function(opts) {
  options(.RTUTOR.OPTS=opts)
}

# Default problem set options
rt.opts = function() {
  getOption(".RTUTOR.OPTS")
}
