# A plugin is something that can be shown on the RHS toolbar


slides.plugins.ui = function(ps, opts=rt.opts()) {
  restore.point("slides.plugins.ui")
  
  # slide plugins are defined in opts
  sp.names = opts$slide.plugins
  if (length(sp.names)==0) return(NULL)
  

  sps = lapply(sp.names,get.slide.plugin, ps=ps)
  names(sps) = sp.names
  ps$slide.plugins = sps
  
  panels = lapply(sps, function(sp) {
    sp$panel
  })
  names(panels) = NULL
  ui = tagList(
    do.call(tabsetPanel, c(list(id="slidePluginTabsetPanel"), panels)),
    hr()
  )
  
  ui
}

init.slide.plugin.handlers = function(slide.plugins = ps$slide.plugins, ps,...) {
  for (sp in slide.plugins) {
    if (!is.null(sp$init.handlers)) {
      sp$init.handlers(ps=ps)
    }
  }
}

get.slide.plugin = function(name,ps=NULL,...) {
  if (is.null(name)) return(NULL)
  fun = paste0("rtutor.slide.plugin.",name)
  do.call(fun,list(ps=ps,...))
}
  