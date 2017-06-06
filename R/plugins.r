# A plugin is something that can be shown on the RHS toolbar


sidebar.ui = function(ps) {
  restore.point("sidebar.ui")
  plugins = ps$plugins
  Plugins = getPlugins()
  menu.labels = lapply(Plugins, function(x) x$menu.label)
  
  
  ns = nestedSelector(id="plugin_sel",btn.size = "xs", selectors = list(
    p = list(
      choices = named.list(plugins, menu.labels),
      contents = paste0(plugins,"_plugin_div")
    )
  ))
  divs = lapply(seq_along(Plugins), function(i) {
    hidden_div(id=paste0(plugins[i],"_plugin_div"),Plugins[[i]]$sidebar.ui(ps=ps))
  })
  tagList(
    div(style="background-color: #eeeeee; width: 100%; border-bottom: solid #999999 1px;",ns$ui),
    div(style="padding-left: 2px; padding-right: 2px;",
      divs
    )
  )
}

sidebar.ui.handlers = function(...) {
  
  nestedSelectorHandler("plugin_sel", function(value,..., ps=get.ps()) {
    #args = list(...)
    restore.point("plugin_selHandler")
    plugin = value[[1]]
    ps$active.plugin = plugin
    call.plugin.handler("activate.handler",plugin=plugin)
  })
  
  
}

get.plugin.state = function(plugin=ps$active.plugin, ps=get.ps()){
  ps$plugin.states[[plugin]]
}


set.plugin.state = function(plugin=ps$active.plugin, pis=NULL, ps=get.ps()){
  ps$plugin.states[[plugin]] = pis
}


activePlugin = function(plugin=ps$active.plugin, ps=get.ps(),...) {
  getPlugin(plugin)
}

getPlugins = function(plugins=ps$plugins, ps=get.ps(),...) {
  li = lapply(plugins, getPlugin)
  names(li) = plugins
  li
}

getPlugin = function(plugin,...) {
  if (is.null(plugin)) return(NULL)
  fun = paste0("rtutor.plugin.",plugin)
  do.call(fun,list())
}
  
call.plugin.handler = function(handler,plugin=ps$active.plugin,..., args=list(), Plugin = getPlugin(plugin), ps=get.ps(), ups=get.ups()) {
  restore.point("call.plugin.handler")
  
  if (is.null(Plugin)) return()
  args = c(args,list(...))
  if (!is.null(Plugin[[handler]]))
    do.call(Plugin[[handler]],args)  
}

rtutor.plugin.stats = function() {
  list(
    name = "stats",
    menu.label = "Stats",
    sidebar.ui = function(...) {uiOutput("uiProblemSetStats")},
    activate.handler = function(...) {rtutor.update.stats.panel()},
    task.select.handler = NULL,
    task.checked.handler = function(ts,...) {
      restore.point("task.checked.handler")
      if (isTRUE(ts$solved)) rtutor.update.stats.panel()
    },
    task.hint.handler = function(...) {rtutor.update.stats.panel()}
  )
}

rtutor.plugin.export = function() {
  list(
    name = "export",
    menu.label = "Export",
    sidebar.ui = function(...) {export.ui()},
    activate.handler = function(...) {make.export.handlers()}
  )
}

rtutor.plugin.dataexplorer = function() {
  list(
    name = "dataexplorer",
    menu.label = "Data",
    sidebar.ui = function(...) {data.explorer.ui()},
    activate.handler = function(...) {
      data.explorer.init.handlers()
      update.data.explorer.ui()
    },
    task.select.handler = function(...) {update.data.explorer.ui()}
  )
}