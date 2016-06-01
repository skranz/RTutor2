#' Make a problem set app suited for hosting RTutor in the web
#' 
#' The user first opens the login app, which creates a session file
#' and then calls this app.
clickerApp = function(dir=getwd(),obj.dir=paste0(dir,"/objs"), res.dir=paste0(dir,"/res"),opts.dir=paste0(dir,"/opts"), check.obj.millis=1000,  ...) {

  cat("\nInitialize problem set, this may take a while...")
  app = eventsApp()

  ca = new.env()
  app$ca = ca
  
  return(app)


  #runEventsApp(app=app,ui=ui,launch.browser=launch.browser, quiet=FALSE)

}

get.ca = function(app=getApp()) {
  app$ca
}

init.ps.clicker = function(ps, opts) {
  restore.point("init.ps.clicker")
  
  pc = list(
    ses.id = ps$ses.id,
    ses.start = Sys.time(),
    ps.name = ps$ps.name,
    opts=opts
  )
  cses.dir = paste0(opts$clicker.dir,"/ses")
  saveRDS(pc, file = paste0(cses.dir,"/",pc$ses.id,".pc"))
  
}
