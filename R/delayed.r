shinyDelayedRun = function(millis=5000, fun,..., app=getApp()) {
  restore.point("shinyDelayedRun")
  
  if (is.null(app$delayed.ids))
    app$delayed.ids = list()
  
  id = length(app$delayed.ids) + 1
  app$delayed.ids[[id]] = 0
  observe({
    app=getApp()
    times = app$delayed.ids[[id]]
    cat("\n delayed run times = ", times)
    if (times==0) {
      app$delayed.ids[[id]] = app$delayed.ids[[id]]+1
      invalidateLater(millis)
    } else if (times==1) {
      fun(...)
    }
  })
}