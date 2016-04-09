
stats = function(do.display=TRUE(),ups = get.ups(), ps=get.ps()) {
  if (is.null(ps)) {
    display("No problem set specified. You must check a problem before you can see your stats.")
    return(invisible())
  }
  sr = compute.stats()
  if (do.display) {
    display(ups$user.name, "'s stats for problem set ",ps$ps.name,":\n")
    print(as.data.frame(sr))
    return(invisible(sr))
  }

}

first.toupper = function(txt) {
  paste0(toupper(substring(txt,1,1)), substring(txt,2))
}

#' Shows your progress
#' @export
compute.stats = function(by=opts$stats.aggregate.by,ups=get.ups(), ps=get.ps(), opts=rt.opts()) {
  restore.point("compute.stats")

  by = "section"
  tas = cbind(ups$utt, ps$task.table)
  parent.bi = unlist(ps$bdf[tas$bi,paste0("parent_",by)])
  parent.ind = ps$bdf$stype.ind[parent.bi]
  
  parent.name = paste0(first.toupper(by)," ", parent.ind)
  tas = cbind(tas,parent.bi, parent.name)

  pa = group_by(tas, parent.name) %>%
    summarise(
      points = sum(points),
      max.points = sum(max.points),
      percentage = round(points/max.points*100),
      hints = sum(num.hints)
    )
  
  all = tas %>%
    summarise(
      parent.name = "Total",
      points = sum(points),
      max.points = sum(max.points),
      percentage = round(points/max.points*100),
      hints = sum(num.hints)
    )
  res = rbind(pa, all)
  sr = dplyr::select(res,parent.name,percentage, points, max.points, hints)
  colnames(sr) = c("Part","Solved (%)","Points", "Max. Points", "Hints")
  rownames(sr) = NULL


  sr
}


rtutor.update.stats.panel = function(app = getApp(),ps=get.ps(),ups=get.ups(),...) {
  restore.point("rtutor.update.stats.panel")
  
  if (!ps$has.tasks) {
    dsetUI("uiProblemSetStats", p("The problem set has no tasks."))
    return()
  }
  
  df = compute.stats(ups=ups)
  perc = df[NROW(df),2]
  html = html.table(df)
  html = paste0("<h4>You solved ", perc, "% ...</h3><br>\n", html)
  cat(paste0("
*******************************************************
    rtutor.update.stats.panel ", as.numeric(perc)," % 
*******************************************************"))

  # use dsetUI since the panel may be hidden
  dsetUI("uiProblemSetStats", HTML(html))
} 

