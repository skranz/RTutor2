showRTutorPartAddin = function(...) {
  preview.rtutor.part.addin(single.part=TRUE)
}

showRTutorAddin = function(...) {
  preview.rtutor.part.addin(single.part=FALSE)
}


preview.rtutor.part.addin = function(single.part=TRUE,...) {
  library(rstudioapi)
  library(RTutor2)
  doc = rstudioapi::getActiveDocumentContext()
  restore.point("preview.rtutor.part.addin")
  cat("\nView frame")
  
  file = basename(doc$path)
  dir = dirname(doc$path)
  
  if (nchar(file)==0) {
    cat("\nRStudio has not detected your RTutor .rmd tab. Please try again!")
    return()
  }
  
  txt = doc$contents
  range = doc$selection[[1]]$range
  line = range$start[1]
  
  
  show.line = filter.line = NULL
  if (single.part) {
    filter.line = line 
  } else {
    show.line = line
  }

  ps = rtutor.make.frame.ps(txt, dir=dir, source.file = file, show.line=show.line, filter.line = filter.line)
  app = rtutorApp(ps=ps, dir=dir)
  
  viewApp(app)
} 
