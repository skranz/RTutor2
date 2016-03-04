showRTutorPartAddin = function(...) {
  preview.rtutor.frames.addin(single.part=TRUE)
}

showRTutorAddin = function(...) {
  preview.rtutor.frames.addin(single.part=FALSE)
}


preview.rtutor.part.addin = function(single.part=TRUE,...) {
  library(rstudioapi)
  library(RTutor2)
  doc = rstudioapi::getActiveDocumentContext()
  restore.point("preview.rtutor.frames.addin")
  cat("\nView frame")
  
  file = basename(doc$path)
  dir = dirname(doc$path)
  
  txt = doc$contents
  range = doc$selection[[1]]$range
  line = range$start[1]
  
  sub.txt = txt[1:line]
  if (!any(str.starts.with(sub.txt,"#. frame"))) {
    cat("\nSorry,... I don't see a tag '#. frame' above your cursor in your .Rmd file.")
    return()
  }
  
  if (single.part) {
    ps = rtutor.make.frame.ps(txt, bdf.filter=bdf.part.filter(line=line))
    start.slide = 1
  } else {
    ps = rtutor.make.frame.ps(txt)
  }
  if (ps$slides) {
    
  }
  bdf = ps$bdf
  show.frame.ps(ps,frame.ind = frame.ind)
} 
