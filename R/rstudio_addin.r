previewRTutorFrameAddin = function(...) {
  preview.rtutor.frames.addin(single.frame=TRUE)
}

previewRTutorAllFramesAddin = function(...) {
  preview.rtutor.frames.addin(single.frame=FALSE)
}


preview.rtutor.frames.addin = function(single.frame=TRUE,...) {
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
  
  if (single.frame) {
    ps = rtutor.make.frame.ps.te(txt, bdf.filter=bdf.frame.filter(line=line))
    frame.ind = 1
  } else {
    ps = rtutor.make.frame.ps.te(txt)
    frame.ind = line.to.type.ind(line, type="frame", bdf=ps$bdf)
  }
  bdf = ps$bdf
  show.frame.ps(ps,frame.ind = frame.ind)
} 
