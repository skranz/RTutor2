
previewRTutorFrameAddin = function(...) {
  library(rstudioapi)
  library(RTutor2)
  doc = rstudioapi::getActiveDocumentContext()
  restore.point("previewRTutorFrameAddin")
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
  
  te = rtutor.make.frame.ps.te(txt, bdf.filter=bdf.frame.filter(line=line))
  bdf = te$bdf
  ui = make.te.ui(te=te)
  cat("\nView frame...")
  view.html(ui=ui)

}
