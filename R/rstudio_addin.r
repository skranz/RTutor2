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
  
  txt = doc$contents
  range = doc$selection[[1]]$range
  line = range$start[1]
  
  sub.txt = txt[1:line]
  if (!any(str.starts.with(sub.txt,"#. frame"))) {
    cat("\nSorry,... I don't see a tag '#. frame' above your cursor in your .Rmd file.")
    return()
  }
  
  if (single.part) {
    ps = rtutor.make.frame.ps(txt, bdf.filter=bdf.part.filter(line=line),priority.opts = list(slides=TRUE, slide.type="auto"), dir=dir)
    app = slidesApp(ps,user.name="Jane Doe",catch.errors=FALSE,start.slide=1,dir=dir)
  } else {
    ps = rtutor.make.frame.ps(txt,dir=dir)
    app = rtutorApp(ps=ps, dir=dir)
  }
  viewApp(app)
} 
