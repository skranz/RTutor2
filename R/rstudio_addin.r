checkProblemSet2Addin = function(...) {
  library(RTutor2)
  doc = rstudioapi::getActiveDocumentContext()
  restore.point("preview.rtutor.part.addin")
  cat("\nView frame")
  
  file = basename(doc$path)
  dir = dirname(doc$path)
  
  if (nchar(file)==0) {
    cat("\nRStudio has not detected your RTutor .Rmd tab. Please try again!")
    return()
  }
  
  ext = tools::file_ext(file)
  if (tolower(ext) != "rmd") {
    cat("\nRStudio has not detected your RTutor .Rmd tab. The current tab is on the file", file," instead. Please try again!")
    return()
  }
  name = tools::file_path_sans_ext(file)
  rps.files = c(name)
  if (str.ends.with(name,"_sample_solution")) {
    rps.files = c(rps.files, str.left.of(name,"_sample_solution"))
  }
  if (str.ends.with(name,"_problemset")) {
    rps.files = c(rps.files, str.left.of(name,"_problemset"))
  }
  long.rps.files = paste0(dir,"/",rps.files, ".rps")
  rps.file = NULL
  for (f in rps.file) {
    if (file.exists(f)) {
      rps.file = f
      break
    }
  }
  if (is.null(rps.file)) {
    cat(paste0("\nI could not find the file ", rps.files[length(rps.files)], ".rps, which is needed to check your problem set ", file, ". Also make sure that you have not renamed your .rmd file."))
    return()
  }
  
}

showRTutorPartAddin = function(...) {
  preview.rtutor.part.addin(single.part=TRUE)
}

showRTutorAddin = function(...) {
  preview.rtutor.part.addin(single.part=FALSE)
}


createRTutorOfflineSlides = function(...) {
  doc = rstudioapi::getActiveDocumentContext()

  file = basename(doc$path)
  
  if (nchar(file)==0) {
    cat("\nRStudio has not detected your RTutor .rmd tab. Please try again!")
    return()
  }
  dir = dirname(doc$path)
  setwd(dir)
  create.offline.slides(file)
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
  
  setwd(dir)
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
