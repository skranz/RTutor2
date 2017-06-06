guess.rtutor.file = function(txt = readLines(file)) {
  restore.point("guess.rtutor.file")
  
  num = sum(str.starts.with(txt,"#. section") | str.starts.with(txt, "#. frame"))
  if (num > 0) return("ps")
  num = sum(str.starts.with(txt,"#. rmd") | str.starts.with(txt, "#. readonly"))
  if (num > 0) return("rmdform")
  
  return("ps")
}

checkProblemSet3Addin = function(...) {
  doc = rstudioapi::getActiveDocumentContext()
  restore.point("checkProblemSet2Addin")
  cat("\nCheck Problemset")
  
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
  for (f in long.rps.files) {
    if (file.exists(f)) {
      rps.file = f
      break
    }
  }
  if (is.null(rps.file)) {
    cat(paste0("\nI could not find the file ", rps.files[length(rps.files)], ".rps, in the directory ", dir,", which is needed to check your problem set ", file, ". Also make sure that you have not renamed your .rmd file."))
    return()
  }
  
  ps.name = tools::file_path_sans_ext(basename(rps.file))
  
  # save file
  txt = doc$contents
  writeLines(txt, doc$path)
  
  
  res = try(RTutor3::check.problem.set(ps.name = ps.name,stud.short.file = file, stud.path = dir))
}
