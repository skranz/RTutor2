examples.translate.old.sol = function() {
  setwd("D:/libraries/RTutor/examples")
  in.file = "The Impact of Shrouded Fees 3_sol.Rmd"
  out.file = "The Impact of Shrouded Fees - 3_sol.Rmd"
  
  in.file = "Problem_Set_Sol.Rmd"
  out.file = "Bank Runs 2_sol.Rmd"
  
  setwd("D:/libraries/RTutor/examples")
  in.file = "intro_sol.Rmd"
  out.file = "intro2_sol.Rmd"
  
  translate.old.rtutor.sol(in.file=in.file, out.file=out.file)
}

translate.old.rtutor.sol = function(txt = readLines(in.file), in.file=NULL, out.file=rmd.file) {
  
  txt = gsub("#< task_notest","#< show_notest",txt)
  txt = gsub("#< task","#< show",txt)

  if (!is.null(out.file)) {
    writeLines(txt, out.file)
  }
  invisible(txt)
}