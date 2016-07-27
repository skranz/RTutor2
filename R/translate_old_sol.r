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

is.old.rtutor.sol = function(txt = readLines(file), file) {
  old = grepl("#< task",txt) | grepl("#! start_note",txt) | grepl("## Exercise",txt)
  new = grepl("#< show",txt) | grepl("#< note",txt) | grepl("#. section",txt)
  sum(old) > sum(new) | sum(old) > sum(grepl("## Exercise",txt))
}

translate.old.rtutor.sol = function(txt = readLines(in.file), in.file=NULL, out.file=in.file, backup.in.file = if (is.null(in.file)) paste0(in.file,".old") else NULL) {
  
  if (!is.null(backup.in.file) & !is.null(in.file)) {
    cat("\nCreated backup of orginal file: ", backup.in.file)
    file.copy(from=in.file, to=backup.in.file)
  }
  
  txt = gsub("#< task_notest","#< show_notest",txt)
  txt = gsub("#< task","#< show",txt)
  txt = gsub("#! start_note","#< note",txt)
  txt = gsub("#! end_note","#> end note",txt)
  txt = gsub("## Exercise ","#. section ",txt)

  
  
  if (!is.null(out.file)) {
    cat("\nWrote new RTutor format to: ", backup.in.file)
    writeLines(txt, out.file)
  }
  invisible(txt)
}