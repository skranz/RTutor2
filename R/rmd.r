write.ps.rmd = function(ps) {
  write.sol.rmd(ps)
  write.out.rmd(ps)
  write.shown.rmd(ps)
} 

# 
write.sol.rmd = function(ps, file = paste0(ps$ps.name,"_sample_solution.Rmd")) {
  restore.point("write.sol.rmd")
  rmd = ps$bdf$sol.rmd[1]  
  header = ps.rmd.header(ps)
  rmd = c(header, rmd)
  rmd = mark_utf8(rmd)
  writeLines(rmd, file, useBytes=TRUE)
}


write.out.rmd = function(ps, file = paste0(ps$ps.name,"_output_solution.Rmd")) {
  restore.point("write.output.solution")

  rmd = ps$bdf$out.rmd[1]
  header = out.rmd.header(ps)
  rmd = c(header, rmd)
  rmd = mark_utf8(rmd)
  writeLines(rmd, file, useBytes=TRUE)
}


write.shown.rmd = function(ps, file = paste0(ps$ps.name,"_problemset.Rmd")) {
  restore.point("write.sol.rmd")
  rmd = ps$bdf$shown.rmd[1]
  header = ps.rmd.header(ps)
  rmd = c(header, rmd)
  rmd = mark_utf8(rmd)
  writeLines(rmd, file, useBytes=TRUE)
}



ps.rmd.header = function(ps, opts=rt.opts()) {

"This is an interactive RTutor problem set. You can check your solution by running the RStudio addin 'Check Problemset'.\n"

paste0("Problemset: ", ps$ps.name,"
Username: Enter Your Username Here
")  

  
}



out.rmd.header = function(ps, opts = rt.opts()) {
  libs = paste0("library(", c(ps$libs,"RTutor"),")", collapse="\n")  
  source.txt = if (!is.null(ps$extra.code.file)) paste0('\nsource("',ps$extra.code.file,'")', collapse="") else ""
  
  knit.params = opts$knit.print.params
  knit.params$html.data.frame = FALSE
  knit.opts =  paste0(names(knit.params), " = ", knit.params, collapse=", ")
  
  header = paste0(
'
---
title: Problem Set ', ps$ps.name,'
output: 
  html_document: 
    keep_md: yes
    toc: yes
---

```{r setup, include=FALSE, echo=FALSE}
# Load libraries and source extra code
',libs,source.txt,'

# render data frames similar to the RTutor browser.
# for word or latex output, we set html.data.frame = FALSE

RTutor2::set.knit.print.opts(',knit.opts,')

# continue knitting even if there is an error
knitr::opts_chunk$set(error = TRUE) 
```
'
)
  header  
}
