write.ps.rmd = function(ps) {
  write.sol.rmd(ps)
  write.out.rmd(ps)
  write.shown.rmd(ps)
} 

# 
write.sol.rmd = function(ps, file = paste0(ps$ps.name,"_sample_solution.Rmd")) {
  restore.point("write.sol.rmd")
  rmd = ps$rmd$sol  
  header = ps.sample.sol.header(ps)
  rmd = c(header, rmd)
  rmd = mark_utf8(rmd)
  writeLines(rmd, file, useBytes=TRUE)
}


write.out.rmd = function(ps, file = paste0(ps$ps.name,"_output_solution.Rmd")) {
  restore.point("write.output.solution")
  rmd = ps$rmd$rmd  
  header = out.rmd.header(ps)
  rmd = c(header, rmd)
  rmd = mark_utf8(rmd)
  writeLines(rmd, file, useBytes=TRUE)
}


write.shown.rmd = function(ps, file = paste0(ps$ps.name,"_problemset.Rmd")) {
  restore.point("write.shown.rmd")
  rmd = ps$rmd$shown  
  header = ps.rmd.header(ps)
  rmd = c(header, rmd)
  rmd = mark_utf8(rmd)
  writeLines(rmd, file, useBytes=TRUE)
}



ps.rmd.header = function(ps, opts=ps$opts) {

"This is an interactive RTutor problem set. You can check your solution by running the RStudio addin 'Check Problemset'.\n"

paste0("Problemset: ", ps$ps.name,"
Username: ENTER YOUR USERNAME HERE
")  

  
}

ps.sample.sol.header = function(ps, opts=ps$opts) {

"This is an interactive RTutor problem set. You can check your solution by running the RStudio addin 'Check Problemset'.\n"

paste0("Problemset: ", ps$ps.name,"
Username: Jane Doe
")  

  
}




out.rmd.header = function(ps, opts = ps$opts) {
  libs = paste0("library(", c(ps$opts$libs,"RTutor"),")", collapse="\n")  
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


code.to.rmd.chunk = function(code, args, label=args$label) {
  restore.point("code.to.rmd.chunk")
  
  if (is.null(label)) {
    label = "chunk"
  }
  args = args[setdiff(names(args),"label")]
  args = lapply(args, function(arg) {
    if (is.character(arg)) return(paste0("'",arg,"'"))
    arg
  })
  if (length(args)>0) {
    head = paste0('```{r "',label, '", ',paste0(names(args)," = ", args, collapse=", "),'}')
  } else {
    head = paste0('```{r "',label, '" }')
  }
  c(head,code,"```")
}



remove.verbatim.end.chunks = function(chunk.start, chunk.end) {
  restore.point("remove.verbatim.end.chunks")
  df = data.frame(ind =c(0, seq_along(chunk.start), seq_along(chunk.end)),
                  row=c(0, chunk.start,chunk.end),
                  type=c("f",
                         rep("s",length(chunk.start)),
                         rep("e",length(chunk.end))
                       )
                  )
  df = arrange(df,row)
  df$del =  df$type == "e" & !is.true(lag(df$type) == "s")

  keep.ind = df$ind[df$type=="e" & !df$del]
  chunk.end[keep.ind]
}

