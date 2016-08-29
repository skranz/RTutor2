parse.as.math.block = function(bi, ps, use.count = TRUE, add.end="",...) {
  restore.point("rtutor.parse.as.math.block")
  bdf = ps$bdf; br = bdf[bi,];
  type = br$type
  if (use.count) {
    count = sum(bdf$type[seq.int(bi)] == br$type)
    title = paste0(capitalize(br$type)," ", count,". ")
  } else {
    title = paste0(capitalize(br$type),". ")
     
  }
  if (type == "proof") {
    class = type
  } else {
    class = "mathblock"
  }
  txt = merge.lines(get.bi.inner.txt(bi=bi,ps=ps))
  inner.md = paste0('<span class="',class,'-title">',title,'</span>\n',txt, add.end)
  inner.html = fragment.to.html(sep.lines(inner.md),bi=bi,ps=ps)
  html = paste0('<div class="',class,'">', inner.html,"</div>")

  ps$bdf$ui[[bi]] = ps$bdf$inner.ui = HTML(html)
  ps$bdf$is.static[[bi]] = TRUE
}

rtutor.parse.proposition = function(...) parse.as.math.block(...)

rtutor.parse.theorem = function(...) parse.as.math.block(...)

rtutor.parse.lemma = function(...) parse.as.math.block(...)

rtutor.parse.proof = function(...)
  parse.as.math.block(..., use.count=FALSE, add.end = "&#9633;")

rtutor.parse.conjecture = function(...) parse.as.math.block(...)

rtutor.parse.assumption = function(...) parse.as.math.block(...)

rtutor.parse.fact = function(...) parse.as.math.block(...)

rtutor.parse.claim = function(...) parse.as.math.block(...)

rtutor.parse.remark = function(...) parse.as.math.block(...)

rtutor.parse.definition = function(...) parse.as.math.block(...)

capitalize = function(str) {
  paste0(toupper(substring(str,1,1)),substring(str,2))
  
}