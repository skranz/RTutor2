rtutor.parse.toc = function(bi, ps, opts=ps$opts) {
  restore.point("rtutor.parse.toc")
  bdf = ps$bdf;
  
  toc.levels = opts$toc.levels
  n = length(toc.levels)
  level.counts = rep(0, n)
  ignore.levels = rep(FALSE,n)
  rows = which(bdf$type %in% toc.levels)
  html = rep("",NROW(bdf)) 
  old.level = 0  
  for (i in rows) {
    type = bdf$type[i]
    level = match(type, toc.levels)
    level.counts[level] = level.counts[level]+1
    if (level < n) {
      level.counts[(level+1):n] = 0
    }
    entry = make.toc.link(bi=i,ps=ps, level=level, level.counts=level.counts)
    
    # Don't add entry if section, subsection etc has no title
    if (is.null(entry)) {
      ignore.levels[level] = TRUE
    } else {
      ignore.levels[level:n] = FALSE
    }
    if (any(ignore.levels[1:level])) next
    ol = ""
    if (level > old.level) {
      ol = c(rep("<ol class='toc-ol'>", level-old.level))
    } else if (level < old.level){
      ol = c(rep("</ol>", old.level-level))
    }
    html[i] = paste0(c(ol, '<li class="toc-li">', entry,'</li>'),collapse="\n")
    old.level = level
  }
  html = paste0(c(html[rows],rep("</ol>",old.level)),collapse="\n")
  ps$bdf$ui[[bi]] = HTML(html)
  
}

make.toc.link = function(bi, ps, level, level.counts) {
  restore.point("make.toc.link")
  
  title = extract.part.title(bi,ps)
  if (nchar(title)==0) return(NULL)
  number = paste0(level.counts[1:level],collapse=".")
  res = paste0("<span class='toc-",level,"'>",number," ",title,"</span>")
  # local webpage link
  res = paste0("<a href='#part",bi,"'>",res,"</a>")
  res
  #title
}

extract.part.title = function(bi, ps) {
  arg.str= ps$bdf$arg.str[[bi]]
  args = parse.block.args(arg.str =arg.str, allow.unquoted.title = TRUE)
  if (str.starts.with(arg.str,"[")) {
    args$name = str.trim(str.right.of(args$name,']'))
  }
  title = first.non.null(args$title, args$name)
  title
}