example.convert = function() {
  setwd("D:/libraries/RTutor2/examples/auction")
  source.file = "auction_old_sol.Rmd"
  dest.file = "auction_new_sol.Rmd"
  convert.sol.file(source.file, dest.file)
  
}

convert.sol.file= function(source.file, dest.file) {
  txt = readLines(source.file)
  new = convert.sol.rmd(txt)
  writeLines(new,dest.file)
}

convert.sol.rmd = function(txt) {
  restore.point("converst.sol.rmd")
  
  subst = rbind(
    c("#< task", "#< show"),
    c("## Exercise", "#. section")
  )
  
  for (r in 1:NROW(subst)) {
    txt = gsub(subst[r,1],subst[r,2],txt, fixed=TRUE)
  }
  
  rows = str.starts.with(txt,"#. section ")
  arg.str = str.right.of(txt[rows],"#. section ")
  arg.str = quote.single.arg(arg.str)
  txt[rows] = paste0("#. section ", arg.str)
  txt
}

quote.single.arg = function(arg.str) {
  restore.point("quote.arg")
  
  arg.str = str.trim(arg.str)
  first = substring(arg.str,1,1)
  is.quoted = first == "'" | first == '"'
  has.arg = nchar(arg.str) >0
  
  rows = !is.quoted & has.arg
  if (sum(rows)>0)
    arg.str[rows] = paste0('"',arg.str[rows],'"')
  arg.str
}
