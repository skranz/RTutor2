# nested layouts do not work yet
# code was quick and dirty programmed, can be improved
txt.to.layout = function(txt, layout, mode="default") {
  restore.point("txt.to.layout")
  
  if (is.character(layout))
    layout = get.ps.layout(layout)
  df = rmdtools:::split.text.in.startline.blocks(txt=txt, start.with = "##. ")
  if (df$end[1]==0) {
    df=df[-1,]
  } else {
    df$head[1] = "##. main"
  }

  parts = str.left.of(str.right.of(df$head,"##. "),",")

  # extract arguments for whiskers
  i = 1
  arg.str = str.right.of(df$head,",",not.found = NA)
  args = as.list(unlist(lapply(seq_along(parts), function(i) {
    if (is.na(arg.str[i])) return(NULL)
    args = unlist(strsplit(arg.str[i],"[,=]"))
    val.ind = seq(2,length(args),by=2)
    names = str.trim(args[val.ind-1])
    vals = args[val.ind]
    names(vals) = paste0(parts[i],".",names)
    vals
  })))
  
  defs = layout$whiskers[[mode]]
  defs[names(args)] = args
  
  li = lapply(seq_along(parts), function(i) {
    paste0(txt[(df$start[i]+1):df$end[i]], collapse="\n")
  })
  names(li) = parts
  lay = layout[[mode]]
  res = replace.whiskers(lay, c(li,defs), eval=FALSE,empty.val = "")
  #cat(res)
  res
}

parse.layout = function(txt, name) {
  li = parse.hashdot.yaml(txt, hashdot="##. ")
  li$name = name
}

load.default.layouts = function() {
  file = system.file("defaults/layouts.yaml",package="RTutor2")
  li = read.yaml(file)
  for (i in seq_along(li)) {
    li[[i]]$name = names(li)[i]
  }
  li
}



add.layout.to.ps = function(layout, ps=get.ps()) {
  if (is.null(ps[["layouts"]]))
    ps$layouts = list()
  ps$layouts[[name]] = layout
}

get.ps.layout = function(name, ps=get.ps(), load.defaults=FALSE) {
  restore.point("get.ps.layout")
  
  if (is.list(name)) return(name)
  if (load.defaults & (!name %in% names(ps$layouts) )) {
    def = load.default.layouts()
    new = setdiff(names(def), names(ps$layouts))
    ps$layouts[new] = def[new]
  }
  ps$layouts[[name]]
}


