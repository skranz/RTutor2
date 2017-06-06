
# blocks specified in RTutor
RTutor3.block.types.df = function(...) {
  restore.point("RTutor3.block.types.df")

  types = c(
    "chunk","quiz","award",
    "show","notest","show_notest","hint","test","test_arg",
    "rmdform"
  )
  widgets = c("chunk","quiz","award","rmdform")
  parent.types = c("chunk","award")
  container.types = c("award")
  remove.inner = c("rmdform","quiz")
  
  n = length(types)
  bt.df = data_frame(type=types, package="RTutor3", is.widget=types %in% widgets, parse.inner.blocks = (type!="chunk"), remove.inner.blocks= types %in% remove.inner, is.parent=types %in% parent.types, is.container = types %in% container.types, dot.level=0, arg.li = vector("list",n))
  
  bt.df
}
