
# blocks specified in RTutor
RTutor2.block.types.df = function(...) {
  restore.point("RTutor2.block.types.df")

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
  bt.df = fast_df(type=types, package="RTutor2", is.widget=types %in% widgets, parse.inner.blocks = (types!="chunk"), remove.inner.blocks= types %in% remove.inner, is.parent=types %in% parent.types, is.container = types %in% container.types, dot.level=0, arg.li = vector("list",n))
  
  bt.df
}
