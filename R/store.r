get.store.file = function(id = ps$bdf$id[[bi]],mode="cur", ts=NULL, bi=ts$bi, ups=get.ups(), ps=get.ps()) {
  
  if (mode=="cur" | mode =="hist") 
   return(paste0(mode,"_",id,"___",ups$userid,".json"))
 return(paste0(mode,"_",id,".json"))
  
}

rt.store = function(values, id = ps$bdf$id[[bi]], modes=c("cur","hist_all",), ts=NULL, bi=ts$bi, ups=get.ups(), ...) {
  restore.point("rt.store")
  
  du = list(TIME_ = as.character(Sys.time()), USER_ = ups$user.name)
  if (!is.data.frame(values)) {
    values = c(du, values)
  } else {
    values = cbind(as_data_frame(du), values)
  }
  
  for (mode in modes) {
    append = mode == "hist_all" | mode =="hist"
    file = get.store.file(mode=mode, id=id)
    webforms::write.ndjson(values, file, append=append)
  }
  
}

rt.fetch = function(id, mode="cur",as.data.frame= mode == "hist" | mode == "hist_all") {
  restore.point("rt.fetch")
  
  file = get.store.file(mode=mode, id=id)
  if (!file.exists(file)) return(NULL)
  res = webforms::read.ndjson(file,as.data.frame = as.data.frame)
  if (!as.data.frame & (mode=="cur" | mode =="hist"))
    res = res[[1]]
  res
}