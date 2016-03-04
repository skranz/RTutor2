
bdf.frame.filter = function(line=NULL,frame.ind=NULL,type.ind=frame.ind,bdf.ind=NULL,type,keep.precompute=TRUE) {
  bdf.type.filter(line,type.ind,bdf.ind,type="frame", keep.precompute=keep.precompute)
}

bdf.part.filter = function(line=NULL,ranked.types=c("frame","subsubsection","subsection","section"),keep.precompute=TRUE) {
  bdf.type.filter(line=line,ranked.types=ranked.types, keep.precompute=keep.precompute)
}


bdf.type.filter = function(line=NULL,type.ind=NULL,bdf.ind=NULL,type=NULL, ranked.types = NULL, keep.precompute=TRUE) {
  types.to.keep = NULL
  if (keep.precompute) {
    types.to.keep = "precompute"
  }
  function(bdf, te=NULL) {
    restore.point("in.bdf.type.filer")
    
    # if we have multiple types, pick the first type that exists
    if (!is.null(ranked.types)) {
      for (type in ranked.types) {
        if (sum(bdf$type == type)>0) break
      }
    }
    
    bdf.ind = get.bdf.ind(line=line,type.ind=type.ind,bdf.ind=bdf.ind,bdf=bdf,te=te,type=type)
    if (is.null(bdf.ind)) return(bdf)
    
    child.ind = which(bdf[,paste0("parent_",type)] == bdf.ind)
    keep = bdf$type %in% types.to.keep & bdf$index <= bdf.ind
    for (ktype in types.to.keep) {
      keep = keep | (bdf[,paste0("parent_",ktype)] >0 & bdf$index <= bdf.ind)
    }
    keep.ind = which(keep)
    
    rows = sort(unique(c(keep.ind,bdf.ind,child.ind)))
    bdf[rows,,drop=FALSE]
  }
  
}

get.bdf.ind = function(line=NULL,type.ind=NULL, bdf.ind=NULL, bdf=NULL, type=NULL, te=NULL) {
  if (!is.null(bdf.ind)) return(bdf.ind)
  if (!is.null(type.ind)) {
    return(bdf$index[bdf$type==type][type.ind]) 
  }
  if (!is.null(line)) {
    return(line.to.bdf.ind(line=line,bdf=bdf, type=type,te=te))
  }
  return(NULL)
}

bdf.ind.to.type.ind = function(bdf.ind=NULL, bdf=NULL, type.col="type") {
  type = bdf[bdf.ind,type.col]
  type.ind = sum(bdf[[type.col]][1:bdf.ind] == type)
}

line.to.type.ind = function(line,bdf,type=NULL,txt.start = if (is.null(te$txt.start)) 1 else te$txt.start, te=NULL, type.col="type", return.type.ind=FALSE) {
  line.to.bdf.ind(line, bdf,type, txt.start, te,type.col, return.type.ind=TRUE)
}


line.to.bdf.ind = function(line,bdf,type=NULL,txt.start = if (is.null(te$txt.start)) 1 else te$txt.start, te=NULL, type.col="type", return.type.ind=FALSE) {
  restore.point("line.to.bdf.ind")
  
  line = line-txt.start+1
  if (!is.null(type))
    df = bdf[bdf[[type.col]]==type,,drop=FALSE]

  rows = which(df$start <= line & df$end >= line)
  if (return.type.ind) {
    if (length(rows)>1) {
      return(rows[which.max(df$start[rows])])
    }
    return(rows)
  }
  
  if (length(rows)>1) {
    return(df$index[rows[which.max(df$start[rows])]])
  }
  return(df$index[rows])
}

