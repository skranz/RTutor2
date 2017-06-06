
get.bdf.ind = function(line=NULL,type.ind=NULL, bdf.ind=NULL, bdf=NULL, type=NULL, te=NULL) {
  restore.point("get.bdf.ind")

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

