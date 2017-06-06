
give.award = function(award.bi, ups = get.ups(), ps=get.ps()) {
  restore.point("give.award")

  if (length(award.bi)==0) return()
  
  if (has.award(award.bi)) return()
  
  ps$given.awards.bi = c(ps$given.awards.bi, award.bi)
  
  if (!isTRUE(ps$is.shiny)) {
    award = get.award(award.bi)

    message(paste0('
**********************************************************
* Congrats, you earned the award "', award$award.name, '"
**********************************************************

PS: awards() shows all your awards
'))
  }
  show.award(award.bi)
  return(TRUE)
}

has.award = function(award.bi, ps=get.ps()) {
  award.bi %in% ps$given.awards.bi
}

get.award = function(award.bi, ps =get.ps()) {
  ps$bdf$obj[[award.bi]]  
}

show.award = function(award.bi, ps=get.ps()) {
  restore.point("show.award")
  
  if (isTRUE(ps$is.shiny)) {
    show.container(ps=ps,bi=award.bi)
    return()
  }
  
  award = get.award(award.bi)
  htmlFile <- tempfile(fileext=".html")
  html = paste0("<h4>Award: ", award$award.name,"</h4>", award$html)
  writeLines(html,htmlFile)
  if (require(rstudioapi)) {
    rstudioapi::viewer(htmlFile)
  } else {
    cat(paste0("\n*** ",award$award.name, " ***\n", award$txt,"\n"))
  }
}


#' Show all your awards
#' @export
awards = function(as.html=FALSE, details=TRUE, ps = get.ps()) {
  restore.point("awards")
  
  awards.bi = ps$given.awards.bi
  awards = lapply(awards.bi, get.award)
    
  if (!as.html) {
    cat(paste0("You have earned ", length(awards.bi)," awards:\n"))
    if (!details) {
      award.names = sapply(awards, function(award) award$award.name)
      print(award.names)
    } else {
      for (ad in awards) {
        cat(paste0("\n*** ",ad$award.name, " ***\n", ad$txt,"\n"))
      }
    }
  } else {
    if (!details) {
      award.names = sapply(awards, function(award) award$award.name)
      txt = paste0("<h4>",awards.names,"...</h4>")
    } else {
      txt = sapply(awards, function(ad) {
        paste0(ad$html)
      })
    }
    txt = c(paste0("<h3>You have earned ", length(awards.bi)," awards</h3>"),txt)

    txt = HTML(paste0(txt, collapse="\n"))
    txt
  }
}

has.award = function(award.name,ups=get.ups()) {
  award.name %in% names(ups$awards)
}

rtutor.parse.award = function(bi,ps) {
  restore.point("rtutor.parse.award")
  br = ps$bdf[bi,]
  
  args = parse.block.args(arg.str = ps$bdf$arg.str[[bi]])
  award.name = args$name
  
  res = get.children.and.fragments.ui.list(bi,ps, keep.null=FALSE)
  out.rmd = merge.lines(c(paste0("---\n### Award ",award.name),res$rmd$rmd,"---"))
  rmd = list(shown="",sol="",rmd=out.rmd, newline=FALSE)
  content.ui=res$ui.li
  obj = list(award.bi =bi, award.name=award.name, html=as.character(tagList(content.ui)), txt = res$out.rmd)
  ps$bdf$obj[[bi]] = obj
  
  title = paste0("Award: ",award.name) 
  
  inner.ui = tagList(br(),shinyBS::bsCollapse(id = paste0("award_collapse_",bi), myCollapsePanel(title=title,header.style="background-color: #DFC463;box-shadow: 2px 2px 2px #888888;",content.ui)))

  armd.parse.as.container(bi=bi,am=ps,args = args, inner.ui=inner.ui,rmd = rmd,highlight.code = TRUE,is.widget = FALSE,title = NULL, is.hidden = TRUE)  
}

