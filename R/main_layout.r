ps.layout = function(ps, ps.content.ui, opts = rt.opts(), main.layout = opts$main.layout) {
  default.ps.layout(ps, ps.content.ui)
}


default.ps.layout = function(ps, ps.content.ui,...) {
  restore.point("default.ps.layout")
  
  json.opts =
'
defaults: {
  resizable: false,
  closable: false,
  slideable: false,
  spacing_closed: 0,
  spacing_open: 0
},
north: {
  size: "auto",
  resizable: false,
  closable: false,
  slideable: false
},
east: {
  initClosed: true,
  closable: true,
  resizable: true,
  spacing_open: 5
},
center: {
  closable: false,
  resizable: false
}


'
  style = tags$style(HTML('
.ui-layout-east {
	background:	#FFF;
	border:		none;
	padding:	0px;
	overflow:	auto;
}


.ui-layout-north {
	background:	#FFF;
	border:		none;
	padding:	0px;
	overflow:	hidden;
}

.ui-layout-center {
	background:	#FFF;
	border:		none;
	padding:	2px;
	overflow:	auto;
}


'
  ))
  title = first.non.null(ps$opts$title,"RTutor")
  
  if (is.false(ps$opts$use_sidebar)) {
    sidebar.btn =  sidebar.ui = NULL
  } else {
    sidebar.btn = HTML(
'<button id="toogleEastBtn" type="button" class="btn btn-default action-button shiny-bound-input btn-xs" onclick="mainLayoutLayoutVar.toggle(\'east\')">
        <i class="fa fa-bars"></i>
</button>')
    sidebar.ui = sidebar.ui()
  }
  
  tagList(jqueryLayoutHeader(), jqueryLayoutPanes(id="mainLayout",style=style,json.opts = json.opts,
    north = div(
      style="margin-left: 0px; margin-right: 0px;color: #333;",
      HTML("<table width='100%'><tr><td style='white-space: nowrap;  padding-left: 4px;'>"),
      HTML(title),
      HTML("</td><td align='left' style='width: 100%; padding-left: 10px;'>"),
      ps$menu.sel.ui$ui,
      HTML("</td><td align='right' valign='top' nowrap>"),
      HTML("<table><tr><td valign='center' nowrap>"),
      sidebar.btn,
      HTML("</td></tr></table>"),
      HTML("</td></tr></table>"),
      hr(style="padding: 0; margin: 0; background-color: #999999; height: 1px;")
     ),
    center = div(
      #style="margin-left: 15%; margin-right: 15%;",
      style="max-width: 50em; margin:auto; padding-left: 5px; padding-right: 5px",
      #ps$navbar.ui,
      with.mathjax(ps.content.ui)
    ),
    east = div(
      sidebar.ui
    )
  ))
}


fixed.section.menu.layout = function(ps, ps.content.ui) {
    json.opts =
'
defaults: {
  //spacing_open: 4
},
north: {
  size: "auto",
  resizable: false,
  //spacing_open: 4,
  spacing_closed: 10
},
east: {
  initClosed: true,
  closable: true,
  resizable: true
}
'
  style = tags$style(HTML('
.ui-layout-east {
	background:	#FFF;
	border:		none;
	padding:	0px;
	overflow:	auto;
}


.ui-layout-north {
	background:	#FFF;
	border:		none;
	padding:	0px;
	overflow:	auto;
}

.ui-layout-center {
	background:	#FFF;
	border:		none;
	padding:	2px;
	overflow:	auto;
}


'
  ))

    json.opts =
'
defaults: {
  //spacing_open: 4
},
north: {
  size: "auto",
  resizable: false,
  //spacing_open: 4,
  spacing_closed: 10
},
east: {
  closable: true,
  resizable: true
}
'
  
  
  jqueryLayoutPage(style=style,
    north = div(
      style="margin-left: 2px; margin-right: 2px;",
      ps$navbar.ui
    ),
    center = div(
      #style="margin-left: 10%; margin-right: 10%; overflow: auto; height: 100%;",
      style="margin-left: 10%; margin-right: 10%;",
      with.mathjax(ps.content.ui)
    ),
    east = div(
      sidebar.ui()
    )
  )
}


rtutor.navbar = function(ps, opts=rt.opts(), nav.levels = c("section","subsection","frame")) {
  restore.point("rtutor.navbar")
  
  bdf = ps$bdf
  nav.levels = intersect(nav.levels, bdf$type)
  

  get.raw.selector = function(nav.levels=nav.levels, parent.bi=NULL, level=1, name=nav.levels[[1]]) {
    restore.point("get.raw.selector")
    #stop()
    if (is.null(parent.bi)) {
      bis = which(bdf$type == nav.levels[1])
    } else {
      bis = which(bdf$parent_container==parent.bi)
    }
    types = bdf$type[bis]
    ignore = !types %in% nav.levels
    bis = bis[!ignore]
    types = types[!ignore]
    if (length(bis)==0) {
      return(NULL)
    }
    
    titles = sapply(seq_along(bis), function(i) {
      bi = bis[i]
      obj = bdf$obj[[bi]]
      title = first.non.null(obj$button.label, obj$title, obj$name, paste0(bdf$type[[bi]]," ",bdf$stype.ind[[bi]]))
      title
    })
    choices = bis
    names(choices) = titles
    
    contents = bdf$div.id[bis]
    children = as.list(paste0("s",bis))
    names(children) = bis

        
    children.sel = lapply(bis, function(bi) {
      child.sel.li = get.raw.selector(nav.levels=nav.levels,parent.bi = bi,level=level+1, name=paste0("s",bi))[[1]]  
    }) 
    names(children.sel) = paste0("s",bis)
    is.child = !sapply(children.sel, is.null)
    children = children[is.child]
    children.sel = children.sel[is.child]
    
    sel = list(list(
      bis = bis,
      choices = choices,
      children = children,
      div = as.character(level),
      contents = contents
    ))
    names(sel)=name
    c(sel,children.sel)
  }
  
  ps$menu.selectors = get.raw.selector(nav.levels=nav.levels, level=1, name="ps")
  
  ps$menu.sel.ui = nestedSelector(id="rtNavbarSelector", btn.size="xs", ps$menu.selectors, input.type="radioBtnGroup", scroll.top.sel = "#mainLayout_center")
  
  
  
  title = first.non.null(opts$menu.title, "Sections")
  ps$navbar.ui = div(
    #style = "background-color: #eeeeee;",
    HTML("<table><tr><td style='padding-left: 5px; padding-right: 10px; vertical-align: top;'>"),
    HTML(title),
    HTML("</td><td>"),
    ps$menu.sel.ui$ui,
    HTML("</td></tr></table>")
   )
  ps$navbar.ui
} 

slide.title.bar.ui = function(title, slide.ind, num.slides) {
  div(class="rtutor-slide-title-bar",
    HTML("<table width='100%'><tr><td>"),
    h4(class="slide_title",title),
    HTML("</td><td align='right' valign='top' nowrap>"),
    HTML("<table><tr><td valign='center' nowrap>"),
    div(class="nav_buttons_div",  rtutor.navigate.btns()),
    HTML("</td><td valign='center' nowrap style='padding-left: 5px'>"),
    HTML(paste0(slide.ind, " of ",num.slides)),      
    HTML("</td></tr></table>"),
    HTML("</td></tr></table>")
  )  
}
