rtutorAlert = function(session=getApp()$session, id,title=NULL, content=message,style=type, message=content, append=FALSE,type=style,...) {
    res = try(createAlert(session,id, 
        title = title, 
        message= content,
        type = "info", append=FALSE,...
    ), silent=TRUE)
    if (is(res,"try-error")) {
      res = try(createAlert(session,id, 
          title = title, 
          content= content,
          style = "info", append=FALSE,...
      ), silent=TRUE)
    }
}

myCollapsePanel = function (title, ..., value = title, panel.style = "default", header.style="", body.style="") 
{
    content <- list(...)
    id <- paste0("cpanel", sprintf("%07i", as.integer(stats::runif(1, 
        1, 1e+06))))
    if (is.null(value)) {
        value = title
    }
    bsTag <- shiny::tags$div(class = paste0("panel panel-", panel.style), 
        value = value, shiny::tags$div(style=header.style,class = "panel-heading", 
            role = "tab", id = paste0("heading_", id), shiny::tags$h4(class = "panel-title", 
                shiny::tags$a(`data-toggle` = "collapse", href = paste0("#", 
                  id), title))), shiny::tags$div(id = id, class = "panel-collapse collapse", 
            role = "tabpanel", shiny::tags$div(style=body.style,class = "panel-body", 
                content)))
    #htmltools::attachDependencies(bsTag, shinyBSDep)
}