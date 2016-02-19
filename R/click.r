examples.click = function() {
  app = eventsApp()
  app$button.click.handlers.env = new.env() 
  eventId = "buttonClick"
  app$ui = tagList(
    buttonHandlerScript(),
    fluidPage(
      actionButton("btn1","btn1"),
      actionButton("btn2","btn2"),
      actionButton("btn3","btn3")
    )
  )
  buttonHandler("btn1",var=45,function(id, var,...) {
    print(var)
  })
  viewApp(app)
}

# more efficient version of button handler
buttonHandler = function(id, fun,..., app=getApp()) {
  args = list(...)
  app$button.click.handlers.env[[id]] = list(fun=fun,args=args)
}

shiny.events.button.click.handler = function(value,..., app=getApp()) {
  restore.point("button.click.handler")
  id = value$id
  h = app$button.click.handlers.env[[id]]
  if (is.null(h)) {
    cat("\nNo click handler for ", id," registered.")
    return()
  }
  do.call(h$fun,c(list(id=id),h$args))
}


buttonHandlerScript = function(eventId="shinyEventButtonClick") {
  restore.point("buttonHandlerScript")
  
  res = tags$script(paste0('
  $(document).on("click", function (e) {
    var tag = e.target.nodeName;
    if (tag === "BUTTON") {
      Shiny.onInputChange("',eventId,'", {id: e.target.id, tag: tag, nonce: Math.random(), pageX: e.pageX, pageY: e.pageY});
    }
  });'))
  
  
  ca = substitute(env = list(eventId=eventId),
  observeEvent(input[[eventId]],{
    value = input[[eventId]]
    shiny.events.button.click.handler(value)
  })
  )
  addEventHandlerToApp(id=id,call=ca,type="change",app=app)
  #changeHandler(eventId,shiny.events.button.click.handler)
  return(res)
}

# Add javascript to deal with clicks on free html area,
# i.e. not on inputs, buttons, links or images
# can be used to proceed with slides
rtutorClickHandler = function() { 
  res = tags$script(paste0('
  $(document).on("click", function (e) {
    var nn = e.target.nodeName;

    if (nn === "BUTTON") {
      Shiny.onInputChange("shinyEventButtonClick", {id: e.target.id, tag: nn, nonce: Math.random(), pageX: e.pageX, pageY: e.pageY});
      return;
    }


    if (nn === "BUTTON" || nn === "IMG" || nn === "INPUT" || nn === "A") {
      return;
    }
    var pn = e.target.parentNode;
    if (pn.className === "radio" || pn.className === "checkbox") {
      return;
    }
    var gpn = pn.parentNode;
    if (gpn.className === "radio" || gpn.className === "checkbox") {
      return;
    }

    Shiny.onInputChange("doc_click", {id: e.target.id, tag: nn, nonce: Math.random(), pageX: e.pageX, pageY: e.pageY});
  });'))
  eventId = "shinyEventButtonClick"
  
  ca = substitute(env = list(eventId=eventId),
  observeEvent(input[[eventId]],{
    value = input[[eventId]]
    shiny.events.button.click.handler(value)
  })
  )
  addEventHandlerToApp(id=eventId,call=ca,type="change",app=getApp())
  return(res)
}


# Add javascript to deal with clicks on free html area,
# i.e. not on inputs, buttons, links or images
# can be used to proceed with slides
docClickEvents = function(id="doc_click") { 
  tags$script(paste0('
  $(document).on("click", function (e) {
    var nn = e.target.nodeName;

    if (nn === "BUTTON" || nn === "IMG" || nn === "INPUT" || nn === "A") {
      return;
    }
    var pn = e.target.parentNode;
    if (pn.className === "radio" || pn.className === "checkbox") {
      return;
    }
    var gpn = pn.parentNode;
    if (gpn.className === "radio" || gpn.className === "checkbox") {
      return;
    }

    Shiny.onInputChange("',id,'", {id: e.target.id, tag: nn, nonce: Math.random(), pageX: e.pageX, pageY: e.pageY});
  });'))
}

