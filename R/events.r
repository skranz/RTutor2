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

# TO DO: Need to think about were to store 
# app$glob$..eventHandlersList[[eventId]]
# what shall be global, what shall be local...

# A general event handler for all sorts of java script events
# that are bound to a reactive shiny variable via
# Shiny.onInputChange("{{eventId}}", {id: e.target.id, tag: e.target.nodeName, nonce: Math.random()});
eventHandler = function(eventId,id=eventId, fun, ...,app=getApp()) {
  args = list(...)
  restore.point("eventHandler")
  value=list(fun=fun,args=args)
  if (!is.null(id)) {
    assign(x=id,value, app$glob$..eventHandlersList[[eventId]])
  } else {
    app$glob$..globalEventHandlersList[[eventId]] = value
  }
  #ls(app$..eventHandlersList[[eventId]])
  if (is.list(app$glob$..eventHandlersList[[eventId]])) stop()
}

shiny.event.id.triggered = function(value,.., app=getApp()) {
  restore.point("shiny.event.id.triggered")
  eventId = value$eventId
  id = value$id
  cat("\nevent triggered eventId = ",eventId," target id = ",id)
  h = app$glob$..eventHandlersList[[eventId]][[id]]
  if (is.null(h)) {
    app$glob$..globalEventHandlersList[[eventId]]    
  }
  if (is.null(h)) {
    cat("\nNo event handler for eventId =",eventId," target id = ", id," registered.")
    return()
  }
  do.call(h$fun,c(list(id=id, eventId=eventId),h$args))
}

# more efficient version of button handler via global eventId handler
buttonHandler = function(id, fun,..., eventId="shinyEventButtonClick", app=getApp()) {
  restore.point("buttonHandler")
  eventHandler(eventId=eventId,id=id,fun=fun,...,app=app)
  #args = list(...)
  #app$button.click.handlers.env[[id]] = list(fun=fun,args=args)
}

# register a global eventIdHandler
registerEventIdHandler = function(eventId, app=getApp()) {
  restore.point("registerEventIdHandler")
  
  if (is.null(app$glob$..eventHandlersList)) {
    app$glob$..eventHandlersList = list()    
  }
  if (is.null(app$glob$..eventHandlersList[[eventId]])) {
    app$glob$..eventHandlersList[[eventId]] = new.env()
  }
  if (is.null(app$glob$..globalEventHandlersList)) {
    app$glob$..globalEventHandlersList = list()    
  }

  
  ca = substitute(env = list(eventId=eventId),
  observeEvent(input[[eventId]],{
    value = input[[eventId]]
    shiny.event.id.triggered(value)
  })
  )
  addEventHandlerToApp(id=eventId,call=ca,type="change",app=getApp())
}

swipeLeftHandler = function(id=NULL, fun,..., eventId="swipeLeftEvent",app=getApp()) {
  restore.point("swipeLeftHandler")
  eventHandler(eventId=eventId,id=id,fun=fun,...,app=app)
}


swipeRightHandler = function(id=NULL, fun,..., eventId="swipeRightEvent",app=getApp()) {
  restore.point("swipeRightHandler")
  eventHandler(eventId=eventId,id=id,fun=fun,...,app=app)
}


swipeEvents = function(swipeLeftId="swipeLeftEvent",swipeRightId="swipeRightEvent", add.handlers=TRUE) {
  restore.point("swipeEvents")
  code = NULL
  if (!is.null(swipeLeftId)) {
    code = paste0(code,'
      $( document ).on("swipeleft", function( e ) {
        Shiny.onInputChange("',swipeLeftId,'", {id: e.target.id, tag: e.target.nodeName, nonce: Math.random()});
      });
    ')
  }
  if (!is.null(swipeRightId)) {
    eventId = swipeRightId
    code = paste0(code,'
      $( document ).on("swiperight", function( e ) {
        Shiny.onInputChange("',eventId,'", {id: e.target.id, e.target.nodeName, eventId:"',eventId,'", nonce: Math.random()});
      });
    ')
  }
  res = list(
    tags$script(src="http://code.jquery.com/mobile/1.4.5/jquery.mobile-1.4.5.min.js"),
    tags$script(code)
  )

  events = c(swipeLeftId,swipeRightId)
  for (eventId in events) {
    registerEventIdHandler(eventId)
  }
  res
} 


buttonHandlerEvents = function(eventId="shinyEventButtonClick") {
  restore.point("buttonHandlerScript")
  
  res = tags$script(paste0('
  $(document).on("click", function (e) {
    var tag = e.target.nodeName;
    if (tag === "BUTTON") {
      Shiny.onInputChange("',eventId,'", {eventId: "',eventId,'", id: e.target.id, tag: tag, nonce: Math.random(), pageX: e.pageX, pageY: e.pageY});
    }
  });'))
  
  registerEventIdHandler(eventId)
  return(res)
}


# Add javascript to deal with clicks on free html area,
# i.e. not on inputs, buttons, links or images
# can be used to proceed with slides
docClickEvents = function(id="doc_click") { 
  res = tags$script(paste0('
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

