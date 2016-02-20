
# Add javascript to deal with clicks on free html area,
# i.e. not on inputs, buttons, links or images
# can be used to proceed with slides
rtutorClickHandler = function(button.handler=TRUE, use.frame.click = TRUE,opts = ps.opts()) {
  code =  '$(document).on("click", function (e) {'

  # Variable definitions
  code = paste0(code,'
    var tag = e.target.nodeName;
    var eclass = e.target.className;
    var pn = e.target.parentNode;
    var gpn = pn.parentNode;
  ')
  
  if (button.handler) {
    code = paste0(code,'
      if (tag === "BUTTON") {
        Shiny.onInputChange("shinyEventButtonClick", {id: e.target.id, eventId:"shinyEventButtonClick", tag: tag, nonce: Math.random(), pageX: e.pageX, pageY: e.pageY});
        return;
      }
    ')
  }
  
  if (use.frame.click) {
    # General no click handler
    code = paste0(code,'
      if (tag === "BUTTON" || tag === "IMG" || tag === "INPUT" || tag === "A") {
        return;
      }
      if (pn.className === "radio" || pn.className === "checkbox") {
        return;
      }
      if (gpn.className === "radio" || gpn.className === "checkbox") {
        return;
      }
    ')
    # don't handle clicks from ACE editor
    code = paste0(code,'
      if (tag === "DIV") {
        if (eclass === "ace_content" || eclass === "ace_scroller" || eclass === "ace_gutter" || pn.className === "ace_scroller" || pn.className === "ace_gutter" || gpn.className === "ace_gutter" || gpn.className === "ace_scroller") {
          return;
        }
      }
    ')
    # if not returned, register doc_click handler
    code = paste0(code,'
      Shiny.onInputChange("doc_click", {id: e.target.id, tag: tag, class: eclass, pClass: pn.className, gpClass: gpn.className, nonce: Math.random(), pageX: e.pageX, pageY: e.pageY});
    ')
  }
  
  code = paste0(code,'
  });  
  ')

  res = tags$script(code)  
  #registerEventIdHandler("doc_click")
  
  eventId = "shinyEventButtonClick"
  registerEventIdHandler("shinyEventButtonClick")
  
  # ca = substitute(env = list(eventId=eventId),
  # observeEvent(input[[eventId]],{
  #   value = input[[eventId]]
  #   shiny.events.button.click.handler(value)
  # })
  # )
  # addEventHandlerToApp(id=eventId,call=ca,type="change",app=getApp())
  return(res)
}
