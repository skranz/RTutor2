
# Add javascript to deal with clicks on free html area,
# i.e. not on inputs, buttons, links or images
# can be used to proceed with slides
rtutorClickHandler = function(button.handler=TRUE, image.handler = TRUE, use.frame.click = TRUE,opts = rt.opts()) {
  restore.point("rtutorClickHandler")
  
  
  code =  '$(document).on("click", function (e) {'

  # Variable definitions
  code = paste0(code,'
    var tag = e.target.nodeName;
    var eclass = e.target.className;
    var pn = e.target.parentNode;
    var gpn = pn.parentNode;
  ')
  

  if (button.handler) {
    eventId = "buttonHandlerEvent"
    code = paste0(code,'
    if (tag === "BUTTON") {
      Shiny.onInputChange("',eventId,'", {eventId: "',eventId,'", id: e.target.id, tag: tag, nonce: Math.random(), pageX: e.pageX, pageY: e.pageY});
      return;
    } else {
      var ptag = e.target.parentNode.nodeName;
      if (ptag === "BUTTON" || ptag ==="BUTTON") {
        Shiny.onInputChange("',eventId,'", {eventId: "',eventId,'", id: e.target.parentNode.id, tag: ptag, nonce: Math.random(), pageX: e.pageX, pageY: e.pageY});
        return;
      }
    }
    ')
  }

  if (image.handler) {
    imageEventId = "imageClickEvent"
    code = paste0(code,'
      if (tag === "IMG") {
        var img = $(e.target);
        var offset = img.offset();

        var oimg = document.getElementById(img.attr("id"));

        var xratio = oimg.naturalWidth / img.width();
        var yratio = oimg.naturalHeight / img.height();

        //alert("Image click: offset= "+JSON.stringify(offset));
        var x = (e.pageX - offset.left)*xratio;
        var y = (e.pageY - offset.top)*yratio;
        Shiny.onInputChange("',imageEventId,'", {eventId: "',imageEventId,'", id: e.target.id, x: x, y: y, tag: tag, nonce: Math.random(), pageX: e.pageX, pageY: e.pageY});
        return;
      }
    ')
    registerEvent("imageClickEvent", jscript="", overwrite=TRUE)
    
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
      Shiny.onInputChange("documentClickHandlerEvent", {id: e.target.id, tag: tag, class: eclass, pClass: pn.className, gpClass: gpn.className, nonce: Math.random(), pageX: e.pageX, pageY: e.pageY});
    ')
  }
  
  code = paste0(code,'
  });  
  ')

  res = bottomScript(HTML(code))  
  registerEvent("documentClickHandlerEvent", jscript="", overwrite=TRUE)
  registerEvent("buttonHandlerEvent", jscript="", overwrite=TRUE)
  return(res)
}
