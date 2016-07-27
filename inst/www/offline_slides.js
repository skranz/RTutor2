var rtSlideNum = -1;
var rtNumSlides = 3;
var rtSlideIds = [];
var prevSlideId = "";


$(".rtNextBtn").click(function(){
  rtShowNext();
});

$(".rtPrevBtn").click(function(){
  rtShowPrev();
});

function rtShowNext() {
  if (rtSlideNum < rtNumSlides) {
    rtShowSlide(rtSlideNum+1);
  }
}

function rtShowPrev() {
  if (rtSlideNum > 1) {
    rtShowSlide(rtSlideNum-1);
  }
}

function rtAdaptSlideMargin(slideNum) {

  var id = rtSlideIds[slideNum-1];
  var slide = $("#"+id);

  
  var old_left = parseInt($("#"+id).css('padding-left').replace('px', ''));
  var old_right = parseInt($("#"+id).css('padding-right').replace('px', ''));
  alert("current left-padding" + old_left);

  var oslide = document.getElementById(id);
  
  var sw = oslide.scrollWidth - old_left - old_right;
  var sh = oslide.scrollHeight;

  var ww = $(window).width();
  var wh = $(window).height();
  var min_pad = 0.025 * ww;
  var max_pad = 0.15 * ww;
  
  var pad = (ww - sw) / 2;
  if (pad < min_pad) {
    pad = min_pad; 
  } else if (pad > max_pad) {
    pad = max_pad;
  }
  
  $("#"+id).css({"padding-left": pad, "padding-right": pad});
  alert("1. slide: "+ sw +", "+ sh + " window: "+ ww +", "+ wh+" padding = " + pad);

  // Check if there is still a height overflow and if yes set padding to 0
  oslide = document.getElementById(id);
  sh = oslide.scrollHeight;
  alert("2. slide: "+ sw +", "+ sh + " window: "+ ww +", "+ wh+" padding = " + pad);
  if (sh > wh & pad > min_pad) {
    pad = min_pad;
    $("#"+id).css({"padding-left": pad, "padding-right": pad});
    alert("Padding of "+ id + " adapted to " + perc_pad);
  }
  
}

function rtShowSlide(slideNum) {
  
  if (slideNum == rtSlideNum) return;
  rtSlideNum = slideNum;
  var id = rtSlideIds[rtSlideNum-1];
  
  var slide = $("#"+id);

  $("#"+id).css({"display": "block", "visibility": "hidden"});
  //rtAdaptSlideMargin(rtSlideNum);
  $("#"+id).css({"padding-left": "12%", "padding-right": "12%"});
  $("#"+id).css({"visibility": "visible"});
  if (prevSlideId !== "") {
    $("#"+prevSlideId).css("display","none");
  }
  prevSlideId = id;
}

function rtWasEventForDocument(e) {
  var tag = e.target.nodeName;
  var eclass = e.target.className;
  var pn = e.target.parentNode;
  var gpn = pn.parentNode;
  
  if (tag === "BUTTON") {
    return false;
  } else {
    var ptag = e.target.parentNode.nodeName;
    if (ptag === "BUTTON" || ptag ==="BUTTON") {
      return false;
    }
  }

  if (tag === "A") {
    return false;
  }

  if (tag === "IMG") {
    if (tag === "BUTTON" || tag === "IMG" || tag === "INPUT" || tag === "A") {
      return false;
    }
    if (pn.className === "radio" || pn.className === "checkbox") {
      return false;
    }
    if (gpn.className === "radio" || gpn.className === "checkbox") {
      return false;
    }
  }
  if (tag === "DIV") {
    if (eclass === "ace_content" || eclass === "ace_scroller" || eclass === "ace_gutter" || pn.className === "ace_scroller" || pn.className === "ace_gutter" || gpn.className === "ace_gutter" || gpn.className === "ace_scroller") {
      return false;
    }
  }
  return true;
}

$(document).on("click", function (e) {
  if(! rtWasEventForDocument(e)) return;
  
  var pageX = e.pageX;
  var ww = $( window ).width();
  if (pageX / ww <= 0.2) {
    if (rtSlideNum > 1) {
      rtShowSlide(rtSlideNum-1);
    }
  } else {
    if (rtSlideNum < rtNumSlides) {
      rtShowSlide(rtSlideNum+1);
    }
  }
});  


$(document).keydown(function(e) {
  if(! rtWasEventForDocument(e)) return;
  
  var code = e.which;
  
  if (code == 37) { // left arrow
    rtShowPrev();
    e.preventDefault(); // prevent the default action (scroll / move caret)
  } else if (code == 39) {
    rtShowNext();  // right arrow
    e.preventDefault(); // prevent the default action (scroll / move caret)
  }
});