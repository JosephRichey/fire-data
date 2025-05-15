globalThis.edit_incident = function(ns, id) {
  console.log("working from edit_incident");
  var fullInputId = ns + "edit_incident";   
  console.log("Setting input with id: " + fullInputId);   
  Shiny.setInputValue(fullInputId, id, {priority: "event"}); 
};

globalThis.edit_response = function(ns, id) {
  console.log("working from edit_response");
  var fullInputId = ns + "edit_response";   
  console.log("Setting input with id: " + fullInputId);   
  Shiny.setInputValue(fullInputId, id, {priority: "event"}); 
};

globalThis.add_response = function(ns, id) {
  console.log("working from add_response");
  var fullInputId = ns + "add_response";   
  console.log("Setting input with id: " + fullInputId);   
  Shiny.setInputValue(fullInputId, id, {priority: "event"}); 
};

Shiny.addCustomMessageHandler("jsCode", function(message) {
  eval(message.code);
});


window.enableScroll = function () {
  console.log("Enabling scroll");
  document.body.style.overflow = 'auto';
  document.body.style.paddingRight = '17px';
};