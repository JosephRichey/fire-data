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

globalThis.setCheckboxValue = function(inputId, value) {
  const el = document.getElementById(inputId);

  if (el) {
    el.checked = value;
    Shiny.setInputValue(inputId, value, { priority: "event" });
    console.log("Checkbox value set directly");
    
  } else {
    const observer = new MutationObserver(() => {
      const el = document.getElementById(inputId);
      if (el) {
        console.log("Checkbox found after mutation");
        el.checked = value;
        Shiny.setInputValue(inputId, value, { priority: "event" });
        observer.disconnect();
      }
    });

    observer.observe(document.body, { childList: true, subtree: true });
  }
}

Shiny.addCustomMessageHandler("jsCode", function(message) {
  eval(message.code);
});


window.enableScroll = function () {
  console.log("Enabling scroll");
  document.body.style.overflow = 'auto';
  document.body.style.paddingRight = '17px';
  
  Shiny.setInputValue("app-incident_response-time_adjust_needed", false, {priority: "event"})
  
};