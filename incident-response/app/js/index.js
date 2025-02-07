export function edit_incident(ns, id) {
  console.log("working from edit_incident");
  // Construct the full namespaced id, e.g. if ns is "app-" then "app-jsValue"   
  var fullInputId = ns + "edit_incident";   
  console.log("Setting input with id: " + fullInputId);   
  Shiny.setInputValue(fullInputId, id, {priority: "event"}); 
}

