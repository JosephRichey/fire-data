//https://chatgpt.com/share/67b51cca-bce4-800b-be78-8db9f1d33c2e

function updateActiveTab() {
    let currentHash = "./" + window.location.hash;  // Add "./" to match href values

    document.querySelectorAll('.nav-icons a').forEach(link => {
        link.classList.remove('active');  // Remove active from all links
        if (link.getAttribute('href') === currentHash) {
            link.classList.add('active');  // Add active to matching link
        }
    });
}

// Run on page load
updateActiveTab();

// Run whenever the route changes
globalThis.addEventListener('hashchange', updateActiveTab);

globalThis.finalize_incident = function(ns, id) {
  console.log("working from finalize_incident");
  var fullInputId = ns + "finalize_incident";
  console.log("Setting input with id: " + fullInputId);
  Shiny.setInputValue(fullInputId, id, {priority: "event"});
};

globalThis.show_details = function(ns, id) {
  console.log("working from show_details");
  var fullInputId = ns + "show_details";
  console.log("Setting input with id: " + fullInputId);
  Shiny.setInputValue(fullInputId, id, {priority: "event"});
};

globalThis.toggle_firefighter = function(ns, id) {
  console.log("working from toggle_firefighter");
  var fullInputId = ns + "toggle_firefighter";
  console.log("Setting input with id: " + fullInputId);
  Shiny.setInputValue(fullInputId, id, {priority: "event"});
};

console.log("Clipboard.js loaded!");
Shiny.addCustomMessageHandler('txt', function(txt) {
    navigator.clipboard.writeText(txt).then(() => {
        console.log("Text copied to clipboard!");
    }).catch(err => {
        console.error("Failed to copy text: ", err);
    });
});

Shiny.addCustomMessageHandler("disableInput", function(id) {
  document.getElementById(id)?.setAttribute("disabled", "disabled");
  console.log("Disabled input with id: " + id);
});

Shiny.addCustomMessageHandler("enableInput", function(id) {
  document.getElementById(id)?.removeAttribute("disabled");
  console.log("Enabled input with id: " + id);
});


