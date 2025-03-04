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
window.addEventListener('hashchange', updateActiveTab);

export function finalize_incident(ns, id) {
  console.log("working from finalize_incident");
  var fullInputId = ns + "finalize_incident";
  console.log("Setting input with id: " + fullInputId);
  Shiny.setInputValue(fullInputId, id, {priority: "event"});
};

export function show_details(ns, id) {
  console.log("working from show_details");
  var fullInputId = ns + "show_details";
  console.log("Setting input with id: " + fullInputId);
  Shiny.setInputValue(fullInputId, id, {priority: "event"});
};

export function toggle_firefighter(ns, id) {
  console.log("working from toggle_firefighter");
  var fullInputId = ns + "toggle_firefighter";
  console.log("Setting input with id: " + fullInputId);
  Shiny.setInputValue(fullInputId, id, {priority: "event"});
};
