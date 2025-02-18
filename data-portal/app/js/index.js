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
