(function() {
    var toggle = document.getElementById("toggle-menu");
    toggle.addEventListener("click", function(ev) {
        document.querySelectorAll(".doc-nav").forEach(function(el) {
            el.classList.toggle("hidden");
        });
        document.querySelectorAll(".doc-nav .content").forEach(function(el) {
            el.classList.toggle("hidden");
            el.classList.toggle("show");
        });
    });

    var darkModeSwitch = document.getElementById('dark-mode-switch');
    darkModeSwitch.addEventListener('change', function(event) {
        localStorage.setItem('darkmode', event.target.checked.toString());
    });
    var darkMode = JSON.parse(localStorage.getItem('darkmode')) || false;
    darkModeSwitch.checked = darkMode;
})();

