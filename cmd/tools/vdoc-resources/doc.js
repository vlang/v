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

    var html = document.getElementsByTagName('html')[0];
    var darkModeToggle = document.getElementById('dark-mode-toggle');
    darkModeToggle.addEventListener('click', function() {
        html.classList.toggle('dark');
        localStorage['dark-mode'] = html.classList.contains('dark');
    });
    localStorage['dark-mode'] == 'true' && html.classList.add('dark');
})();
