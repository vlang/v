(function() {
    // Mobile view menu toggle button
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

    // Dark mode
    var html = document.getElementsByTagName('html')[0];
    var darkModeToggle = document.getElementById('dark-mode-toggle');
    darkModeToggle.addEventListener('click', function() {
        html.classList.toggle('dark');
        var isDarkModeEnabled = html.classList.contains('dark');
        localStorage.setItem('dark-mode', isDarkModeEnabled);
    });
    if (localStorage.getItem('dark-mode') === 'true') {
        html.classList.add('dark');
    }

    // Check if css var() is supported and enable dark mode toggle
    if (window.CSS && CSS.supports('color', 'var(--fake-var)')) {
        darkModeToggle.style.visibility = 'unset';
    }

    // Search
    var searchInput = document.getElementById('search');
    searchInput.addEventListener('input', function(e) {
        var searchValue = e.target.value.toLowerCase();
        var menuItems = document.querySelectorAll('.content > ul > li');
        for (var i = 0; i < menuItems.length; i++) {
            var menuItem = menuItems[i];
            var links = menuItem.querySelectorAll('a');
            var hasResult = false;
            for (var li = 0; li < links.length; li++) {
                var link = links[li];
                if (!searchValue || link.text.toLowerCase().indexOf(searchValue) !== -1) {
                    hasResult = true;
                }
                if (li > 0) {
                    if (!searchValue || link.text.toLowerCase().indexOf(searchValue) !== -1) {
                        link.style.display = '';
                    } else {
                        link.style.display = 'none';
                    }
                }
            }
            menuItem.style.display = !searchValue || hasResult ? '' : 'none';
        }
    });

    // Collapse
    var dropdownArrows = document.querySelectorAll('.dropdown-arrow');
    for (var i = 0; i < dropdownArrows.length; i++) {
        var dropdownArrow = dropdownArrows[i];
        dropdownArrow.addEventListener('click', function(e) {
            var parent = e.target.parentElement.parentElement.parentElement;
            parent.classList.toggle('open');
        });
    }
})();
