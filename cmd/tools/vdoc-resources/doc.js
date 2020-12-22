(function() {
    if (document.body.scrollIntoView) {
        var docnav = document.querySelector('.doc-nav');
        var active = docnav.querySelector('li.active');
        if (active) {
            active.scrollIntoView({ block: 'center', inline: 'nearest' });
        }
    }
    setupScrollSpy();
    setupMobileToggle();
    setupDarkMode();
    setupSearch();
    setupCollapse();
})();

function setupScrollSpy() {
    var sectionPositions = [];
    var sections = document.querySelectorAll('section');
    sections.forEach(function(section) {
        sectionPositions.push(section.offsetTop);
    });
    var scrollPos = 0;
    window.addEventListener('scroll', function(e) {
        // Reset classes
        document.querySelectorAll('.doc-toc a[class="active"]').forEach(function(link) {
            link.classList.remove('active');
        });
        // Set current menu link as active
        var scrollPosition = document.documentElement.scrollTop || document.body.scrollTop;
        for (var i = 0; i < sectionPositions.length; i++) {
            var section = sections[i];
            var position = sectionPositions[i];
            if (position >= scrollPosition) {
                var link = document.querySelector('.doc-toc a[href="#' + section.id + '"]');
                if (link) {
                    link.classList.add('active');
                    var docToc = document.querySelector('.doc-toc');
                    var tocHeight = docToc.clientHeight;
                    var scrollTop = docToc.scrollTop;
                    if ((document.body.getBoundingClientRect()).top < scrollPos && scrollTop < link.offsetTop - 10) {
                        docToc.scrollTop = link.clientHeight + link.offsetTop - tocHeight + 10;
                    } else if (scrollTop > link.offsetTop - 10) {
                        docToc.scrollTop = link.offsetTop - 10;
                    }
                }
                break;
            }
        }
        scrollPos = (document.body.getBoundingClientRect()).top;
    });
}

function setupMobileToggle() {
    var toggle = document.getElementById('toggle-menu');
    toggle.addEventListener('click', function(ev) {
        document.querySelectorAll('.doc-nav').forEach(function(el) {
            el.classList.toggle('hidden');
        });
        document.querySelectorAll('.doc-nav .content').forEach(function(el) {
            el.classList.toggle('hidden');
            el.classList.toggle('show');
        });
    });
}

function setupDarkMode() {
    var darkModeToggle = document.getElementById('dark-mode-toggle');
    darkModeToggle.addEventListener('click', function() {
        html.classList.toggle('dark');
        var isDarkModeEnabled = html.classList.contains('dark');
        localStorage.setItem('dark-mode', isDarkModeEnabled);
        darkModeToggle.setAttribute('aria-checked', isDarkModeEnabled)
    });
    // Check if css var() is supported and enable dark mode toggle
    if (window.CSS && CSS.supports('color', 'var(--fake-var)')) {
        darkModeToggle.style.visibility = 'unset';
    }
}

function setupSearch() {
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
}

function setupCollapse() {
    var dropdownArrows = document.querySelectorAll('.dropdown-arrow');
    for (var i = 0; i < dropdownArrows.length; i++) {
        var dropdownArrow = dropdownArrows[i];
        dropdownArrow.addEventListener('click', function(e) {
            var parent = e.target.parentElement.parentElement.parentElement;
            parent.classList.toggle('open');
        });
    }
}
