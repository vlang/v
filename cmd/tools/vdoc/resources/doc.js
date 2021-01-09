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
    var html = document.getElementsByTagName('html')[0];
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
    var onInputChange = debounce(function(e) {
        var searchValue = e.target.value.toLowerCase();
        var menu = document.querySelector('.doc-nav > .content');
        var search = document.querySelector('.doc-nav > .search');
        if (searchValue === '') {
            // reset to default
            menu.style.display = '';
            search.style.display = '';
        } else if (searchValue.length > 2) {
            // search for less than 3 characters can display too much results
            search.innerHTML = '';
            menu.style.display = 'none';
            search.style.display = 'block';
            // cache length for performance
            var foundModule = false;
            var searchModuleIndexLength = searchModuleIndex.length;
            var ul = document.createElement('ul');
            search.appendChild(ul);
            for (var i = 0; i < searchModuleIndexLength; i++) {
                // no toLowerCase needed because modules are always lowercase
                var title = searchModuleIndex[i];
                if (title.indexOf(searchValue) === -1) {
                    continue
                }
                foundModule = true;
                // [description, link]
                var data = searchModuleData[i];
                var description = data[0];
                var link = data[1];
                var el = createSearchResult({
                    link: link,
                    title: title,
                    description: description,
                    badge: 'module',
                });
                ul.appendChild(el);
            }
            if (foundModule) {
                var hr = document.createElement('hr');
                hr.classList.add('separator');
                search.appendChild(hr);
            }
            var searchIndexLength = searchIndex.length;
            var results = [];
            for (var i = 0; i < searchIndexLength; i++) {
                var title = searchIndex[i];
                if (title.toLowerCase().indexOf(searchValue) === -1) {
                    continue
                }
                // [badge, description, link]
                var data = searchData[i];
                var badge = data[0];
                var description = data[1];
                var link = data[2];
                var prefix = data[3];
                results.push({
                    badge: badge,
                    description: description,
                    link: link,
                    title: prefix + ' ' + title,
                });
            }
            results.sort(function(a, b) {
                if (a.title < b.title) {
                    return -1;
                }
                if (a.title > b.title) {
                    return 1;
                }
                return 0;
            });
            var ul = document.createElement('ul');
            search.appendChild(ul);
            for (var i = 0; i < results.length; i++) {
                var result = results[i];
                var el = createSearchResult(result);
                ul.appendChild(el);
            }
        }
    });
    searchInput.addEventListener('input', onInputChange);
}

function createSearchResult(data) {
    var li = document.createElement('li');
    li.classList.add('result');
    var a = document.createElement('a');
    a.href = data.link;
    a.classList.add('link');
    li.appendChild(a);
    var defintion = document.createElement('div');
    defintion.classList.add('definition');
    a.appendChild(defintion);
    if (data.description) {
        var description = document.createElement('div');
        description.classList.add('description');
        description.textContent = data.description;
        a.appendChild(description);
    }
    var title = document.createElement('span');
    title.classList.add('title');
    title.textContent = data.title;
    defintion.appendChild(title);
    var badge = document.createElement('badge');
    badge.classList.add('badge');
    badge.textContent = data.badge;
    defintion.appendChild(badge);
    return li;
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

function debounce(func, timeout) {
    var timer;
    return (...args) => {
        const next = () => func(...args);
        if (timer) {
            clearTimeout(timer);
        }
        timer = setTimeout(next, timeout > 0 ? timeout : 300);
    }
}
