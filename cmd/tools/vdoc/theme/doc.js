(function () {
	const docnav = document.querySelector('header.doc-nav');
	const active = docnav.querySelector('li.active');
	active?.scrollIntoView({ block: 'center', inline: 'nearest' });
	setupMobileToggle();
	setupDarkMode();
	setupScrollSpy();
	setupSearch();
	setupCollapse();
	setupCodeCopy();
})();

function setupScrollSpy() {
	const mainContent = document.querySelector('#main-content');
	const toc = mainContent.querySelector('.doc-toc');
	if (!toc) {
		return;
	}
	const sections = mainContent.querySelectorAll('section');
	const sectionPositions = Array.from(sections).map((section) => section.offsetTop);
	let lastActive = null;
	let clickedScroll = false;
	const handleScroll = debounce(() => {
		if (clickedScroll) {
			clickedScroll = false;
			return;
		}
		if (lastActive) {
			lastActive.classList.remove('active');
		}
		for (const [i, position] of sectionPositions.entries()) {
			if (position >= mainContent.scrollTop) {
				const link = toc.querySelector('a[href="#' + sections[i].id + '"]');
				if (link) {
					// Set current menu link as active
					link.classList.add('active');
					const tocStart = toc.getBoundingClientRect().top + window.scrollY;
					if (link.offsetTop > toc.scrollTop + toc.clientHeight - tocStart - 16) {
						// Scroll the toc down if the active links position is below the bottom of the toc
						toc.scrollTop = link.clientHeight + link.offsetTop - toc.clientHeight + tocStart + 10;
					} else if (toc.scrollTop < 32 + tocStart) {
						// Scroll to the top of the toc if having scrolled up into the last bit
						toc.scrollTop = 0;
					} else if (link.offsetTop < toc.scrollTop) {
						// Scroll the toc up if the active links position is above the top of the toc
						toc.scrollTop = link.offsetTop - 10;
					}
				}
				lastActive = link;
				break;
			}
		}
	}, 10);
	mainContent.addEventListener('scroll', handleScroll);
	toc.querySelectorAll('a').forEach((a) =>
		a.addEventListener('click', () => {
			if (lastActive) {
				lastActive.classList.remove('active');
			}
			a.classList.add('active');
			lastActive = a;
			clickedScroll = true;
		}),
	);
}

function setupMobileToggle() {
	document.getElementById('toggle-menu').addEventListener('click', () => {
		const docNav = document.querySelector('header.doc-nav');
		const isHidden = docNav.classList.contains('hidden');
		docNav.classList.toggle('hidden');
		const search = docNav.querySelector('.search');
		const searchHasResults = search.classList.contains('has-results');
		if (isHidden && searchHasResults) {
			search.classList.remove('mobile-hidden');
		} else {
			search.classList.add('mobile-hidden');
		}
		const content = docNav.querySelector('.content');
		content.classList.toggle('hidden');
		content.classList.toggle('show');
	});
}

function setupDarkMode() {
	const html = document.querySelector('html');
	const darkModeToggle = document.getElementById('dark-mode-toggle');
	darkModeToggle.addEventListener('click', () => {
		html.classList.toggle('dark');
		const isDarkModeEnabled = html.classList.contains('dark');
		localStorage.setItem('dark-mode', isDarkModeEnabled);
		darkModeToggle.setAttribute('aria-checked', isDarkModeEnabled);
	});
}

function setupSearch() {
	const onInputChange = debounce((e) => {
		const searchValue = e.target.value.toLowerCase();
		const docNav = document.querySelector('header.doc-nav');
		const menu = docNav.querySelector('.content');
		const search = docNav.querySelector('.search');
		if (searchValue === '') {
			// reset to default
			menu.style.display = '';
			if (!search.classList.contains('hidden')) {
				search.classList.add('hidden');
				search.classList.remove('has-results');
			}
		} else if (searchValue.length >= 2) {
			// search for less than 2 characters can display too much results
			search.innerHTML = '';
			menu.style.display = 'none';
			if (search.classList.contains('hidden')) {
				search.classList.remove('hidden');
				search.classList.remove('mobile-hidden');
				search.classList.add('has-results');
			}
			// cache length for performance
			let foundModule = false;
			const ul = document.createElement('ul');
			search.appendChild(ul);
			for (const [i, title] of searchModuleIndex.entries()) {
				// no toLowerCase needed because modules are always lowercase
				if (title.indexOf(searchValue) === -1) {
					continue;
				}
				foundModule = true;
				// [description, link]
				const data = searchModuleData[i];
				const el = createSearchResult({
					badge: 'module',
					description: data[0],
					link: data[1],
					title: title,
				});
				ul.appendChild(el);
			}
			if (foundModule) {
				const hr = document.createElement('hr');
				hr.classList.add('separator');
				search.appendChild(hr);
			}
			let results = [];
			for (const [i, title] of searchIndex.entries()) {
				if (title.toLowerCase().indexOf(searchValue) === -1) {
					continue;
				}
				// [badge, description, link]
				const data = searchData[i];
				results.push({
					badge: data[0],
					description: data[1],
					link: data[2],
					title: data[3] + ' ' + title,
				});
			}
			results.sort((a, b) => (a.title < b.title ? -1 : a.title > b.title ? 1 : 0));
			const ul_ = document.createElement('ul');
			search.appendChild(ul_);
			results.forEach((result) => {
				const el = createSearchResult(result);
				ul_.appendChild(el);
			});
		}
	});
	const searchInput = document.querySelector('#search input');
	const url = document.location.toString();
	if (url.includes('?')) {
		const query =
			url
				.split('?')
				.slice(1)
				.filter((p) => p.startsWith('q='))
				.map((p) => p.replace(/^q=/, ''))[0] || '';
		if (query) {
			searchInput.value = query;
			searchInput.focus();
			onInputChange({ target: { value: query } });
		}
	}
	const searchInputDiv = document.getElementById('search');
	searchInputDiv.addEventListener('input', onInputChange);
	setupSearchKeymaps();
}

function setupSearchKeymaps() {
	const searchInput = document.querySelector('#search input');
	const mainContent = document.querySelector('#main-content');
	const docnav = document.querySelector('header.doc-nav');
	// Keyboard shortcut indicator
	const searchKeys = document.createElement('div');
	const modifierKeyPrefix = navigator.platform.includes('Mac') ? '⌘' : 'Ctrl';
	searchKeys.setAttribute('id', 'search-keys');
	searchKeys.innerHTML = '<kbd>' + modifierKeyPrefix + '</kbd><kbd>k</kbd>';
	searchInput.parentElement?.appendChild(searchKeys);
	searchInput.addEventListener('focus', () => searchKeys.classList.add('hide'));
	searchInput.addEventListener('blur', () => searchKeys.classList.remove('hide'));
	// Global shortcuts to focus searchInput
	document.addEventListener('keydown', (ev) => {
		if (ev.key === '/' || ((ev.ctrlKey || ev.metaKey) && ev.key === 'k')) {
			ev.preventDefault();
			searchInput.focus();
		}
	});
	// Shortcuts while searchInput is focused
	let selectedIdx = -1;
	function selectResult(results, newIdx) {
		if (selectedIdx !== -1) {
			results[selectedIdx].classList.remove('selected');
		}
		results[newIdx].classList.add('selected');
		results[newIdx].scrollIntoView({ behavior: 'instant', block: 'nearest', inline: 'nearest' });
		selectedIdx = newIdx;
	}
	searchInput.addEventListener('keydown', (ev) => {
		const searchResults = document.querySelectorAll('.search .result');
		switch (ev.key) {
			case 'Escape':
				searchInput.blur();
				mainContent.focus();
				break;
			case 'Enter':
				if (!searchResults.length || selectedIdx === -1) break;
				searchResults[selectedIdx].querySelector('a').click();
				break;
			case 'ArrowDown':
				ev.preventDefault();
				if (!searchResults.length) break;
				if (selectedIdx >= searchResults.length - 1) {
					// Cycle to first if last is selected
					selectResult(searchResults, 0);
				} else {
					// Select next
					selectResult(searchResults, selectedIdx + 1);
				}
				break;
			case 'ArrowUp':
				ev.preventDefault();
				if (!searchResults.length) break;
				if (selectedIdx <= 0) {
					// Cycle to last if first is selected (or select it if none is selected yet)
					selectResult(searchResults, searchResults.length - 1);
				} else {
					// Select previous
					selectResult(searchResults, selectedIdx - 1);
				}
				break;
			default:
				docnav.scroll(0, 0);
				selectedIdx = -1;
		}
	});
	// Ensure initial keyboard navigability
	mainContent.focus();
}

function createSearchResult(data) {
	const li = document.createElement('li');
	li.classList.add('result');
	const a = document.createElement('a');
	a.href = data.link;
	a.classList.add('link');
	li.appendChild(a);
	const definition = document.createElement('div');
	definition.classList.add('definition');
	a.appendChild(definition);
	if (data.description) {
		const description = document.createElement('div');
		description.classList.add('description');
		description.textContent = data.description;
		a.appendChild(description);
	}
	const title = document.createElement('span');
	title.classList.add('title');
	title.textContent = data.title;
	definition.appendChild(title);
	const badge = document.createElement('badge');
	badge.classList.add('badge');
	badge.textContent = data.badge;
	definition.appendChild(badge);
	return li;
}

function setupCollapse() {
	const dropdownArrows = document.querySelectorAll('.dropdown-arrow');
	dropdownArrows.forEach((arrow) => {
		arrow.addEventListener('click', (e) => {
			const parent = e.target.parentElement.parentElement.parentElement;
			parent.classList.toggle('open');
		});
	});
}

function debounce(func, timeout) {
	let timer;
	return (...args) => {
		const next = () => func(...args);
		if (timer) {
			clearTimeout(timer);
		}
		timer = setTimeout(next, timeout > 0 ? timeout : 300);
	};
}

function setupCodeCopy() {
	const pres = document.querySelectorAll('pre:not(.signature)');
	pres.forEach((pre) => {
		const tempDiv = document.createElement('button');
		tempDiv.className = 'copy';
		tempDiv.innerHTML =
			'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="16" height="16" fill="rgba(173,184,194,1)"><path d="M6.9998 6V3C6.9998 2.44772 7.44752 2 7.9998 2H19.9998C20.5521 2 20.9998 2.44772 20.9998 3V17C20.9998 17.5523 20.5521 18 19.9998 18H16.9998V20.9991C16.9998 21.5519 16.5499 22 15.993 22H4.00666C3.45059 22 3 21.5554 3 20.9991L3.0026 7.00087C3.0027 6.44811 3.45264 6 4.00942 6H6.9998ZM5.00242 8L5.00019 20H14.9998V8H5.00242ZM8.9998 6H16.9998V16H18.9998V4H8.9998V6Z"></path></svg>';
		tempDiv.addEventListener('click', (e) => {
			const parent = e.target;
			var code = tempDiv.parentElement.querySelector('code');
			let i = Array.from(code.childNodes)
				.map((r) => r.textContent)
				.join('');
			navigator.clipboard.writeText(i);
			var tmp = tempDiv.innerHTML;
			tempDiv.innerHTML = 'Copied';
			window.setTimeout(function () {
				tempDiv.innerHTML = tmp;
			}, 1000);
		});
		pre.insertAdjacentElement('afterbegin', tempDiv);
	});
}
