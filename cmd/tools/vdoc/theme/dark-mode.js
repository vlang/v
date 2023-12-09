(function () {
	if (localStorage.getItem('dark-mode') === 'true') {
		document.querySelector('html').classList.add('dark');
	}
})();
