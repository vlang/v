try {
	const themeToggle = document.getElementById('theme-toggle');
	const toggleIcon = themeToggle.querySelector('img');

	function setTheme(mode) {
		if (mode === 'dark') {
			document.body.classList.add('dark-mode');
			toggleIcon.src = '/assets/icons/toggle-dark-svgrepo-com.svg';
		} else {
			document.body.classList.remove('dark-mode');
			toggleIcon.src = '/assets/icons/toggle-light-svgrepo-com.svg';
		}
		localStorage.setItem('theme', mode);
	}

	function toggleTheme() {
		const isDarkMode = document.body.classList.contains('dark-mode');
		setTheme(isDarkMode ? 'light' : 'dark');
	}

	const savedTheme = localStorage.getItem('theme') || 'light';
	setTheme(savedTheme);

	themeToggle.addEventListener('click', toggleTheme);
} catch (e) {
	console.error("Error:", e);
}