(function() {
    var html = document.getElementsByTagName('html')[0];
    if (localStorage.getItem('dark-mode') === 'true') {
        html.classList.add('dark');
    }
})();
