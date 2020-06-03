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
