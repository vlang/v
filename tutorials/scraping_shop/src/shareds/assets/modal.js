var spans = document.getElementsByClassName("close-modal");

document.querySelectorAll('[class^="openModal-"]').forEach(function (button) {
	button.onclick = function () {
		var targetModalId = this.getAttribute('target-modal');
		var targetModal = document.getElementById(targetModalId);
		if (targetModal) {
			targetModal.style.display = "block";
		}
	}
});

function addModalEventListeners() {
	var spans = document.getElementsByClassName("close-modal");

	Array.from(spans).forEach(function (span) {
		span.onclick = function () {
			var modal = this.closest('.modal');
			if (modal) {
				modal.removeAttribute('open');
				modal.style.display = "none";
			}
		}
	});
}

window.onclick = function (event) {
	if (event.target.classList.contains('modal') || event.target.classList.contains('close-modal')) {
		var modal = event.target.classList.contains('modal') ? event.target : event.target.closest('.modal');
		if (modal) {
			modal.removeAttribute('open');
			modal.style.display = "none";
		}
	}
}