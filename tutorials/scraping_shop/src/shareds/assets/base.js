document.addEventListener("DOMContentLoaded", () => {
    const posHtmlElements = document.getElementsByTagName('pos-html');

    Array.from(posHtmlElements).forEach(element => {
        try {
            const htmlString = element.textContent.trim();
            const decodedHtml = new DOMParser().parseFromString(htmlString, 'text/html').body.firstChild;

            if (decodedHtml) {
                element.parentNode.replaceChild(decodedHtml, element);
            }
        } catch (e) {
            console.error("Erro ao transformar o conteúdo em HTML:", e);
        }
    });
});