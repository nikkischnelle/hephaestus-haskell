let directoryList;
let collapsibleList;
let lightMode;

document.addEventListener("DOMContentLoaded", function() {
    try {
        applyLocalStorageSettings();
    } catch (error) {
        console.log("Error applying local storage settings. Local Storage will be deleted.");
        console.log(error);
        localStorage.clear();
        location.reload();
    }
});

function applyLocalStorageSettings() {
    lightMode = JSON.parse(localStorage.getItem('lightMode')) ?? false;
    if (lightMode) {
        document.body.classList.toggle("light-mode");
    }

    directoryList = document.getElementsByClassName("collapsible_content");
    collapsibleList = document.getElementsByClassName("collapsible");

    const storedStyles = JSON.parse(localStorage.getItem('collapsibleStates'));
    if (storedStyles != null) {
        for (let i = 0; i < directoryList.length; i++) {
            const element = directoryList[i];
            element.style.display = storedStyles[i];
        }
    }

    const iconStyles = JSON.parse(localStorage.getItem('allIconStates'));
    if (iconStyles != null) {
        for (let i = 0; i < collapsibleList.length; i++) {
            const entry = collapsibleList[i];
            const state = iconStyles[i];

            for (let j = 0; j < entry.children.length; j++) {
                const element = entry.children[j];
                element.className = state[j];
            }
        }
    }

    for (var i = 0; i < collapsibleList.length; i++) {
        collapsibleList[i].addEventListener("click", function() {
            var content = this.nextElementSibling;
            var arrowIcon = this.children[0];
            var folderIcon = this.children[1];

            folderIcon.classList.toggle("nf-cod-folder");
            folderIcon.classList.toggle("nf-cod-folder_opened");

            arrowIcon.classList.toggle("nf-oct-plus");
            arrowIcon.classList.toggle("nf-md-minus");

            if (content.style.display === "block") {
                content.style.display = "none";
            } else {
                content.style.display = "block";
            }

            this.classList.toggle("active")
            saveCollapsibleStates();
            saveIconStates();
        });
    } 
}

function redirect(url) {
    location.replace(url)
}

function redirectToView(url) {
    location.replace("/view" + url)
}

function saveCollapsibleStates() {
    let styleList = new Array();
    for (let i = 0; i < directoryList.length; i++) {
        const element = directoryList[i];
        styleList[i] = element.style.display;
    }
    localStorage.setItem('collapsibleStates', JSON.stringify(styleList));
}

function saveIconStates() {
    let iconStateList = new Array();
    for (let i = 0; i < collapsibleList.length; i++) {
        const element = collapsibleList[i];
        let tempArr = new Array()
        for (let j = 0; j < element.children.length; j++) {
            const child = element.children[j];
            tempArr.push(child.className);
        }
        iconStateList[i] = tempArr;
    }
    console.log(iconStateList)

    localStorage.setItem('allIconStates', JSON.stringify(iconStateList));
}

function toggleLightMode() {
    document.body.classList.toggle("light-mode");
    lightMode = !lightMode
    localStorage.setItem('lightMode', JSON.stringify(lightMode))
}