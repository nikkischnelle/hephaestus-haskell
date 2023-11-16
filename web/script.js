let directoryList;
let collapsibleList;

document.addEventListener("DOMContentLoaded", function(){
    directoryList = document.getElementsByClassName("collapsible_content");
    collapsibleList = document.getElementsByClassName("collapsible");

    const storedStyles = JSON.parse(localStorage.getItem('collapsibleStates'));
    if (storedStyles != null) {
        for (let i = 0; i < directoryList.length; i++) {
            const element = directoryList[i];
            element.style.display = storedStyles[i];
        }
    }

    const iconStyles = JSON.parse(localStorage.getItem('iconStates'));
    if (iconStyles != null) {
        for (let i = 0; i < collapsibleList.length; i++) {
            const element = collapsibleList[i];
            element.innerHTML = iconStyles[i];
        }
    }


    for (var i = 0; i < collapsibleList.length; i++) {
        collapsibleList[i].addEventListener("click", function() {
            var content = this.nextElementSibling;
            var icon = this.children[0]

            if (content.style.display === "block") {
                content.style.display = "none";
                icon.innerHTML = "folder"
            } else {
                content.style.display = "block";
                icon.innerHTML = "folder_open"
            }

            saveCollapsibleStates();
            saveIconStates();
        });
    } 
});

function redirect(url) {
    location.replace(url)
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
        iconStateList[i] = element.innerHTML;
    }
    localStorage.setItem('iconStates', JSON.stringify(iconStateList));
}