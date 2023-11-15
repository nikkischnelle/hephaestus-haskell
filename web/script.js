let directoryList;

document.addEventListener("DOMContentLoaded", function(){
    directoryList = document.getElementsByClassName("collapsible_content");
    const storedStyles = JSON.parse(localStorage.getItem('directoryStates'));
    if (storedStyles != null) {
        for (let i = 0; i < directoryList.length; i++) {
            const element = directoryList[i];
            element.style.display = storedStyles[i];
        }
    }

    var coll = document.getElementsByClassName("collapsible");
    var i;

    for (i = 0; i < coll.length; i++) {
        coll[i].addEventListener("click", function() {
            var content = this.nextElementSibling;
            if (content.style.display === "block") {
                content.style.display = "none";
            } else {
                content.style.display = "block";
            }
            saveDirectoryStates();
        });
    } 
});

function redirect(url) {
    location.replace(url)
}

function saveDirectoryStates() {
    let styleList = new Array();
    for (let i = 0; i < directoryList.length; i++) {
        const element = directoryList[i];
        styleList[i] = element.style.display;
    }
    localStorage.setItem('directoryStates', JSON.stringify(styleList));
}