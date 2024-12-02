// const ApplicationTracker = require("./libs/application/ApplicationTracker")
window.addEventListener('DOMContentLoaded', () => {

    function hide() {
        let element = document.getElementById("main")
        if (element.style.display === "none") {
            element.style.display = "block"
        } else {
            element.style.display = "none"
        }
    }

    let x = document.getElementsByClassName("main").item(0)
    x.addEventListener('click', function () {
        hide()
    })
})