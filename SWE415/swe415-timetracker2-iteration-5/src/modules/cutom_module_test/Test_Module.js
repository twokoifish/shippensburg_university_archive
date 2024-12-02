const tracker = require("app-tracker");

tracker.fetchDesktopWindows().then(promise => {
  console.log(promise);
}).catch(error => {
  console.log(error);
})