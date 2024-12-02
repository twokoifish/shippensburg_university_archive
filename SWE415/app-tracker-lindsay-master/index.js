const app_tracker = require('bindings')('app-tracker.node')


if (process.platform === "darwin") {
  const permissions = require('node-mac-permissions')
  let status = permissions.getAuthStatus('screen')
  console.log(`Access to Screen is ${status}`)
  if (status !== "authorized")
  {
    permissions.askForScreenCaptureAccess();
  }
}

module.exports = {
  fetchDesktopWindows: app_tracker.fetchDesktopWindows,
}