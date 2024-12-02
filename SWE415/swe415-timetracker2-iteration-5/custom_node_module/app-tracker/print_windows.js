const app_tracker = require('bindings')('app-tracker.node')

/**
 * If user is on a mac, ask for permission to view the screen.
 */
if (process.platform === "darwin") {
    const permissions = require('node-mac-permissions')
    status = permissions.getAuthStatus('screen')
    console.log(`Access to Screen is ${status}`)
    if (status !== "authorized")
    {
        permissions.askForScreenCaptureAccess();
    }
}

/**
 * Get all the apps on the system and return them.
 */
app_tracker.fetchDesktopWindows()
    .then(windows => {
        windows.forEach(window => {
            console.log(window)
        })
    })
    .catch(e => {
        console.log('received error: ' + e);
    })
