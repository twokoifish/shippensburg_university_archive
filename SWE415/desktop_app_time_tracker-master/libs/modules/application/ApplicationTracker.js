'use strict';


/**
 * Import 'electron-active-window' as 'ActiveWindow'
 * @type {function(): ({getActiveWindow: function(): Promise<unknown> | Promise<unknown>})}
 */
const ActiveWindow = require('electron-active-window')

/**
 * Import 'Application' as 'Application'
 * @type {Application|{}}
 */
const Application = require('./Application')

/**
 * Import 'ApplicationMapper' as 'ApplicationMapper'
 * @type {_ApplicationMapper}
 */
const ApplicationMapper = require('./ApplicationMapper')

/**
 * State enum for the state machine in _ApplicationTracker
 * @type {Readonly<{PAUSED: number, STOPPED: number, STARTED: number}>}
 */
const state = Object.freeze({
    "STOPPED": 0,
    "STARTED": 1,
    "PAUSED": 2
})

/**
 * _ApplicationTracker is an object designed to track active processes on a computer.
 */
class _ApplicationTracker {

    /**
     * No need for constructor unless toggling debug mode.
     */
    // constructor() {
    //     this.#toggleDebug()
    // }

    /**
     * Tracks the active window of the computer.
     */
    #track() {

        // If _ApplicationTracker is STARTED
        if (_ApplicationTracker.currentState === state.STARTED) {

            if (_ApplicationTracker.debug) console.log("tracking")

            // Get the active window on the computer
            ActiveWindow().getActiveWindow().then((active) => {

                if (_ApplicationTracker.debug) console.log("getting active window " + active.windowName)

                // Check to see if the active window is already tracked.
                let exist = ApplicationMapper.has(active.windowName)

                // If not, start tracking it.
                if (!exist) {
                    if (_ApplicationTracker.debug) console.log("db does not have " + active.windowName)
                    ApplicationMapper.create();

                // If it is already tracked, fetch it and update its information.
                } else {
                    if (_ApplicationTracker.debug) console.log("map has " + active.windowName)
                    let common = ApplicationMapper.readCommon(active.windowName)
                    if (common instanceof Application) {
                        // more stuff....
                    }
                }
            })
        }

        if (_ApplicationTracker.debug) console.log("not tracking")
    }

    /**
     * Start the tracker if the state machine is STARTED.
     */
    start() {

        // If _ApplicationTracker is STOPPED
        if (_ApplicationTracker.currentState === state.STOPPED) {

            if (_ApplicationTracker.debug) console.log("starting")

            // Start the interval on track()
            _ApplicationTracker.interval = setInterval(this.#track, 1000)

            // Change _ApplicationTracker to STARTED
            _ApplicationTracker.currentState = state.STARTED
        }

        // If _ApplicationTracker is PAUSED
        else if (_ApplicationTracker.currentState === state.PAUSED) {

            if (_ApplicationTracker.debug) console.log("starting")

            // Change it to STARTED
            _ApplicationTracker.currentState = state.STARTED
        }
    }

    /**
     * Stop the tracker if the state machine is STOPPED.
     */
    stop() {

        // If _ApplicationTracker is any state expect STOPPED
        if (_ApplicationTracker.currentState !== state.STOPPED) {

            if (_ApplicationTracker.debug) console.log("stopping")

            // Change it to STOPPED
            _ApplicationTracker.currentState = state.STOPPED

            // Clear the interval on track()
            clearInterval(_ApplicationTracker.interval)
        }
    }

    /**
     * Pause the tracker if the state machine is PAUSED.
     */
    pause() {

        // If _ApplicationTracker is STARTED
        if (_ApplicationTracker.currentState === state.STARTED) {

            if (_ApplicationTracker.debug) console.log("pausing")

            // Change it to PAUSED
            _ApplicationTracker.currentState = state.PAUSED
        }
    }

    /**
     * Private function to toggle debug mode in the program.
     */
    #toggleDebug() {
        _ApplicationTracker.debug = !_ApplicationTracker.debug
    }
}

/**
 * Allows us to define writable properties for a frozen class.
 */
Object.defineProperties(_ApplicationTracker, {

    /**
     * Holds the current state of the machine.
     */
    currentState: {
        // Set the default value of 'currentState'
        value: state.STOPPED,

        // Allow the value of 'currentState' to be changed
        writable: true
    },

    /**
     * Holds the interval for the track function.
     */
    interval: {
        // Set the default value of 'interval'
        value: null,

        // Allow the value of 'interval' to be changed
        writable: true
    },

    /**
     * Holds the debug state for the tracker.
     */
    debug: {
        // Set the default value of 'debug'
        value: Boolean(false),

        // Allow the value of 'debug' to be changed
        writable: true
    }
})

// Create an accessible version of _ApplicationTracker
let ApplicationTracker = new _ApplicationTracker()

// Freeze ApplicationTracker to prevent modifications
Object.freeze(ApplicationTracker)

// Export ApplicationTracker for use in other classes.
module.exports = ApplicationTracker