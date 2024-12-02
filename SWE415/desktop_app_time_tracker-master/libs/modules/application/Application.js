'use strict';

class Application {

    os
    windowClass
    windowName
    startTime
    endTime
    idleTime

    constructor(promise) {
        this.os = promise.os
        this.windowClass = promise.windowClass
        this.windowName = promise.windowName
        this.startTime = new Date().getMilliseconds()
    }

    get os () {
        return this.os
    }

    get windowClass () {
        return this.windowClass
    }

    get windowName () {
        return this.windowName
    }

    get startTime() {
        return this.startTime
    }

    get endTime () {
        return this.endTime
    }

    set endTime (time) {
        this.endTime = time
    }

    get idleTime() {
        return this.idleTime
    }

    set idleTime (time) {
        this.idleTime = time
    }

}

module.exports = Application