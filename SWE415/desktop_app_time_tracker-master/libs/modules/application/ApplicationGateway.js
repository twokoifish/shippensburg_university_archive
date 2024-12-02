'use strict';

const Application = require('./Application')

/**
 * Defined using CommonJS standard.
 */
class _ApplicationGateway {

    constructor() {
        console.log("Making the gateway")
    }

    /**
     * Create a new entry in the database for the passed in node_modules.
     * returns an ID.
     */
    create() {

    }

    read(appID) {
        if (appID instanceof Number && Number.isInteger(appID)) {

        } else {
            throw ("Type mismatch! AppID must be an instance of Number.\n")
        }
    }

    update() {

    }

    delete() {

    }

    stash() {

    }

    has() {

    }

}

const ApplicationGateway = new _ApplicationGateway()
module.exports.ApplicationGateway = ApplicationGateway