const Application = require("spectron").Application;
const path = require("path");
const chai = require("chai");
const { expect } = require('chai');
const chaiAsPromised = require("chai-as-promised");

const AppToggleTrackedCommand = require("../../../logicLayer/commands/application/AppToggleTrackedCommand")
const ApplicationRDG = require("../../../dal/gateways/application/ApplicationRDG")
const SchemaBuilder = require("../../../dal/database/schemaBuilder/SchemaBuilder")

/**
 * Rebuild database before running tests.
 */
global.beforeEach(() => {
    // Rebuild database
    SchemaBuilder.buildUp()
});

global.afterEach(() => {
    SchemaBuilder.tearDown()
})

describe('Test AppToggleTrackedCommand', () => {
    it('Tests Switching to BlackListed', () => {
        // Insert data into database
        let create = new ApplicationRDG();
        create.create("windows", "edge",
            "Google", 0)

        // Read in entry
        let check = new ApplicationRDG();
        let id = check.has("windows", "edge",
            "Google")
        check.read(id)
        expect(check.isBlacklisted).to.equal(0);
        //let comm = new AppToggleTrackedCommand;
        AppToggleTrackedCommand.execute(check.id);
        let checkTwo = new ApplicationRDG();
        checkTwo.read(check.id);
        expect(checkTwo.isBlacklisted).to.equal(1);
    })

    it('Tests Switching from Blacklisted', () => {
        // Insert data into database
        let create = new ApplicationRDG();
        create.create("windows", "edge",
            "Google", 1)

        // Read in entry
        let check = new ApplicationRDG();
        let id = check.has("windows", "edge",
            "Google")
        check.read(id)
        expect(check.isBlacklisted).to.equal(1);
        //let comm = new AppToggleTrackedCommand;
        AppToggleTrackedCommand.execute(check.id);
        let checkTwo = new ApplicationRDG();
        checkTwo.read(check.id);
        expect(checkTwo.isBlacklisted).to.equal(0);
    })
})