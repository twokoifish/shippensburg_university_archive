const Application = require("spectron").Application;
const path = require("path");
const chai = require("chai");
const { expect } = require('chai');
const chaiAsPromised = require("chai-as-promised");

const AppGetAllCommand = require("../../../logicLayer/commands/application/AppGetAllCommand")
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

describe("Testing GetAllApps", () => {
    it("Gets all Apps", () => {
        let test = AppGetAllCommand.execute()
        expect(test).to.have.lengthOf(7)
        let create = new ApplicationRDG();
        create.create("windows", "edge", "Google", 0)
        let test2 = AppGetAllCommand.execute()
        expect(test2).to.have.lengthOf(8)
    })

})