import SchemaBuilder from "../../../dal/database/schemaBuilder/SchemaBuilder";
import ApplicationRDG from "../../../dal/gateways/application/ApplicationRDG";
import {DesktopWindow} from "app-tracker";
import chai = require("chai");

const expect = chai.expect;
const schema : SchemaBuilder = SchemaBuilder.getInstance();

/**
 * Rebuild database before running tests.
 */
global.beforeEach(() => {
  // schema.tearDown()
  schema.buildUp()
})

/**
 * Delete database after testing.
 */
global.afterEach(() => {
  schema.tearDown()
})

/**
 * Test Application RDG
 */
describe('Test ApplicationRDG', () => {
  it('Test Create/Reading Application', () => {
    // Create an application, insert it into the database
    let dw: DesktopWindow = {titleBar: "Google", name: "msedge.exe",
      isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false} as DesktopWindow;
    let appRDG : ApplicationRDG = new ApplicationRDG();
    appRDG.create(dw)
    let appID : number = appRDG.getHash(dw);

    let check : ApplicationRDG = new ApplicationRDG()
    check.read(appID)

    expect(check.id).to.equal(appID) // Runs after TDG test, where we insert 7, so this id should be 8.
    expect(check.windowClass).to.equal("msedge.exe")
    expect(check.windowName).to.equal("Google")
    expect(check.isBlacklisted).to.equal(false)
  })

  it('Test Deleting ApplicationRDG', () => {
    let dw: DesktopWindow = {titleBar: "Discord", name: "Discord.exe",
      isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false};
    let appRDG : ApplicationRDG = new ApplicationRDG();
    appRDG.create(dw);
    let appID : number = appRDG.getHash(dw);

    // Delete it
    let del : ApplicationRDG = new ApplicationRDG(appID)
    del.delete()

    // Check the DB again, and we shouldn't be able to find it
    let check : ApplicationRDG = new ApplicationRDG()
    let checkID : number = check.has("Discord.exe", "Discord")

    expect(checkID).to.equal(-1)
  });

  /**
   * TODO: Change class and name. haven't done yet because the id will change it it will
   * need to delete the old version.
   */
  it('Test Updating ApplicationRDG', () => {
    // Create an application, insert it into the database
    let dw: DesktopWindow = {titleBar: "Google", name: "msedge.exe",
      isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false};
    let appRDG : ApplicationRDG = new ApplicationRDG();
    appRDG.create(dw);
    let appID : number = appRDG.getHash(dw);

    let has: ApplicationRDG = new ApplicationRDG();
    has.read(appID)
    // Update information
    has.isBlacklisted = false
    // Persist to database
    has.update()

    // Fresh ApplicationRDG to fetch from database
    let fetch : ApplicationRDG = new ApplicationRDG()
    fetch.read(appID)

    expect(fetch.isBlacklisted).to.equal(false)
  });

  it('Test Has ApplicationRDG', () => {
    // Create an application, insert it into the database
    let dw: DesktopWindow = {titleBar: "Google", name: "msedge.exe",
      isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false};
    let appRDG : ApplicationRDG = new ApplicationRDG();
    appRDG.create(dw);
    let appID : number = appRDG.getHash(dw);

    // Check for an app that does not exist
    let has: ApplicationRDG = new ApplicationRDG()
    let id: number = has.has("chrome.exe", "Bing")
    expect(id).to.equal(-1)

    // Check for an app that does exist
    let id2 : number = has.has("msedge.exe", "Google")
    expect(id2).to.equal(appID) // Runs after TDG test, where we insert 7, so this id should be 8.
  });

  it('Test genHash abc xyz', () => {
    // test 1 abc xyz
    let dw : DesktopWindow = {titleBar: "abc", name: "xyz", isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false};
    let applicationRDG : ApplicationRDG = new ApplicationRDG();
    let pp : number = applicationRDG.getHash(dw);
    console.log(pp);
  })
  it('Test genHash xyz abc', () => {
    // test 1 xyz abc
    let dw : DesktopWindow = {titleBar: "xyz", name: "abc", isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false};
    let applicationRDG : ApplicationRDG = new ApplicationRDG();
    let pp : number = applicationRDG.getHash(dw);
    console.log(pp);
  })
  it('Test genHash b z', () => {
    // test 1 b z
    let dw : DesktopWindow = {titleBar: "b", name: "z", isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false};
    let applicationRDG : ApplicationRDG = new ApplicationRDG();
    let pp : number = applicationRDG.getHash(dw);
    console.log(pp);
  })

  it('Test realistic getHash', () => {
    //
    let dw: DesktopWindow = {titleBar: "TimeTracker - General", name: "discord.exe", isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false};
    let applicationRDG : ApplicationRDG = new ApplicationRDG();
    let pp : number = applicationRDG.getHash(dw);
    console.log(pp);
  })

  it('Test realistic getHash same length', () => {
    //
    let dw: DesktopWindow = {titleBar: "SWE 415 - General Dis", name: "spotify.exe", isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false};
    let applicationRDG : ApplicationRDG = new ApplicationRDG();
    let pp : number = applicationRDG.getHash(dw);
    console.log(pp);
  })
})
