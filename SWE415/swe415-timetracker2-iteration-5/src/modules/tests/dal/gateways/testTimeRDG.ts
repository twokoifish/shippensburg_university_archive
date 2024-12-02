import SchemaBuilder from "../../../dal/database/schemaBuilder/SchemaBuilder";
import DatabaseManager from "../../../dal/database/manager/DatabaseManager";
import ApplicationRDG from "../../../dal/gateways/application/ApplicationRDG";
import TimeRDG from "../../../dal/gateways/time/TimeRDG";
import chai = require("chai");
import {DesktopWindow} from "app-tracker";

const expect = chai.expect;
const schema : SchemaBuilder = SchemaBuilder.getInstance();
const dbm : DatabaseManager = DatabaseManager.getInstance();

/**
 * Rebuild database before running tests.
 */
global.beforeEach(() => {
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
describe('Test TimeRDG', () => {
  it('Test Create/Reading Time', () => {
    // Create an application, Insert it into the databaseInsert into database
    let dw: DesktopWindow = {titleBar: "Google", name: "msedge.exe",
      isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false} as DesktopWindow;
    let app: ApplicationRDG = new ApplicationRDG()
    app.create(dw)
    let appID:number = app.has("msedge.exe", "Google")
    // let appID:number = app.id
    console.log(appID)

    let create: TimeRDG = new TimeRDG()
    create.create(appID, 10000, 11000,  false, false)

    // Read in entry using a new gateway
    let check : TimeRDG = new TimeRDG()
    check.read(appID, 10000, 11000)

    expect(check.id).to.equal(appID) // Runs after TDG test, where we insert 7, so this id should be 8.
    expect(check.start).to.equal(10000)
    expect(check.end).to.equal(11000)
    expect(check.wasIdle).to.equal(false)
  })

  it('Test Deleting TimeRDG', () => {
    let dw: DesktopWindow = {titleBar: "Google", name: "msedge.exe",
      isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false};
    let applicationRDG : ApplicationRDG = new ApplicationRDG();
    let appID : number = applicationRDG.getHash(dw);

    // Insert data into database
    let create : TimeRDG = new TimeRDG();
    create.create(appID, 10000, 11000, false, false); // Create database entry

    // Delete entry
    let del : TimeRDG = new TimeRDG();
    del.read(appID, 10000, 11000)
    del.delete()

    // Now, we should not be able to find the entry
    // @TODO add has to TimeRDG
    let check : TimeRDG = new TimeRDG()
    check.read(appID, 10000, 11000)
    expect(status).to.equal(false)
  });

  it('Test Updating TimeRDG', () => {
    let dw: DesktopWindow = {titleBar: "Google", name: "msedge.exe",
      isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false};
    let applicationRDG : ApplicationRDG = new ApplicationRDG();
    let appID : number = applicationRDG.getHash(dw);

    // Insert data into database
    let create : TimeRDG = new TimeRDG();
    create.create(appID, 10000, 11000, false, false); // Create database entry

    let time: TimeRDG = new TimeRDG();
    time.read(appID, 10000, 11000);

    // Update information
    time.start = 5555
    time.end = 7777
    time.wasIdle = true
    // Persist to database
    time.update()

    // Fresh ApplicationRDG to fetch from database
    let fetch : TimeRDG= new TimeRDG()
    fetch.read(appID, 5555, 7777)

    expect(fetch.start).to.equal(5555)
    expect(fetch.end).to.equal(7777)
    expect(fetch.wasIdle).to.equal(true)
  });
  //
  // it('Test Has ApplicationRDG', () => {
  //   // Insert data into database
  //   let create : ApplicationRDG = new ApplicationRDG();
  //   create.create("windows", "edge", "Google", false); // Create database entry
  //
  //   // Check for an app that does not exist
  //   let has: ApplicationRDG = new ApplicationRDG()
  //   let id: number = has.has("linux", "chrome", "Bing")
  //   expect(id).to.equal(-1)
  //
  //   // Check for an app that does exist
  //   let id2 : number = has.has("windows", "edge", "Google")
  //   expect(id2).to.equal(8) // Runs after TDG test, where we insert 7, so this id should be 8.
  //
  // });
})
