import SchemaBuilder from "../../../dal/database/schemaBuilder/SchemaBuilder";
import DatabaseManager from "../../../dal/database/manager/DatabaseManager";
import ApplicationRDG from "../../../dal/gateways/application/ApplicationRDG";
import TimeRDG from "../../../dal/gateways/time/TimeRDG";
import chai = require("chai");
import {DesktopWindow} from "app-tracker";
import {TimeTDG} from "../../../dal/gateways/time/TimeTDG";
import TimeDTO from "../../../dal/gateways/time/TimeDTO";

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
 * Test Application TDG
 */
describe('Test TimeTDG', () => {
  it('Get by ID', () => {
    // Create an application, Insert it into the databaseInsert into database
    let dw: DesktopWindow = {titleBar: "Google", name: "msedge.exe",
      isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false} as DesktopWindow;
    let app: ApplicationRDG = new ApplicationRDG()
    app.create(dw)
    let appID:number = app.has("msedge.exe", "Google")
    // let appID:number = app.id
    console.log(appID)

    let createTime: TimeRDG = new TimeRDG()
    createTime.create(appID, 10000, 11000,  false, false)
    createTime.create(appID, 12000, 13000,  false, false)


    let create: TimeTDG = TimeTDG.getInstance()
    let timeList: TimeDTO[] = create.getById(appID)


    expect(timeList[0].start).to.equal(10000)
    expect(timeList[0].end).to.equal(11000)
    expect(timeList[1].start).to.equal(12000)
    expect(timeList[1].end).to.equal(13000)

  })

  it('Get by WasIdle', () => {
    // Create an application, Insert it into the databaseInsert into database
    let dw: DesktopWindow = {titleBar: "Google", name: "msedge.exe",
      isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false} as DesktopWindow;
    let app: ApplicationRDG = new ApplicationRDG()
    app.create(dw)
    let appID:number = app.has("msedge.exe", "Google")
    // let appID:number = app.id
    console.log(appID)

    let createTime: TimeRDG = new TimeRDG()
    createTime.create(appID, 10000, 11000,  false, false)
    createTime.create(appID, 12000, 13000,  true, false)


    let create: TimeTDG = TimeTDG.getInstance()
    let timeList: TimeDTO[] = create.getByWasIdle()

    expect(timeList[0].start).to.equal(12000)
    expect(timeList[0].end).to.equal(13000)

  })

  it('Get By WasNotIdle', () => {
    // Create an application, Insert it into the databaseInsert into database
    let dw: DesktopWindow = {titleBar: "Google", name: "msedge.exe",
      isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false} as DesktopWindow;
    let app: ApplicationRDG = new ApplicationRDG()
    app.create(dw)
    let appID:number = app.has("msedge.exe", "Google")
    // let appID:number = app.id
    console.log(appID)

    let createTime: TimeRDG = new TimeRDG()
    createTime.create(appID, 10000, 11000,  true, false)
    createTime.create(appID, 12000, 13000,  false, true)


    let create: TimeTDG = TimeTDG.getInstance()
    let timeList: TimeDTO[] = create.getByWasNotIdle()

    expect(timeList[0].start).to.equal(12000)
    expect(timeList[0].end).to.equal(13000)

  })

  it('Get By WasForeground', () => {
    // Create an application, Insert it into the databaseInsert into database
    let dw: DesktopWindow = {titleBar: "Google", name: "msedge.exe",
      isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false} as DesktopWindow;
    let app: ApplicationRDG = new ApplicationRDG()
    app.create(dw)
    let appID:number = app.has("msedge.exe", "Google")
    // let appID:number = app.id
    console.log(appID)

    let createTime: TimeRDG = new TimeRDG()
    createTime.create(appID, 10000, 11000,  false, false)
    createTime.create(appID, 12000, 13000,  false, true)


    let create: TimeTDG = TimeTDG.getInstance()
    let timeList: TimeDTO[] = create.getByWasForeground()

    expect(timeList[0].start).to.equal(12000)
    expect(timeList[0].end).to.equal(13000)

  })

  it('Get By WasNotForeground', () => {
    // Create an application, Insert it into the databaseInsert into database
    let dw: DesktopWindow = {titleBar: "Google", name: "msedge.exe",
      isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false} as DesktopWindow;
    let app: ApplicationRDG = new ApplicationRDG()
    app.create(dw)
    let appID:number = app.has("msedge.exe", "Google")
    // let appID:number = app.id
    console.log(appID)

    let createTime: TimeRDG = new TimeRDG()
    createTime.create(appID, 10000, 11000,  false, true)
    createTime.create(appID, 12000, 13000,  false, false)


    let create: TimeTDG = TimeTDG.getInstance()
    let timeList: TimeDTO[] = create.getByWasNotForeground()

    expect(timeList[0].start).to.equal(12000)
    expect(timeList[0].end).to.equal(13000)

  })

  it('Test Deleting TimeTDG', () => {
    // Create an application, Insert it into the databaseInsert into database
    let dw: DesktopWindow = {titleBar: "Google", name: "msedge.exe",
      isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false} as DesktopWindow;
    let app: ApplicationRDG = new ApplicationRDG()
    app.create(dw)
    let appID:number = app.has("msedge.exe", "Google")
    // let appID:number = app.id
    console.log(appID)

    let createTime: TimeRDG = new TimeRDG()
    createTime.create(appID, 10000, 11000,  false, false)
    createTime.create(appID, 12000, 13000,  false, false)


    let create: TimeTDG = TimeTDG.getInstance()
    create.deleteByID(appID)
    let timeList: TimeDTO[] = create.getById(appID)

    expect(timeList.length).to.equal(0)
  });


})
