import chai = require("chai");
import SchemaBuilder from "../../../dal/database/schemaBuilder/SchemaBuilder";
import DatabaseManager from "../../../dal/database/manager/DatabaseManager";
import ApplicationTDG from "../../../dal/gateways/application/ApplicationTDG";
import ApplicationDTO from "../../../dal/gateways/application/ApplicationDTO";
import ApplicationRDG from "../../../dal/gateways/application/ApplicationRDG";
import {DesktopWindow} from "app-tracker";

const expect = chai.expect;
const schema: SchemaBuilder = SchemaBuilder.getInstance();

/**
 * Rebuild database before running tests.
 */
global.beforeEach(() => {
  schema.buildUp()
  // Insert data
  let create1 = new ApplicationRDG()
  let create2 = new ApplicationRDG()
  let create3 = new ApplicationRDG()
  let create4 = new ApplicationRDG()
  let create5 = new ApplicationRDG()
  let create6 = new ApplicationRDG()
  let create7 = new ApplicationRDG()

  let w1: DesktopWindow = {titleBar: "Amsterdam", name: "spotify.exe",
    isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false};
  let w2: DesktopWindow = {titleBar: "Part II", name: "spotify.exe",
    isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false};
  let w3: DesktopWindow = {titleBar: "Google", name: "chrome.exe",
    isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false};
  let w4: DesktopWindow = {titleBar: "Use Somebody", name: "spotify.exe",
    isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false};
  let w5: DesktopWindow = {titleBar: "Geyser", name: "spotify.exe",
    isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false};
  let w6: DesktopWindow = {titleBar: "bad idea!", name: "spotify.exe",
    isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false};
  let w7: DesktopWindow = {titleBar: "Google", name: "msedge.exe",
    isFocus: false, id: 0, pid: 0, preventsSystemSleep: false, preventsScreenSleep: false};

  create1.create(w1)
  create2.create(w2)
  create3.create(w3)
  create4.create(w4)
  create5.create(w5)
  create6.create(w6)
  create7.create(w7)

})

/**
 * Delete database after testing.
 */
global.afterEach(() => {
  schema.tearDown()
})

describe('Test ApplicationTDG', () => {

  it('Test getByWindowClass ApplicationTDG', () => {
    let getByClassSpotify: ApplicationDTO[] = ApplicationTDG.getInstance().getByClass("spotify.exe")
    getByClassSpotify.forEach((ApplicationDTO) => {
      expect(ApplicationDTO.windowClass).to.equal("spotify.exe")
    })
    expect(getByClassSpotify.length).to.equal(5)

    let getByClassChrome: ApplicationDTO[] = ApplicationTDG.getInstance().getByClass("chrome.exe")
    getByClassChrome.forEach((ApplicationDTO) => {
      expect(ApplicationDTO.windowClass).to.equal("chrome.exe")
    })
    expect(getByClassChrome.length).to.equal(1)
  })

  it('Test getByWindowName ApplicationTDG', () => {
    let getByNamePartII : ApplicationDTO[] = ApplicationTDG.getInstance().getByName("Part II")
    getByNamePartII.forEach((ApplicationDTO) => {
      console.log(ApplicationDTO.id)
      expect(ApplicationDTO.windowName).to.equal("Part II")
    })

    let getByNameGeyser : ApplicationDTO[] = ApplicationTDG.getInstance().getByName("Geyser")
    getByNameGeyser.forEach((ApplicationDTO) => {
      expect(ApplicationDTO.windowName).to.equal("Geyser")
    })
  })

  it('Test getBlacklisted ApplicationTDG', () => {
    let getBlacklisted : ApplicationDTO[] = ApplicationTDG.getInstance().getBlacklisted()
    getBlacklisted.forEach((ApplicationDTO) => {
      expect(ApplicationDTO.isBlacklisted).to.equal(true)
    })
  })

  it('Test getWhitelisted ApplicationTDG', () => {
    let getWhitelisted : ApplicationDTO[] = ApplicationTDG.getInstance().getWhitelisted()
    getWhitelisted.forEach((ApplicationDTO) => {
      expect(ApplicationDTO.isBlacklisted).to.equal(false)
    })
  })


  it('Test getAll ApplicationTDG', () => {
    let getAll : ApplicationDTO[] = ApplicationTDG.getInstance().getAll()
    expect(getAll.length).to.equal(7)
  })
});
