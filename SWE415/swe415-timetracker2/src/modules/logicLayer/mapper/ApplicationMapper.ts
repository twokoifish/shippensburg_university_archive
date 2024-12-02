import ApplicationTDG from "../../dal/gateways/application/ApplicationTDG"
import {TimeTDG} from "../../dal/gateways/time/TimeTDG"
import Time  from "./Time"
import {Application}  from "./Application"
import TimeDTO from "../../dal/gateways/time/TimeDTO";
import ApplicationDTO from "../../dal/gateways/application/ApplicationDTO";

export class ApplicationMapper {
  private static instance : ApplicationMapper | null;

  private constructor() {

  }

  public static getInstance(): ApplicationMapper {

    if (!ApplicationMapper.instance) {
      ApplicationMapper.instance = new ApplicationMapper();
    }

    return ApplicationMapper.instance;
  }

  getAllWhitelisted() {
    let appTDG = ApplicationTDG.getInstance()
    let rApps = appTDG.getWhitelisted()
    let dApps:Application[] = []
    rApps.forEach((app:ApplicationDTO) => {
      const id = app.id;
      let tTDG = TimeTDG.getInstance()
      let rTimes = tTDG.getById(id)
      let dTime:Time[] = []
      rTimes.forEach((time: TimeDTO) => {
        let tObj = new Time(time.id, time.start, time.end, time.sessionNumber, time.wasIdle, time.wasForeground)
        dTime.push(tObj)
      })

      let aObj = new Application(app.id, app.windowClass, app.windowName, app.isBlacklisted, app.isSettingsModified, dTime)
      dApps.push(aObj)
    })
    return dApps
  }

  getAll() {
    let appTDG = ApplicationTDG.getInstance()
    let rApps = appTDG.getAll()
    let dApps:Application[] = []
    rApps.forEach((app:ApplicationDTO) => {
      const id = app.id;
      let tTDG = TimeTDG.getInstance()
      let rTimes:TimeDTO[] = tTDG.getById(id)
      let dTime:Time[] = []
      rTimes.forEach((time:TimeDTO) => {
        let tObj = new Time(time.id, time.start, time.end, time.sessionNumber, time.wasIdle, time.wasForeground)
        dTime.push(tObj)
      })

      let aObj = new Application(app.id, app.windowClass, app.windowName, app.isBlacklisted, app.isSettingsModified, dTime)
      dApps.push(aObj)
    })

    return dApps
  }

}
