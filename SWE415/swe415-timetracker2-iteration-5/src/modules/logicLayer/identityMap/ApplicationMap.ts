import ApplicationDTO from "../../dal/gateways/application/ApplicationDTO"
import ApplicationTDG from "../../dal/gateways/application/ApplicationTDG";
import {TimeTDG} from "../../dal/gateways/time/TimeTDG";
import TimeRDG from "../../dal/gateways/time/TimeRDG";
import TimeDTO from "../../dal/gateways/time/TimeDTO";
import ApplicationRDG from "../../dal/gateways/application/ApplicationRDG";

export default class ApplicationMap {
  private static instance : ApplicationMap | null
  private static map = new Map()

  public static getInstance() : ApplicationMap {
    if(!ApplicationMap.instance) {
      ApplicationMap.instance = new ApplicationMap()
    }

    return ApplicationMap.instance
  }

  public static has(promise : ApplicationDTO) {
    this.map.forEach( (key, value) => {
      if(value.windowClass === promise.windowClass && value.windowName === promise.windowName) {
        return key
      }
    })
    return -1
  }

  public static create(app : ApplicationDTO) {
    if(!this.map.has(app.id)) {
      this.map.set(app.id, app)
    }
  }

  public static read(id : number) {
    return (this.map.has(id) ? this.map.get(id) : null)
  }

  public static update(id : number, value : ApplicationDTO) {
    if(this.map.has(id)) {
      this.map.set(id, value)
    }
  }

  public static delete(id : number) {
    if(this.map.has(id)) {
      this.map.delete(id)

      let timeTDG : TimeDTO[] = TimeTDG.getInstance().getById(id);
      timeTDG.forEach((entry) => {
        let timeRDG : TimeRDG = new TimeRDG()
        timeRDG.read(entry.id, entry.start, entry.end);
        timeRDG.delete();
      })

    }
  }


}