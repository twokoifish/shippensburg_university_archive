import {Command} from "../Command";

import {TimeTDG} from "../../../dal/gateways/time/TimeTDG"
import TimeDTO from "../../../dal/gateways/time/TimeDTO";

export class TimeTotalUsageCommand implements Command {
  private readonly _id: number

  constructor(id: number){
    this._id = id
  }
  execute() {
      let t = TimeTDG.getInstance()
      let timeList: TimeDTO[] = t.getById(this._id)
      let totalTime = 0
      timeList.forEach( (time: TimeDTO)=>{
        totalTime += time.end - time.start
      })
      return totalTime
  }
}
