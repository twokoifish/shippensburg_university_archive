import {Command} from "../Command";

import {TimeTDG} from "../../../dal/gateways/time/TimeTDG"

export class TimeDeleteByIDCommand implements Command {
  private readonly _id: number

  constructor(id: number){
    this._id = id
  }
  execute() {
    let t = TimeTDG.getInstance()
    t.deleteByID(this._id)
  }
}
