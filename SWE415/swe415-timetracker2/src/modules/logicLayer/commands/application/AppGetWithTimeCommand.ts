import {Command} from "../Command";

import {ApplicationMapper} from "../../mapper/ApplicationMapper"
import {Application} from "../../mapper/Application";

export class AppGetWithTimeCommand implements Command {

  execute () {
    let a = ApplicationMapper.getInstance()
    // b.forEach((app)=>{
    //   console.log(app.times)
    // })
    return a.getAll()
  }
}
