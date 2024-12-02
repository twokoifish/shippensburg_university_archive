import {Command} from "../Command";
import ApplicationRDG from "../../../dal/gateways/application/ApplicationRDG";


export class AppToggleTrackedCommand implements Command {
  private readonly _id: number

  constructor(id: number){
    this._id = id
  }

  execute(){
    try{
      let app:ApplicationRDG = new ApplicationRDG()
      app.read(this._id)
      if(app.isBlacklisted == false){
        app.isBlacklisted = true
      }else{
        app.isBlacklisted = false
      }
      app.update()

    }catch (e) {
      console.log(e)
    }

  }
}
