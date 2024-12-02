import {Command} from "../Command";
import ApplicationRDG from "../../../dal/gateways/application/ApplicationRDG";



export class AppWasModifiedCommand implements Command {
  private readonly _id: number

  constructor(id: number){
    this._id = id
  }

  execute(){
    try{
      let app:ApplicationRDG = new ApplicationRDG()
      app.read(this._id)
      if(app.isSettingsModified === false) {
        app.isSettingsModified = true
      }

      app.update()

    }catch (e) {
      console.log(e)
    }

  }
}