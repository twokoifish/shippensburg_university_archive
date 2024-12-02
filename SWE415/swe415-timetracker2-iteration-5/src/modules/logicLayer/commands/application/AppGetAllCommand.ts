import {Command} from "../Command";

import {ApplicationMapper} from "../../mapper/ApplicationMapper"

export class AppGetAllCommand implements Command {
    
    execute() {
        let a = ApplicationMapper.getInstance()
        return a.getAll()
    }
}