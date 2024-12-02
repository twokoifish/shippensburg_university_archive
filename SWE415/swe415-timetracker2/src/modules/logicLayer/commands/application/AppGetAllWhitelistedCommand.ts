import {Command} from "../Command";

import {ApplicationMapper} from "../../mapper/ApplicationMapper"

export class AppGetAllWhitelistedCommand implements Command {

  execute () {
    /**
     * this execute probably shouldn't return anything instead a constructor
     * should take in an object that execute will change a state of. Setting
     * a list inside of it to allWhiteListed. TypeScript may yell about the
     * current solution when the mapper is converted to TS.
     */
    let a = ApplicationMapper.getInstance()
    return a.getAllWhitelisted()
  }
}
