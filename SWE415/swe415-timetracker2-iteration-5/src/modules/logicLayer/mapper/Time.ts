/**
 *
 */
export default class Time {

  readonly id : number
  readonly start : number
  readonly end : number
  readonly sessionNumber : number
  readonly wasIdle : boolean
  readonly wasForeground : boolean

  /**
   *
   * @param id
   * @param start
   * @param end
   * @param sessionNumber
   * @param wasIdle
   * @param wasForeground
   */
  constructor(id : number, start : number, end : number, sessionNumber: number, wasIdle: boolean, wasForeground: boolean) {
    //if (id instanceof Number && start instanceof Number && end instanceof Number && sessionNumber instanceof Number && wasIdle instanceof Number) {
      this.id = id
      this.start = start
      this.end = end
      this.sessionNumber = sessionNumber
      this.wasIdle = wasIdle
      this.wasForeground = wasForeground
    // } else {
    //   throw "Invalid parameters for `Time`."
    // }
  }
}
