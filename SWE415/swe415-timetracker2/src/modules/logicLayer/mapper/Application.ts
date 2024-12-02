import Time from "./Time"
import FormattedTime from "./formattedTime";
/**
 *
 */
export class Application {

  public readonly id: number
  readonly windowClass: string
  readonly windowName: string
  readonly isBlacklisted: boolean
  readonly times: Time[] | undefined
  timeLastForeground: FormattedTime
  totalTime: FormattedTime
  totalForeground: FormattedTime
  totalBackground: FormattedTime
  isSettingsModified: boolean;

  /**
   *
   * @param id
   * @param os
   * @param windowClass
   * @param windowName
   * @param isBlacklisted
   * @param times
   */
  constructor(id:number, windowClass:string, windowName:string, isBlacklisted:boolean, isSettingsModified:boolean, times:Time[]) {
    //if (id instanceof Number && os instanceof String && windowClass instanceof String && windowName instanceof String && isBlacklisted instanceof Number && Array.isArray(times) && times[0] instanceof Time) {
      this.id = id
      this.windowClass = windowClass
      this.windowName = windowName
      this.isBlacklisted = isBlacklisted
      this.times = times
      this.timeLastForeground = new FormattedTime(0)
      this.totalTime = new FormattedTime(0)
      this.totalForeground = new FormattedTime(0)
      this.totalBackground = new FormattedTime(0)
      this.isSettingsModified = isSettingsModified
      this.calcTotalTime()
    //} else {
    //  throw "Invalid arguments for `Application`."
    //}
  }

  private calcTotalTime(){
    let totalTime = 0
    let totalForeground = 0
    let totalBackground = 0
    let latestDate = 0
    let elapsedTime = 0
    if(this.times !== undefined) {
        this.times.forEach((time: Time) => {
            if (time.wasForeground) {
                totalForeground += time.end - time.start
                latestDate = time.end   //time should be sorted so at end of forEach time.end should be most recent
            } else {
                totalBackground += time.end - time.start
            }
        })
            totalTime = totalForeground + totalBackground
            if (latestDate === 0) {
                //console.log("latestDate " + this.windowName + latestDate)
                elapsedTime = latestDate
            } else {
                elapsedTime = new Date().getTime() - latestDate
                //console.log("elapsedTime " + this.windowName + elapsedTime)
            }
            this.timeLastForeground = new FormattedTime(elapsedTime)
            //console.log(this._timeLastTracked)
            this.totalForeground = new FormattedTime(totalForeground)
            this.totalBackground = new FormattedTime(totalBackground)
            this.totalTime = new FormattedTime(totalTime)
            //console.log(this._totalTime)
    }
  }
}
