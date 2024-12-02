export default class FormattedTime{

  readonly ms: number
  readonly seconds: number
  readonly minutes: number
  readonly hours: number
  readonly days: number
  formattedString : string

  constructor(ms :number){
    this.ms = ms
    let timeDiff = this.ms / 1000
    this.seconds = Math.floor(timeDiff) % 60
    timeDiff /= 60
    this.minutes = Math.floor(timeDiff) % 60
    timeDiff /= 60
    this.hours = Math.floor(timeDiff) % 24
    timeDiff /= 24
    this.days = Math.floor(timeDiff)
    this.formattedString = "0 secs"
    this.createFormattedTimeString()

  }

  createFormattedTimeString(){
    let formattedString: string = (this.seconds + "s")
    if(this.minutes !== 0){
      if(this.minutes )
        formattedString = (this.minutes + "m :" + this.seconds + "s")
    }
    if(this.hours !== 0){
      formattedString = (this.hours + "hr : " + this.minutes + "m : " + this.seconds + "s")
    }
    if(this.days !== 0){
      formattedString = (this.days + "d : " + this.hours + "hr : " + this.minutes + "m : " + this.seconds + "s")
    }
    this.formattedString = formattedString
  }

}
