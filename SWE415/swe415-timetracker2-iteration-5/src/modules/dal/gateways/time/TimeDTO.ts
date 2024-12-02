class TimeDTO {
    private readonly _id : number
    private readonly _start : number
    private readonly _end : number
    private readonly _sessionNumber : number
    private readonly _wasIdle : boolean
    private readonly _wasForeground : boolean

    constructor (id : number, start : number, end : number, sessionNumber: number, wasIdle: boolean, wasForeground : boolean) {
        this._id = id
        this._start = start
        this._end = end
        this._sessionNumber = sessionNumber
        this._wasIdle = wasIdle
        this._wasForeground = wasForeground
    }

    get id() {
        return this._id
    }

    get start() {
        return this._start
    }

    get end() {
        return this._end
    }

    get sessionNumber() {
        return this._sessionNumber
    }

    get wasIdle() {
        return this._wasIdle
    }

    get wasForeground() {
        return this._wasForeground
    }
}
export default TimeDTO

