export interface Command {
    /**
     * return should be void, but this is set as any
     * so the current version of command pattern can function
     */

    execute(): any
}