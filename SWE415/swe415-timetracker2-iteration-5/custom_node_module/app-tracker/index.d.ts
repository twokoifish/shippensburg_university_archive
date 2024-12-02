// Type definitions for app-tracker
// Project: app-tracker

export type DesktopWindow = {
    name: string
    titleBar: string
    pid: number
    id: number
    isFocus: boolean
    preventsScreenSleep: boolean
    preventsSystemSleep: boolean
};

export function fetchDesktopWindows(): Promise<DesktopWindow[]>;
