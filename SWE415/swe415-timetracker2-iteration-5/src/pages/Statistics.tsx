import React, { Component } from "react";

import {Icon} from "@fluentui/react/lib/Icon";
import "./statistics.css";
import {TooltipHost} from "@fluentui/react";
import {Application} from "../modules/logicLayer/mapper/Application";
import Time from "../modules/logicLayer/mapper/Time";

import { Chart, Series, Title, Legend, CommonSeriesSettings, Font, ValueAxis, ArgumentAxis, Label, Tooltip } from 'devextreme-react/chart';

interface IDataItem {
    app: string;
    total: number;
    foreground: number;
    background: number;
}

let chartData: IDataItem[] = [];

const { ipcRenderer } = window.require('electron');

interface AppProps{

}
interface AppState {
    appList: Application[];
    isNameSortedAsc: boolean;
    isLastForegroundSortedAsc: boolean;
    isLastTrackedSortedAsc: boolean;
    isWasForegroundSortedAsc: boolean;
    isWasBackgroundSortedAsc: boolean;
    isTotalTimeSortedAsc: boolean;
    lastSortType: string;
}

class AppsTable extends React.Component<AppProps, AppState>  {
    constructor(props: AppProps) {
        super(props);

        let time: Time[] = [new Time(0, 0, 0, 0, false, true)]
        let tempAppList = [new Application(0,  "test-windowClass", "test-windowName", false, false, time)]

        this.state = {appList : tempAppList,
            isNameSortedAsc : false,
            isLastForegroundSortedAsc : false,
            isLastTrackedSortedAsc : false,
            isWasForegroundSortedAsc: false,
            isWasBackgroundSortedAsc: false,
            isTotalTimeSortedAsc : false,
            lastSortType : "name"}

        this.handleButtonClick = this.handleButtonClick.bind(this)
        this.handleSortName= this.handleSortName.bind(this)
        this.handleSortLastForeground = this.handleSortLastForeground.bind(this)
        this.handleSortWasForeground = this.handleSortWasForeground.bind(this)
        this.handleSortWasBackground = this.handleSortWasBackground.bind(this)
        this.handleSortTotalTime = this.handleSortTotalTime.bind(this)
    }

    componentDidMount() {
        this.handleButtonClick()
    }

    handleButtonClick() {
        this.fetchAppsList()
        this.createGraphValues()
    }

    fetchAppsList(){

        ipcRenderer.send('AppsGetAllWithTime_Request', 'requesting all apps with time')
        ipcRenderer.once('AppsGetAllWithTime_Response', (event: any, apps: Application[]) => {
            let tempApps: Application[] = apps
            this.setState(() => ({
                appList : tempApps
            }))

            switch(this.state.lastSortType){
                case "name":
                    this.sortName()
                    break
                case "lastForeground":
                    this.sortLastForeground()
                    break
                case "wasForeground":
                    this.sortWasForeground()
                    break
                case "wasBackground":
                    this.sortWasBackground()
                    break
                case "totalTime":
                    this.sortTotalTime()
                    break
                default:
                    console.log("default")
            }
        })
    }

    async handleSortName(){
        console.log("sortName")
        console.log(this.state)
        await this.setState(() => ({
            lastSortType: "name",
            isNameSortedAsc: !this.state.isNameSortedAsc
        }))
        console.log("sortName")
        console.log(this.state)
        this.sortName()
    }

    async handleSortLastForeground(){
        console.log("sortLastForeground")
        console.log(this.state)
        await this.setState(() => ({
            lastSortType: "lastForeground",
            isLastForegroundSortedAsc: !this.state.isLastForegroundSortedAsc
        }))
        console.log("sortLastForeground")
        console.log(this.state)
        this.sortLastForeground()
    }

    async handleSortWasForeground(){
        console.log("sortWasForeground")
        console.log(this.state)
        await this.setState(() => ({
            lastSortType: "wasForeground",
            isWasForegroundSortedAsc: !this.state.isWasForegroundSortedAsc
        }))
        console.log("sortWasForeground")
        console.log(this.state)
        this.sortWasForeground()
    }

    async handleSortWasBackground(){
        console.log("sortWasBackground")
        console.log(this.state)
        await this.setState(() => ({
            lastSortType: "wasBackground",
            isWasBackgroundSortedAsc: !this.state.isWasBackgroundSortedAsc
        }))
        console.log("sortWasBackground")
        console.log(this.state)
        this.sortWasBackground()
    }

    async handleSortTotalTime(){
        console.log("sortTotalTime")
        console.log(this.state)
        await this.setState(() => ({
            lastSortType: "totalTime",
            isTotalTimeSortedAsc: !this.state.isTotalTimeSortedAsc
        }))
        console.log("sortTotalTime")
        console.log(this.state)
        this.sortTotalTime()
    }

    sortName(){
        //console.log("about to sort in " + this.state.isNameSortedAsc)
        if(this.state.isNameSortedAsc) {
            this.sortAppList((a: Application, b: Application) => {
                let fa = a.windowName.toLowerCase(), fb = b.windowName.toLowerCase();

                if (fa < fb) {
                    return -1;
                }
                if (fa > fb) {
                    return 1;
                }
                return 0;
            })
        } else if(!this.state.isNameSortedAsc){
            this.sortAppList((a: Application, b: Application) => {
                let fa = a.windowName.toLowerCase(), fb = b.windowName.toLowerCase();

                if (fa > fb) {
                    return -1;
                }
                if (fa < fb) {
                    return 1;
                }
                return 0;
            })
        }
    }

    sortTotalTime(){
        // console.log("about to sort in " + this.state.isSortedAsc)
        if(this.state.isTotalTimeSortedAsc) {
            this.sortAppList((a: any, b: any) => {
                return a.totalTime.ms - b.totalTime.ms
            })
        } else if(!this.state.isTotalTimeSortedAsc){
            this.sortAppList((a: any, b: any) => {
                return b.totalTime.ms - a.totalTime.ms
            })
        }
    }

    sortWasForeground(){
        if(this.state.isWasForegroundSortedAsc) {
            this.sortAppList((a: any, b: any) => {
                return a.totalForeground.ms - b.totalForeground.ms
            })
        } else if(!this.state.isWasForegroundSortedAsc){
            this.sortAppList((a: any, b: any) => {
                return b.totalForeground.ms - a.totalForeground.ms
            })
        }
    }

    sortWasBackground(){
        if(this.state.isWasBackgroundSortedAsc) {
            this.sortAppList((a: any, b: any) => {
                return a.totalBackground.ms - b.totalBackground.ms
            })
        } else if(!this.state.isWasBackgroundSortedAsc){
            this.sortAppList((a: any, b: any) => {
                return b.totalBackground.ms - a.totalBackground.ms
            })
        }
    }

    sortLastForeground(){
        // console.log("about to sort in " + this.state.isSortedAsc)
        if(this.state.isLastForegroundSortedAsc) {
            this.sortAppList((a: Application, b: Application)=>{
            return a.timeLastForeground.ms - b.timeLastForeground.ms
          })
        } else if(!this.state.isLastForegroundSortedAsc){
            this.sortAppList((a: Application, b: Application) => {
                return b.timeLastForeground.ms - a.timeLastForeground.ms
            })
        }
    }

    sortAppList(f: (a: Application, b: Application) => any){
        let tempList:Application[] = this.state.appList
        tempList.sort(f)
        console.log("tempList:")
        console.log(tempList)
        this.setState({appList : tempList})
        console.log("appList")
        console.log(this.state.appList)
    }

    createGraphValues(){
        chartData = []
        let apps = this.state.appList
        apps.forEach((Application)=>{
           let totalTime = Application.totalTime.ms / 60000;
           let totalForeground = Application.totalForeground.ms / 60000;
           let totalBackground = Application.totalBackground.ms / 60000;

           if (totalTime > 0 || totalForeground > 0 || totalBackground > 0) {
               let name = Application.windowName;
               let data = {
                   app: name,
                   total: totalTime,
                   foreground: totalForeground,
                   background: totalBackground,
                }
                chartData.push(data)
           }
        })
        console.log(chartData)
    }

    render(): React.ReactNode{
        let div = <>
            <div className="container">
                <Chart id="chart" dataSource={chartData}>
                    <Title text="Application Usage">
                        <Font size={17} weight={700} color="white" family="Poppins" />
                    </Title>
                    <Tooltip enabled={true} />
                    <CommonSeriesSettings
                        argumentField="app"
                        type="bar"
                        ignoreEmptyPoints={true}
                        barPadding={0.25}
                    >
                    </CommonSeriesSettings>

                    <Series
                        valueField="total"
                        name="Total Time"
                    />
                    <Series
                        valueField="foreground"
                        name="Foreground"
                    />
                    <Series
                        valueField="background"
                        name="Background"
                    />

                    <ArgumentAxis color="#FFFFF">
                        <Label overlappingBehavior={"stagger"} staggeringSpacing={1}>
                            <Font size={8} weight={300} color="white" family="Poppins" />
                        </Label>
                    </ArgumentAxis>

                    <ValueAxis color="#FFFFF" allowDecimals={false}>
                        <Title text="Time in Minutes">
                            <Font size={10} weight={450} color="white" family="Poppins" />
                        </Title>
                        <Label overlappingBehavior={"stagger"} staggeringSpacing={1}>
                            <Font size={10} weight={500} color="white" family="Poppins" />
                        </Label>
                    </ValueAxis>

                    <Legend verticalAlignment="top" horizontalAlignment="center">
                            fontColor: "#FFFFFF"
                            <Font size={10} weight={450} color="white" family="Poppins" />
                    </Legend>
                </Chart>
                <TooltipHost content="Refresh">
                    <button onClick={this.handleButtonClick}><Icon iconName="Refresh"/></button>
                </TooltipHost>
                <table id="appsTable">
                    <tbody>
                    <tr>
                        <th onClick={this.handleSortName}>Name{this.state.lastSortType !== "name" ?
                            <Icon className="chevIcon" iconName="ScrollUpDown"/> : (this.state.isNameSortedAsc ?
                                <Icon className="chevIcon" iconName="ChevronUp"/> :
                                <Icon className="chevIcon" iconName="ChevronDown"/>)}</th>
                        <th onClick={this.handleSortWasForeground}>Foreground{this.state.lastSortType !== "wasForeground" ?
                            <Icon className="chevIcon"
                                  iconName="ScrollUpDown"/> : (this.state.isWasForegroundSortedAsc ?
                                <Icon className="chevIcon" iconName="ChevronUp"/> :
                                <Icon className="chevIcon" iconName="ChevronDown"/>)}</th>
                        <th onClick={this.handleSortWasBackground}>Background{this.state.lastSortType !== "wasBackground" ?
                            <Icon className="chevIcon"
                                  iconName="ScrollUpDown"/> : (this.state.isWasBackgroundSortedAsc ?
                                <Icon className="chevIcon" iconName="ChevronUp"/> :
                                <Icon className="chevIcon" iconName="ChevronDown"/>)}</th>
                        <th onClick={this.handleSortTotalTime}>Total Time{this.state.lastSortType !== "totalTime" ?
                            <Icon className="chevIcon" iconName="ScrollUpDown"/> : (this.state.isTotalTimeSortedAsc ?
                                <Icon className="chevIcon" iconName="ChevronUp"/> :
                                <Icon className="chevIcon" iconName="ChevronDown"/>)}</th>
                        <th onClick={this.handleSortLastForeground}>Last
                            Foreground{this.state.lastSortType !== "lastForeground" ? <Icon className="chevIcon"
                                                                                            iconName="ScrollUpDown"/> : (this.state.isLastForegroundSortedAsc ?
                                <Icon className="chevIcon" iconName="ChevronUp"/> :
                                <Icon className="chevIcon" iconName="ChevronDown"/>)}</th>


                    </tr>
                    {this.state.appList.map((app: Application) =>
                        <tr key={app.id}>
                            <td>{app.windowName}</td>
                            <td>{app.totalForeground.formattedString}</td>
                            <td>{app.totalBackground.formattedString}</td>
                            <td>{app.totalTime.formattedString}</td>
                            <td>{(app.timeLastForeground !== undefined) ? app.timeLastForeground.ms === 0 ? "never" : app.timeLastForeground.formattedString + " ago" :
                                <h1>still not good</h1>}</td>
                        </tr>
                    )}
                    </tbody>
                </table>
            </div>
        </>;
        return div
    }
}
class Statistics extends Component {

    render() {
        return (
            <div className="container">
                <AppsTable/>
            </div>
        );
    }

}

export default Statistics;