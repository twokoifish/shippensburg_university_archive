import React, { Component } from "react";
import { Toggle } from '@fluentui/react';
import { Icon } from '@fluentui/react/lib/Icon';
import {Application} from "../modules/logicLayer/mapper/Application";
import Time from "../modules/logicLayer/mapper/Time";
import "./settings.css";
const { ipcRenderer } = window.require('electron');

//TODO sorts don't work
/**
 * Props passed to AppsSettingsTables
 */

interface SettingsProps {
  appList: Application[];
  listUpdate: any;
  fetchAppList: any;
  resetDefaultClick: any;
  resetCurrentClick: any;
  saveCurrentClick: any;
}

/**
 * props passed to Settings tables
 */
interface SettingsTablesProps extends SettingsProps{
  sortFunction: any;
  filterFunction?: any;
}

/**
 * states of AppsSettingsTables
 */
interface SettingsTablesState {
  sortFunction: any;
}

/**
 * AppsSettingsTables
 * Constructs UnsetAppsTable and AppsTable
 * Manages sorting of those tables
 */
  //<Props, States> declares expected types of props and state
class AppsSettingsTables extends React.Component<SettingsProps, SettingsTablesState> {
  constructor(props: SettingsProps) {
    super(props);
    //console.log(this.props.appList)
    this.state = {sortFunction: this.sortTotalTimeAsc}
    this.handleSortTotalTime = this.handleSortTotalTime.bind(this)
  }

  // componentDidMount() {
  //   //this.handleButtonClick()
  // }


  async handleSortTotalTime(){
    if(this.state.sortFunction === this.sortTotalTimeAsc){
      await this.setState(() => ({
        sortFunction: this.sortTotalTimeDesc
      }))
    }else{
      await this.setState(() => ({
        sortFunction: this.sortTotalTimeAsc
      }))}

    // this.sortTotalTime()
  }


  sortTotalTimeAsc(a :Application, b :Application){
    return a.totalTime.ms - b.totalTime.ms
  }


  sortTotalTimeDesc(a:Application, b:Application){
    return b.totalTime.ms - a.totalTime.ms
  }


  filterIsModified(app:Application){
    return app.isSettingsModified === false
  }


  containsUnset(){
    let unset = false
    this.props.appList.forEach((app: Application)=>{
      if(app.isSettingsModified === false){
        unset = true
      }
    })
    return unset
  }

  render() {
    //@TODO br's could be replaced by proper padding/margins

    return (
      <div className="container">

        {this.containsUnset() === true ? <UnsetAppsTable appList={this.props.appList} fetchAppList={this.props.fetchAppList} listUpdate={this.props.listUpdate} sortFunction={this.state.sortFunction} filterFunction={this.filterIsModified} resetCurrentClick={this.props.resetCurrentClick} resetDefaultClick={this.props.resetDefaultClick} saveCurrentClick={this.props.saveCurrentClick}/> : null}
        <br/>
        <button onClick={this.props.resetDefaultClick}>Reset To Defaults</button>
        <button onClick={this.props.resetCurrentClick}>Reset To Current</button>
        <button onClick={this.props.saveCurrentClick}>Save Current</button>
        <button onClick={this.props.fetchAppList}><Icon iconName="Refresh"/></button>
        <button onClick={this.handleSortTotalTime}>{ (this.state.sortFunction === this.sortTotalTimeAsc ? <Icon className="chevIcon" iconName="ChevronUp"/> : <Icon className="chevIcon" iconName="ChevronDown"/>)}</button>
        <AppsTable appList={this.props.appList} fetchAppList={this.props.fetchAppList} listUpdate={this.props.listUpdate} sortFunction={this.state.sortFunction} resetCurrentClick={this.props.resetCurrentClick} resetDefaultClick={this.props.resetDefaultClick} saveCurrentClick={this.props.saveCurrentClick}/>
      </div>
    )
  }
}


/**
 * Displays a table with apps who's settings haven't been modified yet.
 * Gives option to modify or ignore.
 */
class UnsetAppsTable extends React.Component<SettingsTablesProps, {}> {
  constructor(props: SettingsTablesProps) {
    super(props);
  }
  render(){
    return(
      <div>
        <h3>Hey there! looks like we detected some new apps. Would you like them tracked?</h3>
        <table className="appsTable">
          <thead>
          <tr>
            <th>Name</th>
            <th>Tracked</th>
            <th>Ignore</th>
          </tr>
          </thead>
          <tbody>
          {
            this.props.appList.length === 0
              ? <td>No Apps Tracked</td>
              : this.props.appList.sort(this.props.sortFunction).filter(this.props.filterFunction).map((app) =>
                <tr key = {app.id}>
                  <td>{app.windowName}</td>
                  <td>{app.isBlacklisted ? <Toggle onChange={(event: any) => {
                      ipcRenderer.send('AppToggleTracked_Request', app.id)
                      ipcRenderer.send('AppWasModified_Request', app.id)
                      window.location.reload()
                    }}/>
                    : <Toggle defaultChecked  onChange={(event: any) => {
                      ipcRenderer.send('AppToggleTracked_Request', app.id)
                      ipcRenderer.send('AppWasModified_Request', app.id)
                      window.location.reload()
                    }}/>}
                  </td>
                  <td><button onClick={(event) => {
                    ipcRenderer.send('AppWasModified_Request', app.id)
                    window.location.reload()
                  }}><Icon iconName="Cancel"/></button>
                  </td>
                </tr>
              )
          }
          </tbody>
        </table>
      </div>
    )
  }
}

/**
 * Displays table of all apps and settings that can be changed for each app.
 */
class AppsTable extends Component <SettingsTablesProps, {}>{
  constructor(props: SettingsTablesProps) {
    super(props);
  }
  render(){
    return(
      <table className="appsTable">
        <tbody>
        <tr>
          <th>Name</th>
          <th>Tracked</th>
          <th>Clear History</th>
        </tr>
        {
          this.props.appList.length === 0
            ? <tr><td>No Apps Tracked</td></tr>
            : this.props.appList.sort(this.props.sortFunction).map((app) =>
              <tr key = {app.id}>
                <td>{app.windowName}</td>
                <td>{app.isBlacklisted ? <Toggle onChange={(event: any) => {
                    console.log(event)
                    ipcRenderer.send('AppToggleTracked_Request', app.id)
                    if(!app.isSettingsModified){
                      ipcRenderer.send('AppWasModified_Request', app.id)
                    }
                  }}/>
                  : <Toggle defaultChecked onChange={(event: any) => {
                    console.log(event)
                    ipcRenderer.send('AppToggleTracked_Request', app.id)
                    if(!app.isSettingsModified){
                      ipcRenderer.send('AppWasModified_Request', app.id)
                    }
                  }}/>}
                </td>
                <td><button onClick={(event) => {
                  ipcRenderer.send('TimeDeleteByID_Request', app.id)
                }}><Icon iconName="Delete"/></button>
                </td>
              </tr>
            )
        }
        </tbody>
      </table>
    )
  }
}



/**
 * State for Settings page
 */
interface SettingsState {
  appList: Application[];
  //sortFunction: any;
}

/**
 * settings page
 */
export default class Settings extends React.Component<{}, SettingsState> {
  constructor(props:any){
    super(props)
    let time: Time[] = [new Time(0, 0, 0, 0, false, false)]
    let tempAppList = [new Application(0,  "test-windowClass", "test-windowName", false, false, time)]

    this.state = {appList : tempAppList}
    this.fetchAppList = this.fetchAppList.bind(this)
    this.appListUpdate = this.appListUpdate.bind(this)
    this.resetDefaultClick = this.resetDefaultClick.bind(this)
    this.resetCurrentClick = this.resetCurrentClick.bind(this)
    this.saveCurrentClick = this.saveCurrentClick.bind(this)
  }

  componentDidMount() {
      this.fetchAppList()
  }

  /**
   * Updates appList
   */
  appListUpdate(newList : Application[]){
    this.setState({
      appList: newList
    })
  }

  /**
   * gets appList from the database
   */
  fetchAppList(){
    ipcRenderer.send('AppsGetAllWithTime_Request', 'requesting all apps with time')
    ipcRenderer.once('AppsGetAllWithTime_Response', (event, apps: Application[]) => {
      this.appListUpdate(apps)
    })

  }


  /*
  * call the command from the ipcHandler to reset the default settings
  */
  resetDefaultClick(){
    ipcRenderer.send('ResetDefaultSettings_Request', 'resetting default settings')
    this.fetchAppList()
    window.location.reload()
  }

  /*
   * call the command from the ipcHandler to reset the default settings
   */
  resetCurrentClick() {
    ipcRenderer.send('ResetCurrentSettings_Request', 'resetting current settings')
    this.fetchAppList()
    window.location.reload()
  }

  /*
   * call the command from the ipcHandler to reset the default settings
   */
  saveCurrentClick() {
    ipcRenderer.send('SaveCurrentSettings_Request', 'saving current settings')
  }

  render() {
    return (
      <div className="container">
        <br/>
        <AppsSettingsTables appList={this.state.appList} listUpdate={this.appListUpdate} fetchAppList={this.fetchAppList} resetCurrentClick={this.resetCurrentClick} resetDefaultClick={this.resetDefaultClick} saveCurrentClick={this.saveCurrentClick}/>
      </div>
    );
  }

}
