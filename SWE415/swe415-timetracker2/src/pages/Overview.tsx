import React, { Component } from "react";
import "./overview.css";
const { ipcRenderer } = window.require('electron');

interface AppNameProps {}
interface AppNameState {
    name: string;
}

class AppNameComponent extends Component<AppNameProps, AppNameState> {
    
    constructor(props: AppNameProps) {
        super(props);
        this.state = {
            name: "None"
        };
    }
    
    componentDidMount() {
        this.refresh();
    }
    
    getAppName(): void {
        ipcRenderer.send('LastActiveApp_Request', 'Requesting last active app');
        ipcRenderer.once('LastActiveApp_Response', (event, args: string) => {
            this.setState({name: args});
        })
    }
    
    refresh(): void {
        setInterval(() => {
            this.getAppName();
        }, 1000);
        
    }
    
    render() {
        return (
          <div className={"lastActive"}>
              <h1>Active Application</h1>
              <p>{this.state.name}</p>
          </div>
        );
    }
}

class AppInstructionsComponent extends Component {
    render() {
        return undefined;
    }
}


export default class Overview extends Component {

    render() {
        return (
          <div className={"container"}>
              <AppNameComponent />
          </div>
        );
    }

}