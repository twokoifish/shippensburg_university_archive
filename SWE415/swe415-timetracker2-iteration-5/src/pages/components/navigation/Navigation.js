import React, { Component } from "react";
import { Nav, initializeIcons } from "@fluentui/react";
import "./navigation.css";
import {INavLinkGroup} from "@fluentui/react";

initializeIcons();

const links: INavLinkGroup[] = [
  {
    name: 'Time Tracker',
    links: [
      {
        name: 'Overview',
        key: 'key1',
        url: '/',
        iconProps: {
          iconName: 'ViewDashboard'
        }
      },
      {
        name: 'Statistics',
        key: 'key2',
        url: '/statistics',
        iconProps: {
          iconName: 'BarChartVertical'
        }
      },
      {
        name: 'Settings',
        key: 'key3',
        url: '/settings',
        iconProps: {
          iconName: 'Settings'
        }
      },
      {
        name: 'About',
        key: 'key4',
        url: '/about',
        iconProps: {
          iconName: 'ContactInfo'
        }
      }
    ]
  }
];

class Navigation extends Component {

  _onRenderGroupHeader(group: INavLinkGroup): JSX.Element {
    return <h2>{group.name}</h2>;
  }

  render() {
    return (
      <Nav
        onRenderGroupHeader={this._onRenderGroupHeader}
        groups={links}
        selectedKey={this.key}
      />
    );
  }

}

export default Navigation;