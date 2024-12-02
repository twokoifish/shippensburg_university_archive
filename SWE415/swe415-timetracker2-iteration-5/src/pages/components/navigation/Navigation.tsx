import * as React from 'react';
import { Nav, INavLinkGroup } from '@fluentui/react/lib/Nav';
import { initializeIcons } from '@fluentui/react/lib/Icons';
import "./navigation.css";
import {IRenderFunction} from "@fluentui/react";
import {IRenderGroupHeaderProps} from "@fluentui/react/dist/react";

initializeIcons();

const links: INavLinkGroup[] = [
    {
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

const Navigation: React.FunctionComponent = () => {

    return (
        <Nav
            //onRenderGroupHeader={_onRenderGroupHeader}
            groups={links}
        />
    );
};

function _onRenderGroupHeader(group: INavLinkGroup): JSX.Element {
    return (<h3>{group.name}</h3>);
}

export default Navigation;