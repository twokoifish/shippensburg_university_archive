/**
 * Import React elements for use on the page.
 */
import React, { Component } from 'react';
import { Route, Switch } from 'react-router-dom'

/**
 * All of the component in our site.
 */
import Overview from "./pages/Overview";
import Statistics from "./pages/Statistics";
import Settings from "./pages/Settings";
import About from "./pages/About";

/**
 * Allows switching the content of the window to a separate file.
 */
export default class NetworkSwitch extends Component {

    /**
     * Renders the pages relative to our file structure.
     * @returns {JSX.Element}
     */
    render() {
        return (
            <Switch>
                <Route exact path="/" component={Overview} />
                <Route exact path="/statistics" component={Statistics} />
                <Route exact path="/settings" component={Settings} />
                <Route exact path="/about" component={About} />
            </Switch>
        );
    }

}