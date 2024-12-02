/**
 * Defines the layout for the window, as defined 50% Navigation, 50% App.
 */
import React from "react";
import { render } from 'react-dom';
import { BrowserRouter } from 'react-router-dom';
import Navigation from "./pages/components/navigation/Navigation";
import NetworkSwitch from "./NetworkSwitch";
import "./index.css";

/**
 * Display content on the page.
 */
render(
    <BrowserRouter>
        <div className={"nav-menu"}>
            <Navigation/>
        </div>
        <div className={"content"}>
            <NetworkSwitch/>
        </div>
    </BrowserRouter>,
    document.querySelector('#root')
);