import React, { Component } from "react";
import "./about.css";
import { Link } from 'react-router-dom';
import { Icon } from "@fluentui/react/lib/Icon";
import Navigation from "./components/navigation/Navigation";

class Resources extends Component {


  render() {
    return (
      <div className={"resources"}>
        <h2 className={"header"}>Acknowledgement</h2>
        <p>
          This web application depends on the hard work of many free and open-source software projects. Their licenses are included below.
        </p>
        <br/>
        <h3 className={"header"}>Google Fonts</h3>
        <hr/>
        <h4 className={"text header"}>Poppins</h4>
        <p className={"text"}>
          These fonts are licensed under the Open Font License.
          You can use them freely in your products & projects - print or digital, commercial or otherwise.
          However, you can't sell the fonts on their own. This isn't legal advice, please consider consulting a lawyer and see the full license for all details.
        </p>
        <h4 className={"text header"}>Open Sans</h4>
        <p className={"text"}>
          These fonts are licensed under the Apache License, Version 2.0.
          You can use them freely in your products & projects - print or digital, commercial or otherwise. However, you can't sell the fonts on their own.
          This isn't legal advice, please consider consulting a lawyer and see the full license for all details.
        </p>
        <br/>
        <h3 className={"header"}>More info</h3>
        <hr/>
        <p className={"text"}>
          info
        </p>
        <br/>
        <h2 className={"header"}>Developed By:</h2>
        <p className={"text"}>
          <text>Isabella Boone</text><br/>
          <text>Joel Gingrich</text><br/>
          <text>Dan Holmgren</text><br/>
          <text>Andrew Januszko</text><br/>
          <text>Joshua Kellogg</text><br/>
          <text>Marlee Lackey</text><br/>
          <text>Ktyal Plummer</text><br/>
          <text>Evan Reese</text><br/>
          <text>Taryn Whitman</text><br/>
        </p>
      </div>
    );
  }

}



export default class About extends Component {

  render() {
    return (
      <div className="container">
        <Resources/>
      </div>
    );
  }

}

