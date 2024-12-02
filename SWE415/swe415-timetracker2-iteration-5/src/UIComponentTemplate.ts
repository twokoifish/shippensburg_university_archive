import React, { Component } from "react";

export default interface UIComponent extends Component {
    render(): JSX.Element;
}