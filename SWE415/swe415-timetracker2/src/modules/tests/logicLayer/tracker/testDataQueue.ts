import chai, { expect } from "chai";
import DataQueue from "../../../logicLayer/tracker/DataQueue";
import {States} from "../../../logicLayer/tracker/States";
import {DesktopWindow} from "app-tracker";

describe("Testing the return sets from the DataQueue:", () => {
  let queue: DataQueue = DataQueue.getInstance();
  let set_a: DesktopWindow[];
  let set_b: DesktopWindow[];
  it("Test #1: OLD['a', 'c'] vs NEW['a', 'b', 'd']", () => {
    set_a = [{name: "a", titleBar: "a", pid: 0, id: 0, isFocus: false, preventsScreenSleep: false, preventsSystemSleep: false}, {name: "c", titleBar: "c", pid: 0, id: 0, isFocus: false, preventsScreenSleep: false, preventsSystemSleep: false}];
    set_b = [{name: "a", titleBar: "a", pid: 0, id: 0, isFocus: false, preventsScreenSleep: false, preventsSystemSleep: false}, {name: "b", titleBar: "b", pid: 0, id: 0, isFocus: false, preventsScreenSleep: false, preventsSystemSleep: false}, {name: "d", titleBar: "d", pid: 0, id: 0, isFocus: false, preventsScreenSleep: false, preventsSystemSleep: false}];
    let result_set: DesktopWindow[][] = queue.getChangelogWrapper(set_a, set_b);
    expect(result_set[0].length).to.equal(2);
    expect(result_set[1].length).to.equal(1);
    expect(result_set[0][0].name).to.equal("b");
    expect(result_set[0][1].name).to.equal("d");
    expect(result_set[1][0].name).to.equal("c");
    console.log("\tAdded " + result_set[0].length + " item(s).");
    console.log("\tRemoved " + result_set[1].length + " item(s).");
  });
  it("Test #2: OLD['a', 'c', 'e', 'f'] vs NEW['a', 'b', 'd']", () => {
    set_a = [{name: "a", titleBar: "a", pid: 0, id: 0, isFocus: false, preventsScreenSleep: false, preventsSystemSleep: false}, {name: "c", titleBar: "c", pid: 0, id: 0, isFocus: false, preventsScreenSleep: false, preventsSystemSleep: false}, {name: "e", titleBar: "e", pid: 0, id: 0, isFocus: false, preventsScreenSleep: false, preventsSystemSleep: false}, {name: "f", titleBar: "f", pid: 0, id: 0, isFocus: false, preventsScreenSleep: false, preventsSystemSleep: false}];
    set_b = [{name: "a", titleBar: "a", pid: 0, id: 0, isFocus: false, preventsScreenSleep: false, preventsSystemSleep: false}, {name: "b", titleBar: "b", pid: 0, id: 0, isFocus: false, preventsScreenSleep: false, preventsSystemSleep: false}, {name: "d", titleBar: "d", pid: 0, id: 0, isFocus: false, preventsScreenSleep: false, preventsSystemSleep: false}];
    let result_set: DesktopWindow[][] = queue.getChangelogWrapper(set_a, set_b);
    expect(result_set[0].length).to.equal(2);
    expect(result_set[1].length).to.equal(3);
    expect(result_set[0][0].name).to.equal("b");
    expect(result_set[0][1].name).to.equal("d");
    expect(result_set[1][0].name).to.equal("c");
    expect(result_set[1][1].name).to.equal("e");
    expect(result_set[1][2].name).to.equal("f");
    console.log("\tAdded " + result_set[0].length + " item(s).");
    console.log("\tRemoved " + result_set[1].length + " item(s).");
  });
});