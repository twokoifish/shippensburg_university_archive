import chai, { expect } from "chai";
import ApplicationTracker from "../../../logicLayer/tracker/ApplicationTracker";
import { States } from "../../../logicLayer/tracker/States";
// import chaiAsPromised from "chai-as-promised";

describe("Testing tracker states:", () => {
  let tracker:ApplicationTracker = ApplicationTracker.getInstance();
  it("Default state.", () => {
    expect(tracker.getState()).to.equal(States.INACTIVE);
  });
  it("Default state -> Active state.", () => {
    tracker.startTracking()
    expect(tracker.getState()).to.equal(States.ACTIVE);
  });
  it("Active state -> Inactive state.", () => {
    tracker.stopTracking()
    expect(tracker.getState()).to.equal(States.INACTIVE);
    tracker.startTracking()
    expect(tracker.getState()).to.equal(States.ACTIVE);
    tracker.pauseTracking()
    expect(tracker.getState()).to.equal(States.INACTIVE);
  });
  
})