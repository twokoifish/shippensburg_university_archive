#include <napi.h>

// Apple APIs
#import <AppKit/AppKit.h>
#import <IOKit/pwr_mgt/IOPMLib.h>

// Constants and Defines
// const int MIN_WINDOW_SIZE = 50; // track the min window size to be tracked

// function to find PIDs keeping the screen or the system awake
NSDictionary* FindPIDAssertions()
{
    // build list of PIDs to return
    NSMutableArray *systemNoSleepPIDS = [NSMutableArray array];
    NSMutableArray *screenNoSleepPIDS = [NSMutableArray array];

    // pull list of processes with assertions associated with them (PIDs preventing the system or display from sleeping)
    NSDictionary* assertionsByPid = nil;
    IOPMCopyAssertionsByProcess((CFDictionaryRef*) &assertionsByPid);

    // walk assertions per PID
    for(NSString *pid in [assertionsByPid allKeys]) {
        // loop through the assertions for this PID
        for (id object in assertionsByPid[pid]) {
            // track PIDs that are preventing the screen from going to sleep
            if (
                [object[@"AssertType"] isEqualToString: @"NoDisplaySleepAssertion"] ||
                [object[@"AssertType"] isEqualToString: @"PreventUserIdleDisplaySleep"]
            ) {
                [screenNoSleepPIDS addObject:pid];
            }
            // track PIDs that are preventing the system from going to sleep
            else if (
                [object[@"AssertType"] isEqualToString: @"NoIdleSleepAssertion"] ||
                [object[@"AssertType"] isEqualToString: @"PreventSystemSleep"] ||
                [object[@"AssertType"] isEqualToString: @"PreventUserIdleSystemSleep"]
            ) {
                [systemNoSleepPIDS addObject:pid];
            }
        }
     }

    // build the dictionary of return values
    NSDictionary *PIDAssertions = @{
        @"screen": screenNoSleepPIDS,
        @"system": systemNoSleepPIDS,
    };

    return PIDAssertions;
}

// Function to extract data from a windowInfo
Napi::Object CreateDesktopWindow(Napi::Env env, NSDictionary *windowInfo, NSDictionary *PIDAssertions, int foregroundPID) {
    Napi::Object desktopWindow = Napi::Object::New(env);

    // set process name
    NSString *windowOwnerName = windowInfo[(id)kCGWindowOwnerName];
    desktopWindow.Set("name", std::string([windowOwnerName UTF8String]));

    // set window name
    NSString *windowName = windowInfo[(id)kCGWindowName];
    desktopWindow.Set("titleBar", std::string([windowName UTF8String]));

    // set window PID
    NSString *windowOwnerPID = windowInfo[(id)kCGWindowOwnerPID];
    desktopWindow.Set("pid", [windowOwnerPID intValue]);

    // set window layer
    NSNumber *windowLayer = windowInfo[(id)kCGWindowNumber];
    desktopWindow.Set("id", [windowLayer intValue]);

    // set window size
//     CGRect windowRect;
//     CGRectMakeWithDictionaryRepresentation((__bridge CFDictionaryRef)(windowInfo[(id)kCGWindowBounds]), &windowRect);
//     desktopWindow.Set("width", floor(windowRect.size.width));
//     desktopWindow.Set("height", floor(windowRect.size.height));

    // Set the flags isFocus, preventsScreenSleep, preventsSystemSleep
    desktopWindow.Set("isFocus",             Napi::Boolean::New(env, (foregroundPID == [windowOwnerPID intValue])));
    desktopWindow.Set("preventsScreenSleep", Napi::Boolean::New(env, [PIDAssertions[@"screen"] containsObject: windowOwnerPID]));
    desktopWindow.Set("preventsSystemSleep", Napi::Boolean::New(env, [PIDAssertions[@"system"] containsObject: windowOwnerPID]));

    return desktopWindow;
}

// Fetch windows from mac os and return them to JS
Napi::Promise FetchDesktopWindows(const Napi::CallbackInfo &info) {
    Napi::Env env = info.Env();
    Napi::Promise::Deferred deferred = Napi::Promise::Deferred::New(env);

    // skip known internal processes such as the system dock
    NSArray *systemWindows;
    systemWindows = [NSArray arrayWithObjects:
        @"com.apple.appkit.xpc.openAndSav",
        @"Activity Monitor",
        @"Control Center",
        @"Dock",
        @"Finder",
        @"loginwindow",
        @"Notification Center",
        @"Spotlight",
        @"SystemUIServer",
        @"TextInputMenuAgent",
        @"TextInputSwitcher",
        @"Window Server",
        @"coreautha",
        @"com.apple.preference.security.r",
        nil];


    // pull the window list from the OS
    CFArrayRef windowList;
    windowList = CGWindowListCopyWindowInfo(kCGWindowListOptionAll, kCGNullWindowID);
    int numberOfWindows = CFArrayGetCount(windowList);

    // pull the PIDs with assertions
    NSDictionary *PIDAssertions = FindPIDAssertions();

    // build an array to store the windows that meet the criteria for looking relevant
    NSMutableArray *relevantWindowList = [NSMutableArray array];

    // track the PID of the current foreground process to figure out what windows are in focus
    pid_t foregroundPID = NSWorkspace.sharedWorkspace.frontmostApplication.processIdentifier;

    // loop the active windows and filter down to the ones that look relevant
    for (int windowIndex = 0; windowIndex < numberOfWindows; windowIndex++) {
        // extract the properties of windowInfo used to skip some irrelevant windows
        NSDictionary *windowInfo = (NSDictionary *)CFArrayGetValueAtIndex(windowList, windowIndex);
        NSNumber *windowAlpha = windowInfo[(id)kCGWindowAlpha];
        NSString *windowOwnerName = windowInfo[(id)kCGWindowOwnerName];
        NSString *windowName = windowInfo[(id)kCGWindowName];

        // skip transparent windows
        if ([windowAlpha floatValue] == 0) {
            continue;
        }

        // NOTE: we can not skip windows that are too small as sometimes they give us signal of conferencing apps running in the background
//         CGRect windowRect;
//         CGRectMakeWithDictionaryRepresentation((__bridge CFDictionaryRef)(windowInfo[(id)kCGWindowBounds]), &windowRect);
//         if (windowRect.size.width < MIN_WINDOW_SIZE || windowRect.size.height < MIN_WINDOW_SIZE) {
//             continue;
//         }

        // skip processes w/o names
        if (windowOwnerName == nil || ([windowOwnerName length] <= 0)) {
            continue;
        }

        // skip windows w/o names
        if (windowName == nil || ([windowName length] <= 0)) {
            continue;
        }

        if ([systemWindows containsObject: windowOwnerName]) {
            continue;
        }

        [relevantWindowList addObject: windowInfo];
    }

    // build node array from the relevant windows
    Napi::Array returnWindowList = Napi::Array::New(env, [relevantWindowList count]);

    // populate node array from the relevant windows
    int returnWindowIndex = 0;
    NSDictionary *windowInfo;

    // build the list of Napi::Object windows being returned
    for (windowInfo in relevantWindowList) {
        returnWindowList[returnWindowIndex++] = CreateDesktopWindow(env, windowInfo, PIDAssertions, foregroundPID);
    }

    // free memory
    CFRelease(windowList);

    deferred.Resolve(returnWindowList);
    return deferred.Promise();
}


// Initializes functions exposed to JS
Napi::Object Init(Napi::Env env, Napi::Object exports) {
    exports.Set(Napi::String::New(env, "fetchDesktopWindows"), Napi::Function::New(env, FetchDesktopWindows));
    return exports;
}

NODE_API_MODULE(app_tracker, Init)
