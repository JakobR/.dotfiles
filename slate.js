var hyper = function(key) {
  return key + ":cmd,ctrl,alt,shift";
};

var fullscreen = slate.operation("move", {
  "x": "screenOriginX",
  "y": "screenOriginY",
  "width": "screenSizeX",
  "height": "screenSizeY"
});

var pushLeft = slate.operation("push", {
  "direction": "left",
  "style": "bar-resize:screenSizeX/2"
});
var pushRight = pushLeft.dup({"direction": "right"});

slate.bind("up:ctrl", fullscreen);
//slate.bind("left:ctrl", pushLeft);
//slate.bind("right:ctrl", pushRight);

var hint = slate.operation("hint", {
  "characters": "ASDFGHJKLQWERTYUIOPCVBN"
});
slate.bind("f19", hint);

slate.bind(hyper("left"),  slate.operation("focus", {"direction": "left"}));
slate.bind(hyper("right"), slate.operation("focus", {"direction": "right"}));
slate.bind(hyper("up"),    slate.operation("focus", {"direction": "up"}));
slate.bind(hyper("down"),  slate.operation("focus", {"direction": "down"}));

// Returns an operation that switches to an application
// - appName: string, the name of the application to switch to (e.g. "Opera")
// - focusOnly: undefined, or boolean. Use a "focus" operation if true, and "open -a" otherwise.
// TODO: When it's already focused? Switching windows inside app might be most useful (there's Cmd+` for this, but it's harder to reach)
var focus = function(appName, focusOnly) {
  if (focusOnly == true) {
    return slate.operation("focus", {"app": appName});
  }
  else {
    return function(win) {
      slate.shell("/usr/bin/open -a '" + appName + "'", false);
    }
  }
}

slate.bind(hyper("d"), focus("Finder"));
slate.bind(hyper("p"), focus("Preview"));
slate.bind(hyper("v"), focus("MacVim"));
slate.bind(hyper("e"), focus("iTerm"));
slate.bind(hyper("x"), focus("Skype"));
slate.bind(hyper("t"), focus("iTunes"));
slate.bind(hyper("a"), focus("Anki"));
slate.bind(hyper("o"), focus("Opera"));
slate.bind(hyper("m"), focus("Mail"));
slate.bind(hyper("c"), focus("Calendar"));
slate.bind(hyper("s"), focus("Google Chrome"));
slate.bind(hyper("f"), focus("Firefox"));
slate.bind(hyper("j"), focus("JEDict"));

//slate.config("windowHintsIgnoreHiddenWindows", false);
//slate.config("windowHintsShowIcons", true);
slate.config("focusPreferSameApp", false);

// direction: 0=left, 1=top, 2=right, 3=bottom
var tolerance = function(win, direction) {
  var app = win.app();
  if (app.name() == "MacVim") {
    return 20;
  }
  return 5;
}

var isWindowOnLeftHalf = function(win) {
  var win_rect = win.rect();
  var screen_rect = win.screen().visibleRect();

  return Math.abs(win_rect.x - screen_rect.x) < tolerance(win,0) &&
    Math.abs(win_rect.y - screen_rect.y) < tolerance(win,1) &&
    Math.abs(win_rect.width - screen_rect.width/2) < tolerance(win,2) &&
    Math.abs(win_rect.height - screen_rect.height) < tolerance(win,3);
}

var isWindowOnRightHalf = function(win) {
  var win_rect = win.rect();
  var screen_rect = win.screen().visibleRect();

  return Math.abs(win_rect.x - screen_rect.x - screen_rect.width/2) < tolerance(win,0) &&
    Math.abs(win_rect.y - screen_rect.y) < tolerance(win,1) &&
    Math.abs(win_rect.width - screen_rect.width/2) < tolerance(win,2) &&
    Math.abs(win_rect.height - screen_rect.height) < tolerance(win,3);
}

var cycleLeft = function(win) {
  if (win == undefined) {
    // this check is true for both undefined and null
    // this happens for example if the spotlight text field is focused
    return;
  }
  if (isWindowOnLeftHalf(win)) {
    var currentScreen = win.screen().id();
    var targetScreen = currentScreen - 1;
    if (targetScreen < 0) {
      targetScreen = slate.screenCount() - 1;
    }
    win.doOperation(slate.operation('throw', {
      'screen': targetScreen,
      "x": "screenOriginX+screenSizeX/2",
      "y": "screenOriginY",
      "width": "screenSizeX/2",
      "height": "screenSizeY"
    }));
  } else {
    win.doOperation(pushLeft);
  }
};

var cycleRight = function(win) {
  if (win == undefined) {
    // this check is true for both undefined and null
    // this happens for example if the spotlight text field is focused
    return;
  }
  if (isWindowOnRightHalf(win)) {
    var currentScreen = win.screen().id();
    var targetScreen = currentScreen + 1;
    if (targetScreen >= slate.screenCount()) {
      targetScreen = 0;
    }
    win.doOperation(slate.operation('throw', {
      'screen': targetScreen,
      "x": "screenOriginX",
      "y": "screenOriginY",
      "width": "screenSizeX/2",
      "height": "screenSizeY"
    }));
  } else {
    win.doOperation(pushRight);
  }
};

slate.bind("left:ctrl", cycleLeft);
slate.bind("right:ctrl", cycleRight);

// Helper functions for debugging
/*
slate.bind("1:ctrl", function(win) {
  var rect = win.rect();
  slate.log('win    x='+rect.x+' y='+rect.y+' w='+rect.width+' h='+rect.height);
  var srect = win.screen().visibleRect();
  slate.log('screen x='+srect.x+' y='+srect.y+' w='+srect.width+' h='+srect.height);
  slate.log('isLeft = '+isWindowOnLeftHalf(win));
  slate.log('isRight = '+isWindowOnRightHalf(win));
  var app = win.app();
  slate.log('app name="'+app.name()+'"');
});

slate.bind("0:ctrl", slate.operation("relaunch"));
// */
