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
slate.bind("left:ctrl", pushLeft);
slate.bind("right:ctrl", pushRight);

/*
 * TODO:
 * Custom function for Ctrl+Left etc. to move window to other screen
 * if it already is at the left half of the screen.
 * (like in Windows 7)
 */
