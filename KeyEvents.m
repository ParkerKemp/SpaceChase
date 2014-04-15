(* ::Package:: *)

BeginPackage["KeyEvents`"]
<< JLink`

initJLink::usage="Initializes JLink, along with the things necessary for key event handling."

keyDown
keyUp
keyTyped
moveUp
moveDown
moveRight
moveLeft

initWindow::usage="Create the main window. Must be called after initJLink."

startGameLoop::usage="Starts the game loop."

Begin["Private`"]

keyUp[e_,char_,code_]:=Module[
{},
mapKeyUp[code]
]

mapKeyUp[code_]:=Module[
{},
Switch[code,
	KeyEvent`VKUUP, Global`accelerating = False,
	KeyEvent`VKULEFT, Global`playerAngularMomentum = 0,
	KeyEvent`VKURIGHT, Global`playerAngularMomentum = 0
	]
]

keyDown[e_,char_,code_]:=Module[
{},
mapKeyDown[code];
]

mapKeyDown[code_]:=Module[
{},
Switch[code,
	KeyEvent`VKUESCAPE,KeyEvents`mainMenu[],
	KeyEvent`VKUUP,KeyEvents`moveUp[],
	KeyEvent`VKUDOWN,KeyEvents`moveDown[],
	KeyEvent`VKURIGHT,KeyEvents`moveRight[],
	KeyEvent`VKULEFT,KeyEvents`moveLeft[]
	]
]

moveUp[]:=Module[
{},
	Global`accelerating = True;
]

moveDown[]:=Module[
{},
Return[0];
]

moveRight[]:=Module[
{},
	Global`playerAngularMomentum = -Global`angularSpeed;
]

moveLeft[]:=Module[
{},
	Global`playerAngularMomentum = Global`angularSpeed;
]

mainMenu[]:=Module[
{},
	Switch[ChoiceDialog["Space Chase", {"New Game"->1,"Exit"->2}],
		1,Print["New game chosen."],
		2,Print["Exit chosen."]
	]
]

keyTyped[e_,char_,code_]:=Module[
{},
Return[0];
]

initJLink[]:=Module[
{},
ReinstallJava[];
LoadJavaClass["java.lang.System"];
LoadJavaClass["java.awt.event.KeyEvent"];
]

initWindow[]:=Module[
{},
listenerClass=LoadJavaClass["com.wolfram.jlink.MathKeyListener"];
listener=JavaNew[listenerClass,{{"keyPressed","KeyEvents`keyDown"},{"keyReleased","KeyEvents`keyUp"},{"keyTyped","KeyEvents`keyTyped"}}];

frame=JavaNew["com.wolfram.jlink.MathFrame"];
frame@addKeyListener[listener];
JavaShow[frame];
(*ShowJavaConsole[];*)
]

drawPlayer[]:=Module[
{},
System`out@println["Drawing player..."];
]

End[]

EndPackage[]



