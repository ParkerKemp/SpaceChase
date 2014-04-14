(* ::Package:: *)

BeginPackage["KeyEvents`"]
<< JLink`

initJLink::usage="Initializes JLink, along with the things necessary for key event handling."

keyDown
keyUp
keyTyped
moveUp

initWindow::usage="Create the main window. Must be called after initJLink."

startGameLoop::usage="Starts the game loop."

Begin["Private`"]

keyDown[e_,char_,code_]:=Module[
{},
System`out@println["Key pressed."];
Print[FullForm[code]];
mapKeyDown[code];
]

mapKeyDown[code_]:=Module[
{},
Switch[code,
	KeyEvent`VKUESCAPE,KeyEvents`mainMenu[],
	KeyEvent`VKUUP,KeyEvents`moveUp[]
	]
]

moveUp[]:=Module[
{},
	System`out@println["Movin' on up!"];
	Global`playerLocation[[2]] += 3;
]

mainMenu[]:=Module[
{},
	Switch[ChoiceDialog["Space Chase", {"New Game"->1,"Exit"->2}],
		1,Print["New game chosen."],
		2,Print["Exit chosen."]
	]
]

keyUp[e_,char_,code_]:=Module[
{},
System`out@println["Key released."];
]

keyTyped[e_,char_,code_]:=Module[
{},
System`out@println["Key typed."];
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
ShowJavaConsole[];
]

drawPlayer[]:=Module[
{},
System`out@println["Drawing player..."];
]

startGameLoop[]:=Module[
{},
RunScheduledTask[drawScene[], 0.05]
]

End[]

EndPackage[]
