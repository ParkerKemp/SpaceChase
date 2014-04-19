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
newGame

initFrame
initListener

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
	destroyFrame[];
	Pause[1];
	Switch[ChoiceDialog["Space Chase", {"New Game"->1,"Exit"->2}, Modal->True],
		1, newGame[],
		2, exitGame[]
	]
]

newGame[]:=Module[
{},
	Global`playerLocation = Global`environmentSize / 2;
	Global`playerVelocity = {0,0};
	Global`playerAcceleration = {0,0};
	Global`playerRotation = 0;
	Global`playerAngularMomentum = 0;
	Global`accelerating = False;
	initFrame[];
]

exitGame[]:=Module[
{},
RemoveScheduledTask[ScheduledTasks[]];
NotebookClose[Global`nb];
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

initListener[]:=Module[
{},
listenerClass=LoadJavaClass["com.wolfram.jlink.MathKeyListener"];
listener=JavaNew[listenerClass,{{"keyPressed","KeyEvents`keyDown"},{"keyReleased","KeyEvents`keyUp"},{"keyTyped","KeyEvents`keyTyped"}}];
]

initFrame[]:=Module[
{},
frame=JavaNew["com.wolfram.jlink.MathFrame"];
frame@addKeyListener[listener];
frame@setModal[];
JavaShow[frame];
(*ShowJavaConsole[];*)
]

destroyFrame[]:=Module[
{},
frame@setVisible[False];
frame@dispose[];
]

drawPlayer[]:=Module[
{},
System`out@println["Drawing player..."];
]

End[]

EndPackage[]



