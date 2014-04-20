(* ::Package:: *)

BeginPackage["KeyEvents`", {"JLink`", "Player`", "SpaceChase`"}]

initJLink::usage="Initializes JLink, along with the things necessary for key event handling."

keyDown
keyUp
keyTyped
moveUp
moveDown
moveRight
moveLeft
newGame

destroyFrame

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
	KeyEvent`VKUUP, playerAccelerating = False,
	KeyEvent`VKULEFT, playerAngMom = 0,
	KeyEvent`VKURIGHT, playerAngMom = 0
	]
]

keyDown[e_,char_,code_]:=Module[
{},
mapKeyDown[code];
]

mapKeyDown[code_]:=Module[
{},
Switch[code,
	KeyEvent`VKUESCAPE,mainMenu[],
	KeyEvent`VKUUP,accelerate[],
	KeyEvent`VKURIGHT,rotateRight[],
	KeyEvent`VKULEFT,rotateLeft[]
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

initListener[]:=Module[
{},
listenerClass=LoadJavaClass["com.wolfram.jlink.MathKeyListener"];
listener=JavaNew[listenerClass,{{"keyPressed","KeyEvents`keyDown"},{"keyReleased","KeyEvents`keyUp"},{"keyTyped","KeyEvents`keyTyped"}}];
]

initFrame[]:=Module[
{},
frame=JavaNew["com.wolfram.jlink.MathFrame"];
frame@addKeyListener[listener];
JavaShow[frame];
(*ShowJavaConsole[];*)
]

destroyFrame[]:=Module[
{},
frame@setVisible[False];
frame@dispose[];
]

End[]

EndPackage[]



