(* ::Package:: *)

BeginPackage["KeyEvents`", {"JLink`", "Player`", "SpaceChase`", "Environment`"}]

initJLink::usage="initJLink[] initializes JLink, along with the things necessary for key event handling."

initFrame::usage="initFrame[] creates a new key event window."
showFrame::usage="showFrame[] brings the key event window into focus."
destroyFrame::usage="destroyFrame[] hides the key event window."

keyUp
keyDown

Begin["Private`"]

keyUp[e_,char_,code_]:=Module[
	{},
	(*Callback function for JLink key event.*)

	mapKeyUp[code]
]

mapKeyUp[code_]:=Module[
	{},
	(*Perform an action based on which key was released.*)

	Switch[code,
		KeyEvent`VKUUP, playerAccelerating = False,
		KeyEvent`VKULEFT, playerAngMom = 0,
		KeyEvent`VKURIGHT, playerAngMom = 0
	]
]

keyDown[e_,char_,code_]:=Module[
	{},
	(*Callback function for JLink key event.*)

	mapKeyDown[code];
]

mapKeyDown[code_]:=Module[
	{},
	(*Perform an action based on which key was pressed.*)

	Switch[code,
		KeyEvent`VKUESCAPE,
			If[gameOver,
				SpaceChase`mainMenu[],
				SpaceChase`continueMenu[]
			],
		KeyEvent`VKUUP,accelerate[],
		KeyEvent`VKURIGHT,rotateRight[],
		KeyEvent`VKULEFT,rotateLeft[]
	]
]



keyTyped[e_,char_,code_]:=Module[
	{},
	(*Callback function for JLink key event.
		Does nothing, but it was necessary to
		provide a function for each event.*)

	Return[0];
]

initJLink[]:=Module[
	{},
	(*Initialize the JVM, as well as some of the classes we use.*)

	ReinstallJava[];
	LoadJavaClass["java.lang.System"];
	LoadJavaClass["java.awt.event.KeyEvent"];
]

initListener[]:=Module[
	{},
	(*Create a new key listener.*)

	listenerClass=LoadJavaClass["com.wolfram.jlink.MathKeyListener"];
	listener=JavaNew[listenerClass,{{"keyPressed","KeyEvents`keyDown"},{"keyReleased","KeyEvents`keyUp"},{"keyTyped","KeyEvents`keyTyped"}}];
]

initFrame[]:=Module[
	{},
	(*Create a new key event window with a listener.*)

	initListener[];
	frame=JavaNew["com.wolfram.jlink.MathFrame"];
	frame@addKeyListener[listener];
	showFrame[];
]

showFrame[] := JavaShow[frame]
	(*Bring the key event window to focus.*)

destroyFrame[]:=Module[
	{},
	(*Hide the key event window.*)

	frame@setVisible[False]
]

End[]

EndPackage[]



