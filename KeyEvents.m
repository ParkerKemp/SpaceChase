(* ::Package:: *)

BeginPackage["KeyEvents`"]
<< JLink`

initJLink::usage="Initializes JLink, along with the things necessary for key event handling."

keyDown

initWindow::usage="Create the main window. Must be called after initJLink."

startGameLoop::usage="Starts the game loop."

Begin["Private`"]

keyDown[e_,char_,code_]:=Module[
{},
System`out@println["Key pressed."];
Global`playerLocation[[2]]-=5;
]

initJLink[]:=Module[
{},
ReinstallJava[];
LoadJavaClass["java.lang.System"];
]

initWindow[]:=Module[
{},
listenerClass=LoadJavaClass["com.wolfram.jlink.MathKeyListener"];
listener=JavaNew[listenerClass,{{"keyPressed","KeyEvents`keyDown"},{"keyReleased","KeyEvents`keyDown"},{"keyTyped","KeyEvents`keyDown"}}];

frame=JavaNew["com.wolfram.jlink.MathFrame"];
frame@addKeyListener[listener];
JavaShow[frame];
(*ShowJavaConsole[];*)
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
