(* ::Package:: *)

BeginPackage["Environment`"]

windowSize
environmentSize
environmentScale
shuttleSize
frameRate
level
textSize
textIncrement
gameOver

Begin["Private`"]

windowSize = {800, 600};
environmentScale = 3;
shuttleSize = 60.0 / environmentScale;
environmentSize = windowSize * environmentScale;
frameRate = 30;
level = 1;
textSize = 0;
textIncrement = 0;
gameOver = False;

End[]

EndPackage[]
