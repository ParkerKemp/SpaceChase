(* ::Package:: *)

BeginPackage["Environment`"]

windowSize
environmentSize
environmentScale
shuttleSize
frameRate
animationRate
level
textObjects
textSize
textOpacity
textMax
textColor
textValue
textCenter
textBounce
textIncrement
gameOver

Begin["Private`"]

windowSize = {800, 600};
environmentScale = 3;
shuttleSize = 60.0 / environmentScale;
environmentSize = windowSize * environmentScale;
frameRate = 30;
animationRate = 30;
level = 1;
textSize = 1;
textIncrement = 2;
textMax = 3;
textOpacity = 4;
textValue = 5;
textBounce = 6;
textColor = 7;
textCenter = 8;
levelText = {0, 0, 40, 0, "asdf", True, White, environmentSize / 2};
textObjects = {levelText, levelText};
gameOver = False;

End[]

EndPackage[]
