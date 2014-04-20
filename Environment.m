(* ::Package:: *)

BeginPackage["Environment`"]

windowSize
environmentSize
environmentScale
shuttleSize
frameRate

Begin["Private`"]

windowSize = {800, 600};
environmentScale = 4;
shuttleSize = 60.0 / environmentScale;
environmentSize = windowSize * environmentScale;
frameRate = 30;

End[]

EndPackage[]
