(* ::Package:: *)

BeginPackage["Audio`", {"Parallel`"}]

importSounds
playExplosion
playWarp

Begin["Private`"]

importSounds[] := Module[
	{},
	explosionSound = Import["explosion1.wav"];
	warpSound = Import["warp.wav"];
]

playExplosion[] := Module[
	{},
	EmitSound[explosionSound]
]

playWarp[] := Module[
	{},
	EmitSound[warpSound]
]

End[]

EndPackage[]
