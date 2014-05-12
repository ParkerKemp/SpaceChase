(* ::Package:: *)

BeginPackage["Audio`", {"Parallel`"}]

importSounds
playExplosion
playWarp

Begin["Private`"]

importSounds[] := Module[
	{},
	explosionSound = Import["Audio/explosion1.wav"];
	warpSound = Import["Audio/warp.wav"];
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
