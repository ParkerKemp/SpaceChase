(* ::Package:: *)

BeginPackage["Audio`", {"Parallel`"}]

importSounds
playExplosion

Begin["Private`"]

importSounds[] := Module[
	{},
	explosionSound = Import["explosion1.wav"]
]

playExplosion[] := Module[
	{},
	EmitSound[explosionSound]
]

End[]

EndPackage[]
