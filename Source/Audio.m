(* ::Package:: *)

BeginPackage["Audio`"]

importSounds::usage="importSounds[] imports all audio files used in Space Chase."
playExplosion::usage="playExplosion[] plays the explosion sound effect."
playWarp::usage="playWarp[] plays the warp-in sound effect."

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
