(* ::Package:: *)

BeginPackage["Player`", {"Environment`"}]

initPlayer
updatePlayer
accelerate
rotateLeft
rotateRight

playerPos
playerVel
playerRot
playerAcc
playerAngMom
playerAccelerating

Begin["Private`"]

initPlayer[environmentSize_] := Module[
	{},
	playerPos = environmentSize / 2;
	playerVel = {0, 0};
	playerAcc = {0, 0};
	playerRot = 0;
	playerAngMom = 0;
	playerAccelerating = False;
	playerAngularSpeed = \[Pi] / 18;
	playerSpeed = 0.5;
]

updatePlayer[] := Module[
	{},
	If[playerAccelerating,
		playerAcc = {playerSpeed * Cos[playerRot], playerSpeed * Sin[playerRot]},
		playerAcc = {0,0}
	];
	playerRot += playerAngMom;
	playerVel += playerAcc;
	playerPos = Mod[playerPos + playerVel, environmentSize];
]

accelerate[] := playerAccelerating = True;

rotateRight[] := playerAngMom = -playerAngularSpeed;

rotateLeft[] := playerAngMom = playerAngularSpeed;

End[]

EndPackage[]
