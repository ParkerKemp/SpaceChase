(* ::Package:: *)

BeginPackage["Player`", {"Environment`"}]

initPlayer::usage="initPlayer[environmentSize] initializes all player attributes, scaled with environmentSize."
updatePlayer::usage="updatePlayer[] increments all player attributes for one frame."
accelerate::usage="accelerate[] sets the acceleration flag for player."
rotateLeft::usage="rotateLeft[] sets the left rotation flag for player."
rotateRight::usage="rotateRight[] sets the right rotation flag for player."

playerPos
playerVel
playerRot
playerAcc
playerAngMom
playerAccelerating

Begin["Private`"]

initPlayer[environmentSize_] := Module[
	{},
	(*Initialize player attributes with default values.*)

	playerPos = environmentSize / 2;
	playerVel = {0, 0};
	playerAcc = {0, 0};
	playerRot = 0;
	playerAngMom = 0;
	playerAccelerating = False;
	playerAngularSpeed = \[Pi] / 18;
	playerSpeed = 0.8;
]

updatePlayer[] := Module[
	{},
	(*Update player attributes for one frame.*)

	(*playerAccelerating flag is set asynchronously by the
		key event listener. This flag serves as a buffer so
		the player's acceleration vector is updated
		synchronously with the game loop.*)
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
