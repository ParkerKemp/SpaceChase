(* ::Package:: *)

BeginPackage["AI`", {"Player`", "Environment`", "Geometry`"}]

initAliens::usage="initAliens[] initializes the alien attribute lists."
updateAliens::usage="updateAliens[] updates all alien attributes for one frame."
spawnNewAlien::usage="spawnNewAlien[{x, y}, mode] spawns an alien at {x, y} with AI mode."
randomAlienPos::usage="randomAlienPos[] returns a random position in the environment."
deleteAlien::usage="deleteAlien[index] deletes all attributes associated with the alien at index."

playerPath
numAliens
alienPos
alienRot
warpRadius
warpIncrement
warpCenter
warpOpacity

Begin["Private`"]

initAliens[] := Module[
	{i},
	(*Initialize all alien attribute lists.*)

	(*alienSpeed is 8.0 by default, but can be reduced to
		make the game easier for testing purposes. We recommend
		2.0 or 3.0.*)
	alienSpeed = 8.0;
	
	playerPath = {{0,0}};
	numAliens = 0;
	alienMode = {};
	alienPos = {};
	alienVel = {};
	alienDest = {};
	alienRot = {};
	alienSpeedFactor = {};
	pAcc = {0, 0};
	warpCenter = {0, 0};
	warpRadius = 0;
	warpIncrement = 0;
	warpOpacity = 0;
]

deleteAlien[index_] := Module[
	{},
	(*Delete the alien at index by deleting all its attributes.*)

	alienMode = Delete[alienMode, index];
	alienPos = Delete[alienPos, index];
	alienVel = Delete[alienVel, index];
	alienDest = Delete[alienDest, index];
	alienRot = Delete[alienRot, index];
	alienSpeedFactor = Delete[alienSpeedFactor, index];
	numAliens--;
]

spawnNewAlien[newPos_, mode_] := Module[
	{},
	(*Spawn a new alien at newPos with given AI mode (1 = smart, 2 = dumb).*)
	
	numAliens++;
	AppendTo[alienPos, newPos];
	AppendTo[alienVel, {0, 0}];
	AppendTo[alienDest, {0, 0}];
	AppendTo[alienRot, 0];
	AppendTo[alienMode, mode];
	AppendTo[alienSpeedFactor, RandomReal[]/5+0.9];
]

randomAlienPos[] := {RandomInteger[{0, environmentSize[[1]]}], RandomInteger[{0, environmentSize[[2]]}]};
(*Return a random position in the environment.*)

updateAliens[] := Module[
	{i},
	(*Update all alien attributes.*)

	(*Only update the alien's direction vector if not 
		game over (i.e. if the game is in progress).*)
	If[!gameOver,
		playerPath = futurePath[];
		updateAIVectors[],
		playerPath = {{0,0}};
	];
	
	(*Increment all alien positions and rotations.*)
	For[i = 1, i <= numAliens, i++,
		alienPos[[i]] = Mod[alienPos[[i]] + alienVel[[i]], environmentSize];
		alienRot[[i]] = angleFromVector[alienVel[[i]]];
	]
]

futurePath[] := Module[
	{i, v = playerVel, p = playerPos, a = playerAcc, retList = {}},
	(*Return a chronological list of player's next 100 future positions, assuming
		the same acceleration.*)

	For[i = 1, i <= 100, i++,
		AppendTo[retList, p = Mod[p + (v += a), environmentSize]];
	];
	Return[retList]
]

getAIDestination[index_] := Module[
	{i},
	(*Return the next destination for the alien at index, based
		on its AI mode and player's position or future positions.*)
	
	(*AlienMode of 1 is intelligent, 2 is dumb*)
	If[alienMode[[index]] == 1,
		For[i = 1, i < Length[playerPath], i++,
			If[minDistance[alienPos[[index]], playerPath[[i]]] / (alienSpeed * alienSpeedFactor[[index]]) < i,
				Return[playerPath[[i]]]
			]
		],
		Return[playerPos]
	];
	
	Return[playerPath[[Length[playerPath]]]]
]

updateAIVectors[]:=Module[
	{i},
	(*Update all alien direction vectors based on their respective destinations.*)

	For[i = 1, i <= numAliens, i++,
		alienDest[[i]] = getAIDestination[i];
		alienVel[[i]] = unitVector[vectorToPoint[alienPos[[i]], alienDest[[i]]]] * (alienSpeed * alienSpeedFactor[[i]]);
	]
]

End[]

EndPackage[]
