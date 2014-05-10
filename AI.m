(* ::Package:: *)

BeginPackage["AI`", {"Player`", "Environment`", "Geometry`"}]

initAliens
updateAliens
spawnNewAlien
beginWarp

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
	numAliens = 0;
	alienMode = {};
	alienPos = {};
	alienVel = {};
	alienDest = {};
	alienRot = {};
	alienSpeedFactor = {};
	alienSpeed = 8.0;
	pAcc = {0, 0};
	warpCenter = {0, 0};
	warpRadius = 0;
	warpIncrement = 0;
	warpOpacity = 0;
]

spawnNewAlien[mode_] := Module[
	{},
		numAliens++;
		AppendTo[alienPos, newPos];(*playerPos + environmentSize/2];*)
		AppendTo[alienVel, {0, 0}];
		AppendTo[alienDest, {0, 0}];
		AppendTo[alienRot, 0];
		AppendTo[alienMode, mode];
		AppendTo[alienSpeedFactor, RandomReal[]/5+0.9];
]

beginWarp[] := Module[
	{},
	newPos = {RandomInteger[{0, environmentSize[[1]]}], RandomInteger[{0, environmentSize[[2]]}]};		
	warpCenter = newPos;
	warpIncrement = 10;
	warpOpacity = 1;
]

updateAliens[] := Module[
	{i},
		playerPath = futurePath[];
		updateAIVectors[];

	For[i = 1, i <= numAliens, i++,
		alienPos[[i]] = Mod[alienPos[[i]] + alienVel[[i]], environmentSize];
		alienRot[[i]] = angleFromVector[alienVel[[i]]];
	]
]

futurePath[] := Module[
	{i, v = playerVel, p = playerPos, a = playerAcc, retList = {}},
	For[i = 1, i <= 30, i++,
		AppendTo[retList, p = Mod[p + (v += a), environmentSize]];
	];
	
	For[i = 1, i <= 100, i++,
		AppendTo[retList, p = Mod[p + v, environmentSize]];
	];
	Return[retList]
]

getAIDestination[index_] := Module[
	{i},
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
	For[i = 1, i <= numAliens, i++,
		alienDest[[i]] = getAIDestination[i];
		alienVel[[i]] = unitVector[vectorToPoint[alienPos[[i]], alienDest[[i]]]] * (alienSpeed * alienSpeedFactor[[i]]);
	]
]

End[]

EndPackage[]
