(* ::Package:: *)

BeginPackage["AI`", {"Player`", "Environment`", "Geometry`"}]

initAliens
updateAliens
numAliens
alienPos

Begin["Private`"]
AI`location={10,10};

initAliens[] := Module[
	{i},
	numAliens = 1;
	alienPos = {};
	alienVel = {};
	alienDest = {};
	alienSpeed = 12.0;
	
	For[i = 0, i < numAliens, i++,
		AppendTo[alienPos, {0, 0}];
		AppendTo[alienVel, {0, 0}];
		AppendTo[alienDest, {0, 0}];
	]
]

updateAliens[] := Module[
	{i},
	playerPath = futurePath[];
	updateAIVectors[];
	For[i = 1, i <= numAliens, i++,
		alienPos[[i]] = Mod[alienPos[[i]] + alienVel[[i]], environmentSize];
	]
]

futurePath[]:=Module[
	{v = playerVel, p = playerPos, a = playerAcc},
	Return[Table[p = Mod[p + (v += a), environmentSize], {x, 1, 60}]
	]
]
getAIDestination[index_]:=Module[
	{i},
	For[i = 1, i < Length[playerPath], i++,
		If[minDistance[alienPos[[index]],playerPath[[i]]]/alienSpeed < i,
			Return[playerPath[[i]]]
		]
	];
	Return[playerPath[[Length[playerPath]]]]
]

updateAIVectors[]:=Module[
	{i},
	For[i = 1, i <= numAliens, i++,
		alienDest[[i]] = getAIDestination[i];
		alienVel[[i]] = unitVector[vectorToPoint[alienPos[[i]], alienDest[[i]]]] * alienSpeed;
	]
]

End[]

EndPackage[]
