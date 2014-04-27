(* ::Package:: *)

BeginPackage["AI`", {"Player`", "Environment`", "Geometry`"}]

initAliens
updateAliens
numAliens
alienPos
alienRot

Begin["Private`"]

initAliens[] := Module[
	{i},
	numAliens = 3;
	alienMode = {};
	alienPos = {};
	alienVel = {};
	alienDest = {};
	alienRot = {};
	alienSpeed = 12.0;
	
	pAcc = {0, 0};
	
	For[i = 0, i < numAliens, i++,
		AppendTo[alienPos, {RandomInteger[{0, environmentSize[[1]]}], RandomInteger[{0, environmentSize[[2]]}]}];
		AppendTo[alienVel, {0, 0}];
		AppendTo[alienDest, {0, 0}];
		AppendTo[alienRot, 0];
		AppendTo[alienMode, 1];
	];

	alienMode[[2]] = 2;
	alienMode[[3]] = 2;
]

updateAliens[] := Module[
	{i},
	(*If[pAcc != playerAcc,
		pAcc = playerAcc;*)
		playerPath = futurePath[];
		updateAIVectors[];
	(*];*)

	For[i = 1, i <= numAliens, i++,
		alienPos[[i]] = Mod[alienPos[[i]] + alienVel[[i]], environmentSize];
		alienRot[[i]] = angleFromVector[alienVel[[i]]];
	]
]

futurePath[]:=Module[
	{v = playerVel, p = playerPos, a = playerAcc},
	Return[Table[p = Mod[p + (v += a), environmentSize], {x, 1, 150}]
	]
]
getAIDestination[index_]:=Module[
	{i},
	If[alienMode[[index]] == 1,
		For[i = 1, i < Length[playerPath], i++,
			If[minDistance[alienPos[[index]],playerPath[[i]]]/alienSpeed < i,
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
		alienVel[[i]] = unitVector[vectorToPoint[alienPos[[i]], alienDest[[i]]]] * alienSpeed;
		(*variance = Random[] * 0.10 + 0.95;
		alienVel[[i]][[1]] *= variance;
		alienVel[[i]][[2]] /= variance;*)
	]
]

End[]

EndPackage[]
