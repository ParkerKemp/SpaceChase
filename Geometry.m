(* ::Package:: *)

BeginPackage["Geometry`", {"Environment`", "Player`"}]

vectorToPoint
unitVector
minDistance
checkCollision
angleFromVector

Begin["Private`"]

vectorToPoint[start_,end_]:=Module[
	{diffx,diffy,vec={0,0}},
	diffx=Mod[end[[1]]-start[[1]],environmentSize[[1]]];
	If[diffx<environmentSize[[1]]/2,
		vec[[1]]=diffx,
		vec[[1]]=diffx-environmentSize[[1]]
	];
	diffy=Mod[end[[2]]-start[[2]],environmentSize[[2]]];
	If[diffy<environmentSize[[2]]/2,
		vec[[2]]=diffy,
		vec[[2]]=diffy-environmentSize[[2]]
	];
	Return[vec]
]

angleFromVector[vec_]:=Module[
	{unit=unitVector[vec]},
	If[unit[[1]]<0,
		Return[N[ArcTan[unit[[2]]/unit[[1]]]]+ \[Pi]],
		Return[N[ArcTan[unit[[2]]/unit[[1]]]]]
	]
]

unitVector[v_] := Module[
	{},
	If[v == {0,0},
		Return[v]
	];
	Return[{v[[1]]/Sqrt[v[[1]]^2+v[[2]]^2],v[[2]]/Sqrt[v[[1]]^2+v[[2]]^2]}]
]

minDistance[p1_, p2_] := Module[
	{x, y},
	x = Abs[p1[[1]] - p2[[1]]];
	x = Min[x, environmentSize[[1]] - x];
	y = Abs[p1[[2]] - p2[[2]]];
	y = Min[y, environmentSize[[2]] - y];
	Return[Sqrt[x^2+y^2]]
]

checkCollision[alienPos_, numAliens_] := Module[
{i},
	For[i = 1, i <= numAliens, i++,
		If[EuclideanDistance[alienPos[[i]], playerPos] < 30,
			Return[True]
		]
	];
	Return[False]
]

End[]

EndPackage[]
