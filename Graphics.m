(* ::Package:: *)

BeginPackage["Graphics`", {"Environment`", "Player`", "AI`"}]

loadImages
drawScene

Begin["Private`"]

loadImages[] := Module[
	{},
	shuttle=Image[
		Show[
			Import["Shuttle.png"],
			ImageSize->shuttleSize,Background->None
		],
		ImageResolution->72
	];

	background = Image[
		Show[
			Import["background.png"]
		]
	];
]

drawScene[]:=DynamicModule[
	{},
	Overlay[{background,
	Dynamic[Show[drawPlayer[],(*ListPlot[playerPath,PlotStyle\[Rule]Orange],*)Graphics[{PointSize[Large],Red,Point[alienPos](*,Green,Point[enemyDestination]*)}]]]
	}]
]

drawPlayer[]:=Module[
	{imageList={Inset[Rotate[shuttle,playerRot],playerPos]}},
	(* Only update when the key "UP" is pressed *)
	If[playerPos[[1]] <= shuttleSize,
		AppendTo[imageList,Inset[Rotate[shuttle,playerRot],playerPos+{environmentSize[[1]],0}]]
	];
	If[playerPos[[1]]>=environmentSize[[1]]-shuttleSize,
		AppendTo[imageList,Inset[Rotate[shuttle,playerRot],playerPos-{environmentSize[[1]],0}]]
	];
	If[playerPos[[2]]<=shuttleSize,
		AppendTo[imageList,Inset[Rotate[shuttle,playerRot],playerPos+{0,environmentSize[[2]]}]]
	];
	If[playerPos[[2]]>=environmentSize[[2]]-shuttleSize,
		AppendTo[imageList,Inset[Rotate[shuttle,playerRot],playerPos-{0,environmentSize[[2]]}]]
	];
	Return[Graphics[imageList,PlotRange->{{0,environmentSize[[1]]},{0,environmentSize[[2]]}},ImageSize->windowSize]]
]

End[]

EndPackage[]
