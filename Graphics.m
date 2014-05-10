(* ::Package:: *)

BeginPackage["Graphics`", {"Environment`", "Player`", "AI`"}]

loadImages
drawScene
incrementTextSize
incrementWarpField

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

	alienShip=Image[
		Show[
			Import["AlienShip.png"],
			ImageSize->shuttleSize,Background->None
		],
		ImageResolution->72
	];
]

drawScene[]:=DynamicModule[
	{},
	Overlay[{background,
	Dynamic[Show[drawPlayer[], drawAllAliens[], drawWarpField[],(*ListPlot[playerPath,PlotStyle->Orange],*) Graphics[{Text[Style["Level " <> ToString[level], White, FontSize->textSize], environmentSize/2]}](*,Green,Point[enemyDestination]*)]]
	}]
]

incrementTextSize[]:=Module[
	{},
	textSize += textIncrement;
	If[textSize > 40,
		textSize = 40;
		textIncrement = -textIncrement,
		If[textSize < 0,
			textSize = 0;
			textIncrement = 0
		]
	]
]

incrementWarpField[level_] := Module[
	{},
	warpRadius += warpIncrement;
	If[warpRadius > 150,
		warpRadius = 150;
		warpIncrement = -warpIncrement,
	If[warpRadius < 0,
		warpRadius = 0;
		warpIncrement = 0;
		warpOpacity = 0;
		If[level == 1,
			spawnNewAlien[2],
			spawnNewAlien[1]
		];
	]]
]

drawWarpField[] := Graphics[{Opacity[warpOpacity], Magenta, Circle[warpCenter, warpRadius]}]

drawPlayer[]:=Module[
	{imageList={Inset[Rotate[shuttle, playerRot], playerPos]}},
	(* Only update when the key "UP" is pressed *)
	If[playerPos[[1]] <= shuttleSize,
		AppendTo[imageList,Inset[Rotate[shuttle, playerRot], playerPos + {environmentSize[[1]],0}]]
	];
	If[playerPos[[1]]>=environmentSize[[1]]-shuttleSize,
		AppendTo[imageList,Inset[Rotate[shuttle, playerRot], playerPos - {environmentSize[[1]],0}]]
	];
	If[playerPos[[2]]<=shuttleSize,
		AppendTo[imageList,Inset[Rotate[shuttle, playerRot], playerPos + {0,environmentSize[[2]]}]]
	];
	If[playerPos[[2]]>=environmentSize[[2]] - shuttleSize,
		AppendTo[imageList,Inset[Rotate[shuttle, playerRot], playerPos - {0,environmentSize[[2]]}]]
	];
	Return[Graphics[imageList,PlotRange->{{0,environmentSize[[1]]},{0,environmentSize[[2]]}}, ImageSize->windowSize]]
]

drawAllAliens[]:=Module[
	{i,graphicsList={}},
	For[i = 1, i <= numAliens, i++,
		AppendTo[graphicsList, drawAlien[i]];
	];
	Return[Graphics[graphicsList]];
]

drawAlien[index_]:=Module[
	{},
	Return[Inset[Rotate[alienShip, alienRot[[index]]], alienPos[[index]]]]
]

End[]

EndPackage[]
