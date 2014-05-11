(* ::Package:: *)

BeginPackage["Graphics`", {"Environment`", "Player`", "AI`", "Particles`"}]

initGraphics
reloadGraphics
drawScene
incrementAnimations
beginWarpAnimation
beginExplosionAnimation

shuttle

Begin["Private`"]

initGraphics[] := Module[
	{},
	shuttleParts = Table[0, {5}];
	alienParts = Table[0, {5}];
	reloadGraphics[];
]

reloadGraphics[] := Module[
	{},
	loadImages[];
	loadAnimations[];
	initParticleEngine[];
]

loadAnimations[] := Module[
	{},
	aniCenter = 1;
	aniIncrement = 2;
	aniOpacity = 3;
	warpAnimation = {{0, 0}, 0, 0};
	explosionAnimation = {{0, 0}, 0, 0};
]

beginWarpAnimation[] := Module[
	{},
	nextAlienPos = randomAlienPos[];
	warpAnimation[[aniCenter]] = nextAlienPos;		
	warpAnimation[[aniIncrement]] = 10;
	warpAnimation[[aniOpacity]] = 1;
]

beginExplosionAnimation[vel_] := Module[
	{},
	createParticles[playerPos, playerVel + vel];
]

loadImages[] := Module[
	{i},
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

	For[i = 1, i < 5, i++,
		shuttleParts[[i]] = 
			Image[Show[Import["ShuttlePart" <> ToString[i] <> ".png"],
				ImageSize->shuttleSize,Background->None
				],
				ImageResolution->72
			];

		alienParts[[i]] =  
			Image[Show[Import["AlienPart" <> ToString[i] <> ".png"],
				ImageSize->shuttleSize,Background->None
				],
				ImageResolution->72
			];
	]
]

drawScene[]:=DynamicModule[
	{},
	Overlay[{background,
	Dynamic[Show[drawPlayer[], drawAllAliens[], drawWarpField[], drawParticles[],(*ListPlot[playerPath,PlotStyle->Orange],*) Graphics[{Text[Style["Level " <> ToString[level], White, FontSize->textSize], environmentSize/2]}](*,Green,Point[enemyDestination]*)]]
	}]
]

incrementAnimations[level_] := Module[
	{},
	incrementTextSize[];
	incrementWarpField[level];
	incrementParticles[];
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
	warpRadius += warpAnimation[[aniIncrement]];
	If[warpRadius > 180,
		warpRadius = 180;
		warpAnimation[[aniIncrement]] = -warpAnimation[[aniIncrement]],
		If[warpRadius < 0,
			warpRadius = 0;
			warpAnimation[[aniIncrement]] = 0;
			warpAnimation[[aniOpacity]] = 0;
			If[level == 1,
				spawnNewAlien[nextAlienPos, 2],
				spawnNewAlien[nextAlienPos, 1]
			];
		]
	]
]

drawParticles[] := Module[
	{imageList = {}},
	For[i = 1, i < numParticles / 2, i++,
		AppendTo[imageList, Inset[Rotate[shuttleParts[[i]], parRot[[i]]], parPos[[i]]]];
		AppendTo[imageList, Inset[Rotate[alienParts[[i]], parRot[[i + 5]]], parPos[[i + 5]]]];
	];

	Return[Graphics[imageList]];
]

(*drawExplosion[] := Graphics[{Opacity[*)

drawWarpField[] := Graphics[{Opacity[warpAnimation[[aniOpacity]]], Magenta, Circle[warpAnimation[[aniCenter]], warpRadius]}]

drawPlayer[]:=Module[
	{imageList={}},

	AppendTo[imageList, Inset[Rotate[shuttle, playerRot], playerPos]];
	
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



