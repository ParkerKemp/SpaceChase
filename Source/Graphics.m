(* ::Package:: *)

BeginPackage["Graphics`", {"Environment`", "Player`", "AI`", "Particles`"}]

initGraphics
resetGraphics
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
	dummyAnimation = {{0, 0}, 0, 0, 0, 0};
	explosionAnimation = Table[dummyAnimation, {10}];
	resetGraphics[];
	reloadGraphics[];
]

resetGraphics[] := Module[
	{},
	loadAnimations[];
	initParticleEngine[];
]

reloadGraphics[] := Module[
	{},
	loadImages[];
]

loadAnimations[] := Module[
	{},
	aniCenter = 1;
	aniIncrement = 2;
	aniOpacity = 3;
	aniMax = 4;
	aniSize = 5;
	warpAnimation = {{0, 0}, 0, 0, 0, 0};
]

beginWarpAnimation[] := Module[
	{},
	nextAlienPos = randomAlienPos[];
	warpAnimation[[aniCenter]] = nextAlienPos;		
	warpAnimation[[aniIncrement]] = 10;
	warpAnimation[[aniOpacity]] = 1;
	warpAnimation[[aniSize]] = 0;
	warpAnimation[[aniMax]] = 180;
	debugTime = AbsoluteTime[];
]

beginExplosionAnimation[vel_] := Module[
	{i},
	createParticles[playerPos, (playerVel + vel)/2];
	For[i = 1, i <= 10, i++,
		(*With[{inc = RandomInteger[{3, 4}], max = RandomInteger*)
		explosionAnimation[[i]] = {{0, 0}, RandomInteger[{4, 8}], 1, RandomInteger[{24, 36}], 0};
	]
]

loadImages[] := Module[
	{i},
	shuttle=Image[
		Show[
			Import["Textures/Shuttle.png"],
			ImageSize->shuttleSize,Background->None
		],
		ImageResolution->72
	];

	background = Image[
		Show[
			Import["Textures/background.png"]
		]
	];

	alienShip=Image[
		Show[
			Import["Textures/AlienShip.png"],
			ImageSize->shuttleSize,Background->None
		],
		ImageResolution->72
	];

	For[i = 1, i < 5, i++,
		shuttleParts[[i]] = 
			Image[Show[Import["Textures/ShuttlePart" <> ToString[i] <> ".png"],
				ImageSize->shuttleSize,Background->None
				],
				ImageResolution->72
			];

		alienParts[[i]] =  
			Image[Show[Import["Textures/AlienPart" <> ToString[i] <> ".png"],
				ImageSize->shuttleSize,Background->None
				],
				ImageResolution->72
			];
	]
]

drawScene[]:=DynamicModule[
	{},
	Overlay[{background,
	Dynamic[Show[drawPlayer[],(* drawPlayerPath[],*) drawAllAliens[], drawWarpField[], drawExplosion[], drawParticles[], drawText[](*ListPlot[playerPath,PlotStyle->Orange],*) (*,Green,Point[enemyDestination]*)]]
	}]
]

drawPlayerPath[] := ListPlot[playerPath, PlotStyle->{Blue, PointSize[Medium]}](*, Graphics[ListPlot[playerPath,PlotStyle->Green]]}*)





drawText[] := Module[
	{textList = {}, i},
	For[i = 1, i <= Length[textObjects], i++,
		AppendTo[textList, Opacity[textObjects[[i]][[textOpacity]]]];
		AppendTo[textList, Text[Style[textObjects[[i]][[textValue]], textObjects[[i]][[textColor]], FontSize->textObjects[[i]][[textSize]]], textObjects[[i]][[textCenter]]]]
	];
	Return[Graphics[textList]]
]

incrementAnimations[level_] := Module[
	{i},
	incrementTextSize[];
	warpAnimation = incrementBurst[warpAnimation, level];
	incrementParticles[];
	For[i = 1, i <= 10, i++,
		explosionAnimation[[i]] = incrementBurst[explosionAnimation[[i]], 0];
	]
]

incrementTextSize[]:=Module[
	{i},
	For[i = 1, i <= Length[textObjects], i++,
		textObjects[[i]][[textSize]] += textObjects[[i]][[textIncrement]];
		If[textObjects[[i]][[textSize]] > textObjects[[i]][[textMax]],
			textObjects[[i]][[textSize]] = textObjects[[i]][[textMax]];
			If[textObjects[[i]][[textBounce]],
				textObjects[[i]][[textIncrement]] = -textObjects[[i]][[textIncrement]],
				textObjects[[i]][[textIncrement]] = 0
			],
			If[textObjects[[i]][[textSize]] < 0,
				textObjects[[i]][[textSize]] = 0;
				textObjects[[i]][[textOpacity]] = 0;
				textObjects[[i]][[textIncrement]] = 0
			]
		]
	]
]

incrementBurst[burst_, level_] := Module[
	{ret = burst},
	ret[[aniSize]] += ret[[aniIncrement]];
	If[ret[[aniSize]] > ret[[aniMax]],
		ret[[aniSize]] = ret[[aniMax]];
		ret[[aniIncrement]] = -ret[[aniIncrement]],
		If[ret[[aniSize]] < 0,
			ret[[aniSize]] = 0;
			ret[[aniIncrement]] = 0;
			ret[[aniOpacity]] = 0;
			If[level > 0,
				If[level == 1, 
					spawnNewAlien[nextAlienPos, 2],
					spawnNewAlien[nextAlienPos, 1]
				]
			]
		]
	];
	Return[ret]
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

drawExplosion[] := Module[
	{i, list = {Opacity[explosionAnimation[[1]][[aniOpacity]]]}},
	For[i = 1, i <= numParticles, i++,
		AppendTo[list, RandomChoice[{Red, Orange}]];
		AppendTo[list, Disk[parPos[[i]], explosionAnimation[[i]][[aniSize]]]]
	];
	Return[Graphics[list]]
]

drawWarpField[] := Graphics[{Opacity[warpAnimation[[aniOpacity]]], Magenta, Circle[warpAnimation[[aniCenter]], warpAnimation[[aniSize]]]}]

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



