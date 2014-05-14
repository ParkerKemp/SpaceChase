(* ::Package:: *)

BeginPackage["Graphics`", {"Environment`", "Player`", "AI`", "Particles`"}]

initGraphics::usage="initGraphics[] initializes all attributes and loads images associated with graphics."
resetGraphics::usage="resetGraphics[] resets all attributes associated with graphics."
reloadGraphics::usage="reloadGraphics[] reloads all images associated with graphics."
drawScene::usage="drawScene[] dynamically draws the entire scene."
incrementAnimations::usage="incrementAnimations[level] increments all interpolated animations."
beginWarpAnimation::usage="beginWarpAnimation[] initializes attributes for the warp animation."
beginExplosionAnimation::usage="beginExplosionAnimation[] initializes attributes for the explosion animation."

shuttle

Begin["Private`"]

initGraphics[] := Module[
	{},
	(*Initialize graphics variables.*)

	shuttleParts = Table[0, {5}];
	alienParts = Table[0, {5}];
	dummyAnimation = {{0, 0}, 0, 0, 0, 0};
	explosionAnimation = Table[dummyAnimation, {10}];
	resetGraphics[];
	reloadGraphics[];
]

resetGraphics[] := Module[
	{},
	(*Reset default values for animations and particles.*)

	loadAnimations[];
	initParticleEngine[];
]

reloadGraphics[] := Module[
	{},
	(*Reload images.*)

	loadImages[];
]

loadAnimations[] := Module[
	{},
	(*Set animations to default values.*)

	aniCenter = 1;
	aniIncrement = 2;
	aniOpacity = 3;
	aniMax = 4;
	aniSize = 5;
	warpAnimation = {{0, 0}, 0, 0, 0, 0};
]

beginWarpAnimation[] := Module[
	{},
	(*Get a random alien position and initialize a warp
		animation there.*)

	nextAlienPos = randomAlienPos[];
	warpAnimation[[aniCenter]] = nextAlienPos;		
	warpAnimation[[aniIncrement]] = 10;
	warpAnimation[[aniOpacity]] = 1;
	warpAnimation[[aniSize]] = 0;
	warpAnimation[[aniMax]] = 180;
]

beginExplosionAnimation[vel_] := Module[
	{i},
	(*Initialize an explosion particle animation with
		player's position and a base velocity vector.*)

	createParticles[playerPos, (playerVel + vel)/2];
	For[i = 1, i <= 10, i++,
		explosionAnimation[[i]] = {{0, 0}, RandomInteger[{4, 8}], 1, RandomInteger[{24, 36}], 0};
	]
]

loadImages[] := Module[
	{i},
	(*Load image files.*)

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

	(*"Parts" textures are all named methodically, so we
		can load them iteratively instead of hard-coding names.*)
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
	(*Draw the entire scene dynamically.*)

	Overlay[{background,
	Dynamic[Show[drawPlayer[], drawPlayerPath[], drawAllAliens[], drawWarpField[], drawExplosion[], drawParticles[], drawText[]]]
	}]
]

drawPlayerPath[] := If[showPlayerPath,
					ListPlot[playerPath, PlotStyle->{Blue, PointSize[Medium]}],
					Graphics[]
					]
	(*Draw the player's future path (as computed by AI).*)


drawText[] := Module[
	{textList = {}, i},
	(*Return a Graphics with all text objects.*)

	For[i = 1, i <= Length[textObjects], i++,
		AppendTo[textList, Opacity[textObjects[[i]][[textOpacity]]]];
		AppendTo[textList, Text[Style[textObjects[[i]][[textValue]], textObjects[[i]][[textColor]], FontSize->textObjects[[i]][[textSize]]], textObjects[[i]][[textCenter]]]]
	];
	Return[Graphics[textList]]
]

incrementAnimations[level_] := Module[
	{i},
	(*Increment all animation attributes.*)

	incrementTextSize[];
	warpAnimation = incrementBurst[warpAnimation, level];
	incrementParticles[];
	For[i = 1, i <= 10, i++,
		explosionAnimation[[i]] = incrementBurst[explosionAnimation[[i]], 0];
	]
]

incrementTextSize[]:=Module[
	{i},
	(*Increment text animations. textIncrement is used to control
		how (if at all) each text object will be resized per frame.
		textMax determines when text should start shrinking again.
		text will only shrink if textBounce is True.*)

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
	(*Increment animation attributes. Burst animations work
		almost identically with the text animations (above).*)

	ret[[aniSize]] += ret[[aniIncrement]];
	If[ret[[aniSize]] > ret[[aniMax]],
		ret[[aniSize]] = ret[[aniMax]];
		ret[[aniIncrement]] = -ret[[aniIncrement]],
		If[ret[[aniSize]] < 0,
			ret[[aniSize]] = 0;
			ret[[aniIncrement]] = 0;
			ret[[aniOpacity]] = 0;
			If[level > 0,
				(*Special case: non-zero level implies that this is
					warpAnimation. When animation is done, spawn
					a new alien (dumb if level 1, smart otherwise.*)
				If[level == 1,
					spawnNewAlien[nextAlienPos, 2],
					spawnNewAlien[nextAlienPos, 1]
				]
			]
		]
	];
	Return[ret]
]

drawParticles[] := Module[
	{imageList = {}},
	(*Generate a Graphics list of particle images.*)

	For[i = 1, i < numParticles / 2, i++,
		AppendTo[imageList, Inset[Rotate[shuttleParts[[i]], parRot[[i]]], parPos[[i]]]];
		AppendTo[imageList, Inset[Rotate[alienParts[[i]], parRot[[i + 5]]], parPos[[i + 5]]]];
	];

	Return[Graphics[imageList]];
]

drawExplosion[] := Module[
	{i, list = {Opacity[explosionAnimation[[1]][[aniOpacity]]]}},
	(*Generate a Graphics list of disks based on explosionAnimation.*)

	For[i = 1, i <= numParticles, i++,
		AppendTo[list, RandomChoice[{Red, Orange}]];
		AppendTo[list, Disk[parPos[[i]], explosionAnimation[[i]][[aniSize]]]]
	];
	Return[Graphics[list]]
]

drawWarpField[] := Graphics[{Opacity[warpAnimation[[aniOpacity]]], Magenta, Circle[warpAnimation[[aniCenter]], warpAnimation[[aniSize]]]}]
	(*Return a magenta circle using warpAnimation.*)

drawPlayer[]:=Module[
	{imageList={}},
	(*Draw player image.*)

	AppendTo[imageList, Inset[Rotate[shuttle, playerRot], playerPos]];
	
	(*If player is close to one of the edges, add another image to
		give the illusion of "poking out" on the other side.*)
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
	(*Return a Graphics list of all aliens.*)

	For[i = 1, i <= numAliens, i++,
		AppendTo[graphicsList, drawAlien[i]];
	];
	Return[Graphics[graphicsList]];
]

drawAlien[index_]:=Module[
	{},
	(*Return a properly-oriented Graphics object of 
		the alien at index.*)

	Return[Inset[Rotate[alienShip, alienRot[[index]]], alienPos[[index]]]]
]

End[]

EndPackage[]



