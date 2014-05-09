(* ::Package:: *)

BeginPackage["SpaceChase`", {"Environment`", "Player`", "AI`", "Graphics`", "KeyEvents`", "Geometry`"}]

SpaceChase
mainMenu

Begin["Private`"]

SpaceChase[] := Module[
	{},
	initJLink[];
	loadImages[];

	newGame[];
	RunScheduledTask[gameLoop[],1/frameRate];
	nb=CreateDocument[Style[drawScene[],Background-> Black,Selectable->False,
		Editable->False],CellMargins->0,ShowCellBracket->False,ShowCellLabel->False,
		"TrackCellChangeTimes"->False,WindowElements->{},WindowFrameElements->{"CloseBox"},
		WindowFrame->"Generic","BlinkingCellInsertionPoint"->False,
		"CellInsertionPointCell"->{},WindowSize->All,WindowMargins->Automatic,
		WindowTitle->"Space Chase",Background->White,Editable->False,Deployed->True];
]

gameLoop[]:=Module[
	{},
	updatePlayer[];
	updateAliens[];
	incrementTextSize[];
	incrementWarpField[];
	If[checkCollision[alienPos, numAliens],
		destroyFrame[];
		newGame[]
	]
]

nextLevel[]:= Module[
	{},
	level++;
	textIncrement = 1;
	textSize = 0;
	If[OddQ[level],
		beginWarp[],
		alienSpeed += 2.0
	];
]

mainMenu[]:=Module[
{},
	destroyFrame[];
	Switch[ChoiceDialog["Space Chase", {"New Game"->1,"Exit"->2}],
		1, newGame[],
		2, exitGame[]
	]
]

newGame[] := Module[
	{},
	initPlayer[environmentSize];
	initAliens[];
	initFrame[];
	level = 0;
	nextLevel[];
	RemoveScheduledTask[levelTask];
	levelTask = RunScheduledTask[nextLevel[], {10, \[Infinity]}];
]

exitGame[]:=Module[
	{},
	RemoveScheduledTask[ScheduledTasks[]];
	NotebookClose[nb];
]

End[]

EndPackage[]
