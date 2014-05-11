(* ::Package:: *)

BeginPackage["SpaceChase`", {"Environment`", "Player`", "AI`", "Graphics`", "KeyEvents`", "Geometry`", "Parallel`", "Audio`", "Particles`"}]

SpaceChase
mainMenu
continueMenu

Begin["Private`"]

SpaceChase[] := Module[
	{},
	RemoveScheduledTask[ScheduledTasks[]];
	initJLink[];
	initGraphics[];
	startParallel[];
	importSounds[];
	newGame[];
	nb=CreateDocument[Style[drawScene[],Background-> Black,Selectable->False,
		Editable->False],CellMargins->0,ShowCellBracket->False,ShowCellLabel->False,
		"TrackCellChangeTimes"->False,WindowElements->{},WindowFrameElements->{"CloseBox"},
		WindowFrame->"Generic","BlinkingCellInsertionPoint"->False,
		"CellInsertionPointCell"->{},WindowSize->All,WindowMargins->Automatic,
		WindowTitle->"Space Chase",Background->White,Editable->False,Deployed->True];
	mainMenu[];
	RunScheduledTask[gameLoop[],1/frameRate];
]

gameLoop[]:=Module[
	{},
	updatePlayer[];
	updateAliens[];
	incrementAnimations[level];
	collision = checkCollision[alienPos, numAliens];
	If[!gameOver && collision != 0,
		playExplosion[];
		beginExplosionAnimation[alienVel[[collision]]];
		deleteAlien[collision];
		gameOver = True;
		shuttle = SetAlphaChannel[shuttle, 0];
		If[ValueQ[levelTask],
			RemoveScheduledTask[levelTask];
			Clear[levelTask];
		]
		(*destroyFrame[];
		newGame[]*)
	]
]

nextLevel[]:= Module[
	{},
	level++;
	textIncrement = 1;
	textSize = 0;
	If[EvenQ[level] || level == 1,
		beginWarpAnimation[];
		playWarp[],
		alienSpeed += 2.0
	];
]

continueMenu[] := Module[
	{},
	Switch[ChoiceDialog["Space Chase", {"Continue"->1, "New Game"->2,"Exit"->3}],
		1, initFrame[],
		2, initFrame[]; newGame[],
		3, exitGame[]
	]
]

mainMenu[]:=Module[
{},
	Switch[ChoiceDialog["Space Chase", {"New Game"->1,"Exit"->2}],
		1, initFrame[]; newGame[],
		2, exitGame[]
	]
]

newGame[] := Module[
	{},
	initPlayer[environmentSize];
	initAliens[];
	reloadGraphics[];
	level = 0;
	nextLevel[];
	If[ValueQ[levelTask],
		RemoveScheduledTask[levelTask];
		Clear[levelTask];
	];
	levelTask = RunScheduledTask[nextLevel[], {10, \[Infinity]}];
	gameOver = False;
]

exitGame[]:=Module[
	{},
	RemoveScheduledTask[ScheduledTasks[]];
	NotebookClose[nb];
]

End[]

EndPackage[]
