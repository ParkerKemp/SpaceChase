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
	initFrame[];
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
	RunScheduledTask[incrementAnimations[level], 1/animationRate];
]

gameLoop[]:=Module[
	{},
	updatePlayer[];
	updateAliens[];
	(*incrementAnimations[level];*)
	collision = checkCollision[alienPos, numAliens];
	If[!gameOver && collision != 0,
		playExplosion[];
		beginExplosionAnimation[alienVel[[collision]]];
		deleteAlien[collision];
		gameOver = True;
		textObjects[[2]][[textIncrement]] = 2;
		textObjects[[2]][[textValue]] = "GAME OVER";
		textObjects[[2]][[textColor]] = Red;
		textObjects[[2]][[textSize]] = 0;
		textObjects[[2]][[textOpacity]] = 1;
		textObjects[[2]][[textMax]] = 72;
		textObjects[[2]][[textBounce]] = False;
		textObjects[[2]][[textCenter]] = environmentSize / 2;
		textObjects[[1]][[textIncrement]] = 1;
		textObjects[[1]][[textValue]] = "Level " <> ToString[level];
		textObjects[[1]][[textColor]] = Red;
		textObjects[[1]][[textSize]] = 0;
		textObjects[[1]][[textOpacity]] = 1;
		textObjects[[1]][[textMax]] = 60;
		textObjects[[1]][[textBounce]] = False;
		textObjects[[1]][[textCenter]] = {environmentSize[[1]]/2, environmentSize[[2]]/2 - 200};
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
	textObjects[[1]][[textIncrement]] = 1;
	textObjects[[1]][[textValue]] = "Level " <> ToString[level];
	textObjects[[1]][[textOpacity]] = 1;
	textObjects[[1]][[textSize]] = 0;
	textObjects[[1]][[textMax]] = 40;
	textObjects[[1]][[textColor]] = Green;
	textObjects[[1]][[textBounce]] = True;
	textObjects[[1]][[textCenter]] = environmentSize / 2;
	If[EvenQ[level] || level == 1,
		beginWarpAnimation[];
		playWarp[],
		alienSpeed += 2.0
	];
]

continueMenu[] := Module[
	{},
	Switch[ChoiceDialog["Space Chase", {"Continue"->1, "New Game"->2,"Exit"->3}],
		1, showFrame[],
		2, newGame[],
		3, exitGame[]
	]
]

mainMenu[]:=Module[
{},
	Switch[ChoiceDialog["Space Chase", {"New Game"->1,"Exit"->2}],
		1, newGame[],
		2, exitGame[]
	]
]

newGame[] := Module[
	{},
	resetGraphics[];
	textObjects[[2]][[textOpacity]] = 0;
	initPlayer[environmentSize];
	initAliens[];
	reloadGraphics[];	
	level = 0;
	nextLevel[];
	If[ValueQ[levelTask],
		RemoveScheduledTask[levelTask];
		Clear[levelTask];
	];
	showFrame[];
	levelTask = RunScheduledTask[nextLevel[], {10, \[Infinity]}];
	gameOver = False;
]

exitGame[]:=Module[
	{},
	RemoveScheduledTask[ScheduledTasks[]];
	NotebookClose[nb];
	destroyFrame[];
]

End[]

EndPackage[]
