(* ::Package:: *)

BeginPackage["SpaceChase`", {"Environment`", "Player`", "AI`", "Graphics`", "KeyEvents`", "Geometry`", "Audio`", "Particles`"}]

SpaceChase::usage="SpaceChase[] starts a game of Space Chase."
mainMenu::usage="mainMenu[] opens the main dialogue menu."
continueMenu::usage="continueMenu[] opens the main dialogue menu with the option to continue."

Begin["Private`"]

SpaceChase[] := Module[
	{},
	(*Start a game of Space Chase from scratch.*)

	RemoveScheduledTask[ScheduledTasks[]];
	initJLink[];
	initFrame[];
	initGraphics[];
	importSounds[];
	
	(*We call newGame[] before the main menu in order to initialize
		variables before creating the game window. It will be called
		again once the player starts a game.*)
	newGame[];
	
	(*Create our main game window.*)
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
	(*Main game loop. Called per frame, this function
		updates all game objects and animations.*)

	updatePlayer[];
	updateAliens[];
	
	collision = checkCollision[alienPos, numAliens];
	
	(*If collision is non-zero, then the player has been caught.*)
	If[!gameOver && collision != 0,
		playExplosion[];
		beginExplosionAnimation[alienVel[[collision]]];
		
		(*collision is the index of the colliding alien.*)
		deleteAlien[collision];
		gameOver = True;

		(*Reset game over text animations (this block admittedly
			should have been in its own function for neatness).*)
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
		
		(*Make the shuttle image invisible.*)
		shuttle = SetAlphaChannel[shuttle, 0];
		
		(*Stop incrementing levels every 10 seconds.*)
		If[ValueQ[levelTask],
			RemoveScheduledTask[levelTask];
			Clear[levelTask];
		]
	]
]

nextLevel[]:= Module[
	{},
	(*Start the next level.*)

	level++;

	(*Reset level text object.*)
	textObjects[[1]][[textIncrement]] = 1;
	textObjects[[1]][[textValue]] = "Level " <> ToString[level];
	textObjects[[1]][[textOpacity]] = 1;
	textObjects[[1]][[textSize]] = 0;
	textObjects[[1]][[textMax]] = 40;
	textObjects[[1]][[textColor]] = Green;
	textObjects[[1]][[textBounce]] = True;
	textObjects[[1]][[textCenter]] = environmentSize / 2;
	
	(*On first level and subsequent even levels, warp in a new alien.
		On non-1 odd levels, increase base alien speed.*)
	If[EvenQ[level] || level == 1,
		beginWarpAnimation[];
		playWarp[],
		alienSpeed += 2.0
	];
]

continueMenu[] := Module[
	{},
	(*Create a dialog menu including "continue".*)

	Switch[ChoiceDialog["Space Chase", {"Continue"->1, "New Game"->2,"Exit"->3}],
		1, showFrame[],
		2, newGame[],
		3, exitGame[]
	]
]

mainMenu[]:=Module[
	{},
	(*Create a dialog menu without "continue".*)
	Switch[ChoiceDialog["Space Chase", {"New Game"->1,"Exit"->2}],
		1, newGame[],
		2, exitGame[]
	]
]

newGame[] := Module[
	{},
	(*Reset all variables and start a new game.*)

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
