#!/usr/bin/env swipl

% Produces a stream of newline delimited JSON objects,
% each of which describes an animation frame. Intended for use
% with the `compose-frames.sh` shell script.

% Constraint library for dealing with geometry and positions.
:- use_module(library(clpfd)).
:- use_module(library(http/json)).
:- use_module(library(main)).

:- initialization(main, main).

main(_Argv):-
	test_game().

% UTILS: These are probably already builtins or standard lib things,
% but I don't know the standard library

list_contains([H|_], H).
list_contains([H|T], X):- dif(H, T), list_contains(T, X).

pluck_from_list([H|T], H, T).

pluck_from_list([H|T], Needle, [H|Rest]):-
	pluck_from_list(T, Needle, Rest).

% Y speed is positive toward the bottom of the screen and negative
% toward the top of the screen.
% X speed of a mob is always positive, and is modified by the facing
% of the mob.

% Units
% 16:9 screen aspect ratio
% 1280 x 720 abstract units of width and height
% (for comparison, NES is 240 x 240 pixels, each pixel is 8:7)

% We render 25 fps, run cycle is 5 frames per sprite.

% We want XSpeed to be 4 game units per frame (megaman is 4.125 units per frame)
% YSpeed of a jump is 14 game units per frame starting, with a 1 game unit per frame per frame gravity
%    (megaman jump is 14.625 units / frame)

anim_frame(Tick, 1):-
	Slice #= Tick mod 20,
	Slice in 0..4.

anim_frame(Tick, 2):-
	Slice #= Tick mod 20,
	Slice in 5..9.

anim_frame(Tick, 3):-
	Slice #= Tick mod 20,
	Slice in 10..14.

anim_frame(Tick, 4):-
	Slice #= Tick mod 20,
	Slice in 15..19.


% Sprite sheet geometry
% sheet_geometry(mobId, spriteId, widthUnits, heightUnits, sheetName, sheetOffsetX, sheetOffsetY, sheetWidth, sheetHeight)

sprite_height(TypeId, Sprite, Height):-
	sheet_geometry(TypeId, Sprite, _Width, Height, _SheetName, _SheetOffsetX, _SheetOffsetY, _SheetWidth, _SheetHeight).

sprite_width(TypeId, Sprite, Width):-
	sheet_geometry(TypeId, Sprite, Width, _Height, _SheetName, _SheetOffsetX, _SheetOffsetY, _SheetWidth, _SheetHeight).
	

sheet_geometry(hero, standLeft, 60, 72, "megaman/stand-left.png", 0, 0, 21, 24).

sheet_geometry(hero, standRight, 60, 72, "megaman/stand-right.png", 0, 0, 21, 24).

% TODO: run cycle filenames are out of order and confusing.

sheet_geometry(hero, runLeft1, 48, 72, "megaman/run-left-2.png", 0, 9, 16, 24).

sheet_geometry(hero, runLeft2, 72, 66, "megaman/run-left-1.png", 0, 0, 24, 22).

sheet_geometry(hero, runLeft3, 48, 72, "megaman/run-left-2.png", 0, 9, 16, 24).

sheet_geometry(hero, runLeft4, 63, 66, "megaman/run-left-3.png", 0, 0, 21, 24).

sheet_geometry(hero, runRight1, 48, 72, "megaman/run-right-2.png", 0, 0, 16, 24).

sheet_geometry(hero, runRight2, 72, 66, "megaman/run-right-1.png", 0, 0, 24, 22).

sheet_geometry(hero, runRight3, 48, 72, "megaman/run-right-2.png", 0, 0, 16, 24).

sheet_geometry(hero, runRight4, 63, 66, "megaman/run-right-3.png", 0, 0, 21, 24).

sheet_geometry(hero, jumpLeft, 78, 90, "megaman/jump-left.png", 0, 0, 26, 30).

sheet_geometry(hero, jumpRight, 78, 90, "megaman/jump-right.png", 0, 0, 26, 30).

sheet_geometry(brick, brick, 32, 32, "placeholders/brick-16x16.png", 0, 0, 16, 16).


% facing is left, right, or none
% mob(type_identifier, xposition, yposition, xspeed, yspeed, facing)
% xposition and yposition are abstract, but probably the bottom left corner of the sprite

sprite_sheet(hero, 0, 0, left, _Tick, standLeft).

sprite_sheet(hero, 0, 0, right, _Tick, standRight).

sprite_sheet(hero, XSpeed, 0, right, Tick, runRight1):-
	anim_frame(Tick, 1),
	XSpeed #\= 0.

sprite_sheet(hero, XSpeed, 0, right, Tick, runRight2):-
	anim_frame(Tick, 2),
	XSpeed #\= 0.

sprite_sheet(hero, XSpeed, 0, right, Tick, runRight3):-
	anim_frame(Tick, 3),
	XSpeed #\= 0.

sprite_sheet(hero, XSpeed, 0, right, Tick, runRight4):-
	anim_frame(Tick, 4),
	XSpeed #\= 0.

sprite_sheet(hero, XSpeed, 0, left, Tick, runLeft1):-
	anim_frame(Tick, 1),
	XSpeed #\= 0.

sprite_sheet(hero, XSpeed, 0, left, Tick, runLeft2):-
	anim_frame(Tick, 2),
	XSpeed #\= 0.

sprite_sheet(hero, XSpeed, 0, left, Tick, runleft3):-
	anim_frame(Tick, 3),
	XSpeed #\= 0.

sprite_sheet(hero, XSpeed, 0, left, Tick, runleft4):-
	anim_frame(Tick, 4),
	XSpeed #\= 0.

% BUG: "jump if Yspeed #\= 0" means we have a weird stand at the top of the jump?

sprite_sheet(hero, _XSpeed, YSpeed, right, _Tick, jumpRight):-
	YSpeed #\= 0.

sprite_sheet(hero, _XSpeed, YSpeed, left, _Tick, jumpLeft):-
	YSpeed #\= 0.

sprite_sheet(brick, _XSpeed, _YSpeed, _Facing, _Tick, brick).


sprite(mob(TypeId, _XPosition, _YPosition, XSpeed, YSpeed, Facing), Tick, SheetId):-
	sprite_sheet(TypeId, XSpeed, YSpeed, Facing, Tick, SheetId).


box_top(box(_Left, Top, _Width, _Height), Top).
box_bottom(box(_Left, Top, _Width, Height), Bottom):-
	Bottom #= Top + Height.
box_left(box(Left, _Top, _Width, _Height), Left).
box_right(box(Left, _Top, Width, _Height), Right):-
	Right #= Left + Width.

overlaps(box(Left1, Top1, Width1, Height1), box(Left2, Top2, Width2, Height2)):-
	% We allow (exactly) shared bounds without considering them overlap
	Left1End #= Left1 + Width1,
	Left2End #= Left2 + Width2,
	Top1End #= Top1 + Height1,
	Top2End #= Top2 + Height2,
	( between(Left2, Left2End, Left1); between(Left1, Left1End, Left2) ),
	( between(Top2, Top2End, Top1); between(Top1, Top1End, Top2)). 

% TODO: Fix these bugs!
% TWO BUGS
% - We assume we collide with only one counterpart.
% - We assume our motion is short enough that we'll never teleport across a counterpart.
collisions(TargetBox, OtherBoxes, Collisions):-
	include(overlaps(TargetBox), OtherBoxes, Collisions).

min_pair(K1-V1, K2-V2, K-V):-
	(K1 #< K2, K=K1, V=V1);
	(K=K2, V=V2).


minimum_absolute_delta(Target, Other1, Other2, Min):-
	AD1 #= abs(Target - Other1),
	AD2 #= abs(Target - Other2),
	min_pair(AD1-Other1, AD2-Other2, _Abs-Min).

move_right_until_collision(TargetBox, Collidables, LeftBarrier):-
	collisions(TargetBox, Collidables, Collisions),
	maplist(box_left(), Collisions, [Lft | LftList]),
	box_right(TargetBox, Frontier),
	foldl(minimum_absolute_delta(Frontier), LftList, Lft, LeftBarrier).

move_left_until_collision(TargetBox, Collidables, RightBarrier):-
	collisions(TargetBox, Collidables, Collisions),
	maplist(box_right(), Collisions, [Rt | RtList ]),
	box_left(TargetBox, Frontier),
	foldl(minimum_absolute_delta(Frontier), RtList, Rt, RightBarrier).

move_up_until_collision(TargetBox, Collidables, BarrierAbove):-
	collisions(TargetBox, Collidables, Collisions),
	maplist(box_bottom(), Collisions, [Btm|BtmList]),
	box_top(TargetBox, Frontier),
	foldl(minimum_absolute_delta(Frontier), BtmList, Btm, BarrierAbove).

move_down_until_collision(TargetBox, Collidables, BarrierBelow):-
	collisions(TargetBox, Collidables, Collisions),
	maplist(box_top(), Collisions, [Top|TopList]),
	box_bottom(TargetBox, Frontier),
	foldl(minimum_absolute_delta(Frontier), TopList, Top, BarrierBelow).

sprite_box(Tick, Mob, box(XPosition, BoxY, Width, Height)):-
	sprite(Mob, Tick, Sprite),
	Mob = mob(TypeId, XPosition, YPosition, _XSpeed, _YSpeed, _Facing),
	sheet_geometry(TypeId, Sprite, Width, Height, _Sheet, _SheetX, _SheetY, _SheetWidth, _SheetHeight),
	% YPosition is bottom left, box position is top left.
	BoxY #= YPosition - Height.

mob_move_x_toward_facing(Mob, Moved):-
	Mob = mob(TypeId, XPosition, YPosition, XSpeed, YSpeed, right),
	integer(XSpeed),
	TargetX #= XPosition + XSpeed,
	Moved = mob(TypeId, TargetX, YPosition, XSpeed, YSpeed, right).

mob_move_x_toward_facing(Mob, Moved):-
	Mob = mob(TypeId, XPosition, YPosition, XSpeed, YSpeed, left),
	integer(XSpeed),
	TargetX #= XPosition - XSpeed,
	Moved = mob(TypeId, TargetX, YPosition, XSpeed, YSpeed, left).


mob_move_x(Mob, _Tick, _Collidables, Mob):-
	Mob = mob(_TypeId, _XPosition, _YPosition, none, _YSpeed, _Facing).

mob_move_x(Mob, Tick, Collidables, Moved):-
	mob_move_x_toward_facing(Mob, MoveTarget),
	sprite_box(Tick, MoveTarget, TargetBox),
	MoveTarget = mob(TypeId, _TargetX, YPosition, _XSpeed, YSpeed, Facing),
	(
		(
			Facing = right,
			move_right_until_collision(TargetBox, Collidables, LeftBarrier),
			sprite_sheet(TypeId, 0, YSpeed, Facing, Tick, Sprite),
			sprite_width(TypeId, Sprite, Width),
			FinalX #= LeftBarrier - Width,
			Moved = mob(TypeId, FinalX, YPosition, 0, YSpeed, Facing)
		);
		(
			Facing = left, 
			move_left_until_collision(TargetBox, Collidables, RightBarrier),
			Moved = mob(TypeId, RightBarrier, YPosition, 0, YSpeed, Facing)
		);
		(Moved = MoveTarget)
	).

mob_move_y(Mob, _Tick, _Collidables, Mob):-
	Mob = mob(_TypeId, _XPosition, _YPosition, _XSpeed, none, _Facing).	

mob_move_y(Mob, Tick, Collidables, Moved):-
	Mob = mob(TypeId, XPosition, YPosition, XSpeed, YSpeed, Facing),
	TargetY #= YPosition + YSpeed,
	integer(YSpeed),

	% Glitch Here! We calculate collisions with the destination bounding box,
	% But if a collision happens, we may render a different sprite!
	TargetMove = mob(TypeId, XPosition, TargetY, XSpeed, YSpeed, Facing),
	sprite_box(Tick, TargetMove, TargetBox),

	(
		(
			move_down_until_collision(TargetBox, Collidables, BarrierBelow),
			YSpeed #> 0,
			FinalY #= BarrierBelow - 1,
			Moved = mob(TypeId, XPosition, FinalY, XSpeed, 0, Facing)
		);
		(
			move_up_until_collision(TargetBox, Collidables, BarrierAbove),
			YSpeed #< 0,
			sprite_sheet(TypeId, XSpeed, 0, Tick, Facing, Sprite),
			sprite_height(TypeId, Sprite, Height),
			FinalY #= BarrierAbove + Height + 1,
			Moved = mob(TypeId, XPosition, FinalY, XSpeed, 0, Facing)
		);
		(Moved = TargetMove)
	).

mob_move(Mob, Tick, Collidables, Moved):-
	mob_move_x(Mob, Tick, Collidables, XMoved),
	mob_move_y(XMoved, Tick, Collidables, Moved).

% TODO: Physics and collision detection should be intertwingled;
% in particular, what collides with what after a move
% (and how to reposition a thing after a move)
% matters.

gravity([], []).
gravity([H | L], [H | L1]):-
	H = mob(_TypeId, _XPosition, _YPosition, _XSpeed, none, _Facing),
	gravity(L, L1).
gravity([H | L], [H1 | L1]):-
	H = mob(TypeId, XPosition, YPosition, XSpeed, YSpeed, Facing),
	integer(YSpeed),
	NewSpeed #= YSpeed + 1,
	H1 = mob(TypeId, XPosition, YPosition, XSpeed, NewSpeed, Facing),
	gravity(L, L1).

after_physics(OldState, Tick, NewState):-
	gravity(OldState, Accellerated),
	Mover = mob(hero, _XPosition, _YPosition, _XSpeed, _YSpeed, _Facing),
	pluck_from_list(Accellerated, Mover, Others),
	maplist(sprite_box(Tick), Others, Collidables),
	mob_move(Mover, Tick, Collidables, Moved),
	NewState = [Moved|Others].

is_endgame([], _Tick, _Bounds).

is_endgame(State, Tick, level_bounds(LevelWidth, LevelHeight)):-
	list_contains(State,
		mob(hero, XPosition, YPosition, XSpeed, YSpeed, Facing)),
	sprite(mob(hero, XPosition, YPosition, XSpeed, YSpeed, Facing),
		Tick,
		Sprite),
	sheet_geometry(hero, Sprite, HeroWidth, HeroHeight, _SheetName, _SheetX, _SheetY, _SheetW, _SheetH),
	( XPosition + HeroWidth #< 0;  XPosition #> LevelWidth; YPosition #< 0; YPosition #> LevelHeight + HeroHeight ).

% TODO: We should know the sprite sheet geometry here
% TODO: We should know about SCREEN geometry here, too!

writable_mob(Tick, mob(TypeId, XPosition, BottomYPosition, XSpeed, YSpeed, Facing), writable{type_id:TypeId, sprite:Sprite, level_x:XPosition, level_y:TopYPosition, level_width:LevelWidth, level_height:LevelHeight, sheet:SheetName, sheet_x:SheetX, sheet_y:SheetY, sheet_width:SheetWidth, sheet_height:SheetHeight}):-
	sprite(mob(TypeId, XPosition, BottomYPosition, XSpeed, YSpeed, Facing),
		Tick,
		Sprite),
	sheet_geometry(TypeId, Sprite, LevelWidth, LevelHeight, SheetName, SheetX, SheetY, SheetWidth, SheetHeight),
	% game geometry positions things at their bottom left corners, but
	% the renderer needs top left corners
	TopYPosition #= BottomYPosition - LevelHeight.

write_state(Tick, State):-
	maplist(writable_mob(Tick),
		State,
		Writables),
	json_write_dict(current_output,
		state{
			tick : Tick,
			sprites : Writables
			},
		[width(0)]),
	nl.


% Right now, game state is just a list of Mobs

% A game produces "done" after writing a list of frames

game(OldState, Tick, Bounds):-
	is_endgame(OldState, Tick, Bounds);
% Timeout
Tick > 100;
!,
	after_physics(OldState, Tick, NewState),
	NextTick #= Tick + 1,
	write(current_output, "# "), write(current_output, gamestate(NewState, NextTick)), nl,
	write_state(Tick, NewState),

	( OldState = NewState; game(NewState, NextTick, Bounds)).

test_game():-
	% Need a better way to describe the initial state of a level
	game(
		[
			mob(hero, 0, 100, 4, 0, right),
			mob(brick, 96, 344, none, none, neutral),
			mob(brick, 128, 344, none, none, neutral),
			mob(brick, 160, 344, none, none, neutral),
			mob(brick, 192, 444, none, none, neutral),
			mob(brick, 224, 444, none, none, neutral),
			mob(brick, 256, 444, none, none, neutral),
			mob(brick, 288, 444, none, none, neutral)
		],
		0,
		level_bounds(1280, 720)).


:- begin_tests(game).

minimum_absolute_delta(331, 312, 312, 312).

sprite_box(20, mob(brick, 96, 344, none, none, neutral), box(96, 312, 32, 32)).

mob_move_y(mob(hero, 84, 310, 4, 21, right), 20, [box(96, 312, 32, 32), box(128, 312, 32, 32)], mob(hero, 84, 311, 4, 0, right)).

after_physics(
	[mob(hero,80,310,4,20,right),mob(brick,96,344,none,none,neutral)],20,
	[mob(hero, 84, 311, 4, 0, right), mob(brick, 96, 344, none, none, neutral)]
	).

:- end_tests(game).