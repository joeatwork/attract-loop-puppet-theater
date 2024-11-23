#!/usr/bin/env swipl

% Produces a stream of newline delimited JSON objects,
% each of which describes an animation frame. Intended for use
% with the `compose-frames.sh` shell script.

% Constraint library for dealing with geometry and positions.
:- use_module(library(clpfd)).
:- use_module(library(http/json)).
:- use_module(library(main)).

:- use_module(prolog/assets).
:- use_module(prolog/goal_agent).
:- use_module(prolog/mobs).

:- initialization(main, main).

main(_Argv):-
	test_game().

% Y speed is positive toward the bottom of the screen and negative
% toward the top of the screen.
% X speed of a mob is always positive, and is modified by the facing
% of the mob.

% Units
% 16:9 screen aspect ratio
% 1280 x 720 abstract units of width and height
% (for comparison, NES is 240 x 240 pixels, each pixel is 8:7)


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

after_physics(Mobs, Tick, [Moved|Others]):-
	gravity(Mobs, Accellerated),
	partition(mob_type(hero), Accellerated, [Mover], Others),
	maplist(sprite_box(Tick), Others, Collidables),
	mob_move(Mover, Tick, Collidables, Moved).

after_agents(Mobs, Tick, NewMobs):-
	control_hero(
		box(360, 2000, 32, 32), % Target
		Mobs, Tick,
		NewMobs).

is_endgame([], _Tick, _Bounds).

is_endgame(Mobs, Tick, level_dimensions(LevelWidth, LevelHeight)):-
	include(mob_type(hero), Mobs, [Hero]),
	sprite_box(Tick, Hero, box(Left, Top, Width, Height)),
	( Left + Width #< 0;  Left #> LevelWidth; Top + Height #< 0; Top #> LevelHeight ).

% We center the viewport on the hero's left heel, which is
% not actually what we want (but we probably want some *constant offset*
% from the left heel)
viewport_follows_hero(Mobs, level_dimensions(LevelWidth, LevelHeight), viewport(VLeft, VTop, VWidth, VHeight)):-
	include(mob_type(hero), Mobs, [mob(hero, HeroLeft, HeroBottom, _XSpeed, _YSpeed, _Facing)]),
	HalfVWidth #= div(VWidth, 2),
	HalfVHeight #= div(VHeight, 2),
	CenteredLeft #= HeroLeft - HalfVWidth,
	CenteredTop #= HeroBottom - HalfVHeight,
	(
		(CenteredLeft #< 0, VLeft = 0);
		(CenteredLeft + VWidth #> LevelWidth, VLeft = LevelWidth - VWidth);
		VLeft = CenteredLeft
	),
	(
		(CenteredTop #< 0, VTop = 0);
		(CenteredTop + VHeight #> LevelHeight, VTop = LevelHeight - VHeight);
		VTop = CenteredTop
	).


writable_mob(Tick, Viewport, Mob, Write):-
	Mob = mob(TypeId, XPosition, BottomYPosition, XSpeed, YSpeed, Facing),
	sprite_sheet(TypeId, XSpeed, YSpeed, Facing, Tick, Sprite),
	sheet_geometry(TypeId, Sprite, LevelWidth, LevelHeight, SheetName, SheetX, SheetY, SheetWidth, SheetHeight),
	% game geometry positions things at their bottom left corners, but
	% the renderer needs top left corners
	TopYPosition #= BottomYPosition - LevelHeight,

	Viewport = viewport(VLeft, VTop, _VWidth, _VHeight),

	% TODO: Cull stuff that isn't in the viewport
	ViewportX #= XPosition - VLeft,
	ViewportY #= TopYPosition - VTop,

	Write =  writable{
		type_id:TypeId,
		sprite:Sprite,
		viewport_x: ViewportX,
		viewport_y: ViewportY,
		level_width:LevelWidth,
		level_height:LevelHeight,
		sheet:SheetName,
		sheet_x:SheetX,
		sheet_y:SheetY,
		sheet_width:SheetWidth,
		sheet_height:SheetHeight
	}.

write_state(Tick, Viewport, State):-
	maplist(writable_mob(Tick, Viewport),
		State,
		Writables),
	json_write_dict(current_output,
		state{
			tick : Tick,
			sprites : Writables
			},
		[width(0)]),
	nl.

% A game produces "done" after writing a list of frames

game(OldState, Tick, LevelDimensions, ViewportDimensions):-
	is_endgame(OldState, Tick, LevelDimensions);
	Tick > 1000;
	!,
	after_physics(OldState, Tick, MovedState),
	after_agents(MovedState, Tick, NewState),
	ViewportDimensions = viewport_dimensions(VWidth, VHeight),
	Viewport = viewport(_VLeft, _VTop, VWidth, VHeight),
	viewport_follows_hero(OldState, LevelDimensions, Viewport),
	write(current_output, "## VIEWPORT: "), write(current_output, Viewport), nl,

	NextTick #= Tick + 1,
	write(current_output, "# "), write(current_output, next_state(NewState, NextTick)), nl,
	write_state(Tick, Viewport, NewState),

	( OldState = NewState; game(NewState, NextTick, LevelDimensions, ViewportDimensions)).

test_game():-
	% Need a better way to describe the initial state of a level
	game(
		[
			mob(hero, 0, 100, 0, 0, right),

			mob(brick, 96, 344, none, none, neutral),
			mob(brick, 128, 344, none, none, neutral),
			mob(brick, 160, 344, none, none, neutral),
			mob(brick, 192, 344, none, none, neutral),
			mob(brick, 224, 344, none, none, neutral),
			mob(brick, 256, 344, none, none, neutral),
			mob(brick, 288, 344, none, none, neutral),
			mob(brick, 320, 344, none, none, neutral),
			mob(brick, 352, 344, none, none, neutral),
			mob(brick, 384, 344, none, none, neutral),
			mob(brick, 416, 344, none, none, neutral),

			mob(brick, 192, 444, none, none, neutral),
			mob(brick, 224, 444, none, none, neutral),
			mob(brick, 256, 444, none, none, neutral),
			mob(brick, 288, 444, none, none, neutral)
		],
		0,
		level_dimensions(1280, 1280),
		viewport_dimensions(1280, 720)).


:- begin_tests(game).

minimum_absolute_delta(331, 312, 312, 312).

sprite_box(20, mob(brick, 96, 344, none, none, neutral), box(96, 312, 32, 32)).

mob_move_y(mob(hero, 84, 310, 4, 21, right), 20, [box(96, 312, 32, 32), box(128, 312, 32, 32)], mob(hero, 84, 311, 4, 0, right)).

after_physics(
	[mob(hero,80,310,4,20,right),mob(brick,96,344,none,none,neutral)],20,
	[mob(hero, 84, 311, 4, 0, right), mob(brick, 96, 344, none, none, neutral)]
	).

:- end_tests(game).
