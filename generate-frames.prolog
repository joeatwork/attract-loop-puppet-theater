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


% We render 60 fps, run cycle is 12fps, so we spend five frames per sprite

% We want XSpeed to be 1.375 game units per frame
% YSpeed of a jump is 4.875 game units per frame starting, with a 0.25 game unit per frame per frame gravity

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


% facing is left or right
% mob(type_identifier, xposition, yposition, xspeed, yspeed, facing)

sprite(mob(hero, _XPosition, _YPosition, 0, 0, left), _Tick, standLeft).

sprite(mob(hero, _XPosition, _YPosition, 0, 0, right), _Tick, standRight).

sprite(mob(hero, _XPosition, _YPosition, XSpeed, 0, right), Tick, runRight1):-
	anim_frame(Tick, 1),
	XSpeed #\= 0.

sprite(mob(hero, _XPosition, _YPosition, XSpeed, 0, right), Tick, runRight2):-
	anim_frame(Tick, 2),
	XSpeed #\= 0.

sprite(mob(hero, _XPosition, _YPosition, XSpeed, 0, right), Tick, runRight3):-
	anim_frame(Tick, 3),
	XSpeed #\= 0.

sprite(mob(hero, _XPosition, _YPosition, XSpeed, 0, right), Tick, runRight4):-
	anim_frame(Tick, 4),
	XSpeed #\= 0.

sprite(mob(hero, _XPosition, _YPosition, XSpeed, 0, left), Tick, runLeft1):-
	anim_frame(Tick, 1),
	XSpeed #\= 0.

sprite(mob(hero, _XPosition, _YPosition, XSpeed, 0, left), Tick, runLeft2):-
	anim_frame(Tick, 2),
	XSpeed #\= 0.

sprite(mob(hero, _XPosition, _YPosition, Xspeed, 0, left), Tick, runleft3):-
	anim_frame(Tick, 3),
	Xspeed #\= 0.

sprite(mob(hero, _XPosition, _YPosition, Xspeed, 0, left), Tick, runleft4):-
	anim_frame(Tick, 4),
	Xspeed #\= 0.

sprite(mob(hero, _XPosition, _YPosition, _XSpeed, YSpeed, right), _Tick, jumpRight):-
	YSpeed #\= 0.

sprite(mob(hero, _XPosition, _YPosition, _XSpeed, YSpeed, left), _Tick, jumpLeft):-
	YSpeed #\= 0.

sprite(mob(brick, _XPosition, _YPosition, _XSpeed, _YSpeed, _Facing), _Tick, brick).

% bounds(MobId, Sprite, Width, Height).
% These are measured from the actual sprites. Metrics are in "scxreen units"

% Sprite sheet geometry
% sheet_geometry(mobId, spriteId, widthUnits, heightUnits, sheetName, sheetOffsetX, sheetOffsetY, sheetWidth, sheetHeight)

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

% Brick

sheet_geometry(brick, brick, 32, 32, "placeholders/brick-16x16.png", 0, 0, 16, 16).

% TODO: This ain't right at all. Need to do hitbox testing.
% TODO: Also, "jump if YSpeed is zero" means we'll flash a stand as we leap into an abyss...

overlaps(box(X1, Y1, Width1, Height1), box(X2, Y2, Width2, Height2)):-
	% We allow (exactly) shared bounds without considering them overlap
	X1End #= X1 + Width1,
	X2End #= X2 + Width2,
	Y1End #= Y1 + Height1,
	Y2End #= Y2 + Height2,
	( between(X2, X2End, X1); between(X1, X1End, X2) ),
	( between(Y2, Y2End, Y1); between(Y1, Y1End, Y2)). 

% TODO: Fix these bugs!
% TWO BUGS
% - We assume we collide with only one counterpart.
% - We assume our motion is short enough that we'll never teleport across a counterpart.
move_collides(TargetBox, [H | L], Counterpart):-
	(overlaps(TargetBox, H), H = Counterpart);
	move_collides(TargetBox, L, Counterpart).

move_right_until_collision(TargetBox, Collidables, DestX):-
	move_collides(TargetBox, Collidables, box(BarrierX, _BarrierY, _BarrierW, _BarrierH)),
	TargetBox = box(_TargetX, _TargetY, TargetWidth, _TargetHeight),
	DestX #= (BarrierX - TargetWidth) - 1.

move_left_until_collision(TargetBox, Collidables, DestX):-
	move_collides(TargetBox, Collidables, box(BarrierX, _BarrierY, BarrierWidth, _BarrierH)),	
	DestX #= BarrierX + BarrierWidth + 1.

% Up is negative Y
move_up_until_collision(TargetBox, Collidables, DestY):-
	move_collides(TargetBox, Collidables, box(_BarrierX, BarrierY, _BarrierWidth, BarrierHeight)),
	DestY #= BarrierY + BarrierHeight + 1.

move_down_until_collision(TargetBox, Collidables, DestY):-
	move_collides(TargetBox, Collidables, box(_BarrierX, BarrierY, _BarrierWidth, _BarrierHeight)),
	TargetBox = box(_TargetX, _TargetY, _TargetWidth, TargetHeight),
	DestY #= (BarrierY - TargetHeight) - 1.

sprite_box(Tick, Mob, box(XPosition, YPosition, Width, Height)):-
	sprite(Mob, Tick, Sprite),
	Mob = mob(TypeId, XPosition, YPosition, _XSpeed, _YSpeed, _Facing),
	sheet_geometry(TypeId, Sprite, Width, Height, _Sheet, _SheetX, _SheetY, _SheetWidth, _SheetHeight).

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


mob_move_x(Mob, _Tick, _Collidables, XPosition, none):-
	Mob = mob(_TypeId, XPosition, _YPosition, none, _YSpeed, _Facing).

mob_move_x(Mob, Tick, Collidables, NewX, NewXSpeed):-
	% Glitch Here! We calculate collisions with the destination bounding box,
	% But if a collision happens, we may render a different sprite!
	mob_move_x_toward_facing(Mob, Moved),
	sprite_box(Tick, Moved, TargetBox),
	Moved = mob(_TypeId, TargetX, _YPosition, XSpeed, _YSpeed, Facing),
	(
		(Facing = right, move_right_until_collision(TargetBox, Collidables, NewX), NewXSpeed = 0);
		(Facing = left, move_left_until_collision(TargetBox, Collidables, NewX), NewXSpeed = 0);
		(NewX = TargetX, NewXSpeed = XSpeed)
	).

mob_move_y(Mob, _Tick, _Collidables, YPosition, none):-
	Mob = mob(_TypeId, _XPosition, YPosition, _XSpeed, none, _Facing).	

mob_move_y(Mob, Tick, Collidables, NewY, NewYSpeed):-
	Mob = mob(TypeId, XPosition, YPosition, XSpeed, YSpeed, Facing),
	TargetY #= YPosition + YSpeed,
	integer(YSpeed),

	% Glitch Here! We calculate collisions with the destination bounding box,
	% But if a collision happens, we may render a different sprite!
	Moved = mob(TypeId, XPosition, TargetY, XSpeed, YSpeed, Facing),
	sprite_box(Tick, Moved, TargetBox),

	(
		(YSpeed #> 0, move_down_until_collision(TargetBox, Collidables, NewY), NewYSpeed = 0);
		(YSpeed #< 0, move_up_until_collision(TargetBox, Collidables, NewY), NewYSpeed = 0);
		(TargetY = NewY, NewYSpeed = YSpeed)
	).

mob_move(Mob, Tick, Collidables, NewX, NewY, NewXSpeed, NewYSpeed):-
	mob_move_x(Mob, Tick, Collidables, NewX, NewXSpeed),
	mob_move_y(Mob, Tick, Collidables, NewY, NewYSpeed).

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
	Mover = mob(hero, _XPosition, _YPosition, _XSpeed, _YSpeed, Facing),
	pluck_from_list(Accellerated, Mover, Others),
	maplist(sprite_box(Tick), Others, Collidables),
	mob_move(Mover, Tick, Collidables, NewX, NewY, NewXSpeed, NewYSpeed),
	NewState = [mob(hero, NewX, NewY, NewXSpeed, NewYSpeed, Facing)|Others].

is_endgame([], _Tick, _Bounds).

is_endgame(State, Tick, level_bounds(LevelWidth, LevelHeight)):-
	list_contains(State,
		mob(hero, XPosition, YPosition, XSpeed, YSpeed, Facing)),
	sprite(mob(hero, XPosition, YPosition, XSpeed, YSpeed, Facing),
		Tick,
		Sprite),
	sheet_geometry(hero, Sprite, HeroWidth, HeroHeight, _SheetName, _SheetX, _SheetY, _SheetW, _SheetH),
	( XPosition + HeroWidth #< 0;  XPosition #> LevelWidth; YPosition + HeroHeight #< 0; YPosition #> LevelHeight ).

% TODO: We should know the sprite sheet geometry here
% TODO: We should know about SCREEN geometry here, too!

writable_mob(Tick, mob(TypeId, XPosition, YPosition, XSpeed, YSpeed, Facing), writable{type_id:TypeId, sprite:Sprite, level_x:XPosition, level_y:YPosition, level_width:LevelWidth, level_height:LevelHeight, sheet:SheetName, sheet_x:SheetX, sheet_y:SheetY, sheet_width:SheetWidth, sheet_height:SheetHeight}):-
	sprite(mob(TypeId, XPosition, YPosition, XSpeed, YSpeed, Facing),
		Tick,
		Sprite),
	sheet_geometry(TypeId, Sprite, LevelWidth, LevelHeight, SheetName, SheetX, SheetY, SheetWidth, SheetHeight).

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
	NextTick #= Tick + 1,
	after_physics(OldState, Tick, NewState),
	% TODO: game state is gonna be a list of sprites and positions in a viewport
	write_state(Tick, NewState),

	( OldState = NewState; game(NewState, NextTick, Bounds)).

test_game():-
	% Need a better way to describe the initial state of a level
	game(
		[
			mob(hero, 0, 0, 4, 0, right),
			mob(brick, 0, 344, none, none, neutral),
			mob(brick, 32, 344, none, none, neutral),
			mob(brick, 64, 344, none, none, neutral),
			mob(brick, 96, 344, none, none, neutral),
			mob(brick, 128, 344, none, none, neutral)
		],
		0,
		level_bounds(1280, 720)).
