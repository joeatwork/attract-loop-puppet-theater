#!/usr/bin/env swipl

% Constraint library for dealing with geometry and positions.
:- use_module(library(clpfd)).
:- use_module(library(http/json)).
:- use_module(library(main)).

:- initialization(main, main).

main(_Argv):-
	test_game().

% UTILS: These are probably already builtins or standard lib things,
% but I don't know the standard library

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

% bounds(MobId, Sprite, Width, Height).
% These are measured from the actual sprites. Metrics are in "scxreen units"

% Sprite sheet geometry
% sheet_geometry(mobId, spriteId, widthUnits, heightUnits, sheetName, sheetOffsetX, sheetOffsetY, sheetWidth, sheetHeight)

sheet_geometry(hero, standLeft, 60, 72, "megaman/stand-left.png", 0, 0, 21, 24).

sheet_geometry(hero, standRight, 60, 72, "megaman/stand-right.png", 0, 0, 21, 24).

% TODO: run cycle filenames are out of order and confusing.
% TODO: Also, it's not clear what value the atom "runLeft1" really gives us.
% TODO: Consider animation(left, sprite) or something

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

% TODO: Returning 

mob_move(OldX, OldY, XSpeed, YSpeed, right, NewX, NewY):-
	NewX #= OldX + XSpeed,
	NewY #= OldY + YSpeed.

mob_move(OldX, OldY, XSpeed, YSpeed, left, NewX, NewY):-
	NewX #= OldX - XSpeed,
	NewY #= OldY + YSpeed.

% TODO: Physics and collision detection should be intertwingled;
% in particular, what collides with what after a move
% (and how to reposition a thing after a move)
% matters.
% TODO: *All* mobs wanna move, not just hero

after_physics(OldState, _Tick, NewState):-
	pluck_from_list(OldState,
		mob(hero, XPosition, YPosition, XSpeed, YSpeed, Facing),
		RemainingState),
	mob_move(XPosition, YPosition, XSpeed, YSpeed, Facing, NewX, NewY),
	NewState = [mob(hero, NewX, NewY, XSpeed, YSpeed, Facing)|RemainingState].

is_endgame([], _Tick, _Bounds).

is_endgame(State, Tick, level_bounds(LevelWidth, LevelHeight)):-
	pluck_from_list(State,
		mob(hero, XPosition, YPosition, XSpeed, YSpeed, Facing),
		_Rest),
	sprite(mob(hero, XPosition, YPosition, XSpeed, YSpeed, Facing),
		Tick,
		Sprite),
	sheet_geometry(hero, Sprite, HeroWidth, HeroHeight, _SheetName, _SheetX, _SheetY, _SheetW, _SheetH),
	XPosition + HeroWidth #< 0,
	XPosition #> LevelWidth,
	YPosition + HeroHeight #< 0,
	YPosition #> LevelHeight.

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

game(OldState, Tick):-
	is_endgame(OldState,
		Tick,
		level_bounds(1280, 720));
% Timeout
Tick > 100;
!,
	NextTick #= Tick + 1,
	after_physics(OldState, Tick, NewState),
	% TODO: game state is gonna be a list of sprites and positions in a viewport
	write_state(Tick, NewState),
	game(NewState, NextTick).

test_game():-
	game([mob(hero, 0, 0, 4, 0, right)],
		0).
