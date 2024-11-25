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
:- use_module(prolog/physics).

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
	CenteredLeft #= max(HeroLeft - HalfVWidth, 0),
	CenteredTop #= max(HeroBottom - HalfVHeight, 0),
	VLeft #= min(CenteredLeft, LevelWidth - VWidth),
	VTop #= min(CenteredTop, LevelHeight - VHeight).


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

game(StartState, Tick, LevelDimensions, ViewportDimensions):-
	is_endgame(StartState, Tick, LevelDimensions);
	Tick > 1000;
	!,
	after_physics(StartState, Tick, PostPhysicsState),
	after_agents(PostPhysicsState, Tick, NewState),
	ViewportDimensions = viewport_dimensions(VWidth, VHeight),
	Viewport = viewport(_VLeft, _VTop, VWidth, VHeight),
	viewport_follows_hero(NewState, LevelDimensions, Viewport),
	write(current_output, "## VIEWPORT: "), write(current_output, Viewport), nl,

	NextTick #= Tick + 1,
	write(current_output, "# "), write(current_output, next_state(NewState, NextTick)), nl,
	write_state(Tick, Viewport, NewState),

	( StartState = NewState; game(NewState, NextTick, LevelDimensions, ViewportDimensions)).

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
			mob(brick, 320, 344, none, none, neutral)
		%	mob(brick, 352, 344, none, none, neutral),
		%	mob(brick, 384, 344, none, none, neutral),
		%	mob(brick, 416, 344, none, none, neutral)
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
