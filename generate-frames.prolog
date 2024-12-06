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

after_agents(Mobs, AgentState, NewAgentState, NewMobs):-
	control_hero(
		box(360, 2000, 32, 32), % Target
		Mobs,
		AgentState,
		NewAgentState,
		NewMobs).

is_endgame([], _Tick, _Bounds).

% TODO Remove _Tick argument
is_endgame(Mobs, _Tick, level_dimensions(LevelWidth, LevelHeight)):-
	include(mob_type(hero), Mobs, [Hero]),
	mob_box(Hero, box(Left, Top, Width, Height)),
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
	Mob = mob(TypeId, _RawLeft, _RawBottom, XSpeed, YSpeed, Facing),
	mob_box(Mob, box(HitLeft, HitTop, _HitWidth, _HitHeight)),
	sprite_sheet(TypeId, XSpeed, YSpeed, Facing, Tick, Sprite),
	sheet_geometry(TypeId, Sprite,
			 LevelOffsetX, LevelOffsetY, LevelWidth, LevelHeight,
			 SheetName, SheetX, SheetY, SheetWidth, SheetHeight),

	Viewport = viewport(VLeft, VTop, _VWidth, _VHeight),
	ViewportX #= (HitLeft + LevelOffsetX) - VLeft,
	ViewportY #= (HitTop + LevelOffsetY) - VTop,

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

game(StartMobs, Tick, AgentState, LevelDimensions, ViewportDimensions):-
	is_endgame(StartMobs, Tick, LevelDimensions);
	Tick > 1000;
	!,
	after_agents(StartMobs, AgentState, NewAgentState, IntentionMobs),
	after_physics(IntentionMobs, MovedMobs),
	ViewportDimensions = viewport_dimensions(VWidth, VHeight),
	Viewport = viewport(_VLeft, _VTop, VWidth, VHeight),
	viewport_follows_hero(MovedMobs, LevelDimensions, Viewport),
	write(current_output, "## VIEWPORT: "), write(current_output, Viewport), nl,

	NextTick #= Tick + 1,
	write(current_output, "# "), write(current_output, next_state(MovedMobs, NextTick)), nl,
	write_state(Tick, Viewport, MovedMobs),

	( StartMobs = MovedMobs; game(MovedMobs, NextTick, NewAgentState, LevelDimensions, ViewportDimensions)).

test_game():-
	% Need a better way to describe the initial state of a level
	game(
		[
			mob(hero, 320, 311, 0, 0, right),

			mob(brick, 288, 344, none, none, neutral),
			mob(brick, 320, 344, none, none, neutral)
		],
		0,
		no_initial_state,
		level_dimensions(1280, 1280),
		viewport_dimensions(1280, 720)).
