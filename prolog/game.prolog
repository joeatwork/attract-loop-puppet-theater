:- module(game, [run_game/4]).

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


% Y speed is positive toward the bottom of the screen and negative
% toward the top of the screen.
% X speed of a mob is always positive, and is modified by the facing
% of the mob.

% Units
% 16:9 screen aspect ratio
% 1280 x 720 abstract units of width and height
% (for comparison, NES is 240 x 240 pixels, each pixel is 8:7)

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


sprite_for(Tick, Mob, Peers, Sprite):-
	Mob = mob(TypeId, _RawLeft, _RawBottom, XSpeed, YSpeed, Facing),
	(
		standing(Mob, Peers) -> 
			sprite_sheet(rooted, TypeId, XSpeed, YSpeed, Facing, Tick, Sprite)
		;
		sprite_sheet(floating, TypeId, XSpeed, YSpeed, Facing, Tick, Sprite)
	).


writable_mob(Tick, Viewport, AllMobs, Mob, Write):-
	exclude(=(Mob), AllMobs, Peers),
	Mob = mob(TypeId, _RawLeft, _RawBottom, _XSpeed, _YSpeed, _Facing),
	sprite_for(Tick, Mob, Peers, Sprite),
	sheet_geometry(TypeId, Sprite,
			 LevelOffsetX, LevelOffsetY, LevelWidth, LevelHeight,
			 SheetName, SheetX, SheetY, SheetWidth, SheetHeight),
	mob_box(Mob, box(HitLeft, HitTop, _HitWidth, _HitHeight)),
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
	maplist(writable_mob(Tick, Viewport, State),
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

game(Tick, AgentState, StartMobs, TargetBox, LevelDimensions, ViewportDimensions):-
	is_endgame(StartMobs, Tick, LevelDimensions);
	Tick > 1000;
	!,
	control_hero(TargetBox, StartMobs, AgentState, NewAgentState, IntentionMobs),
	after_physics(IntentionMobs, MovedMobs),
	ViewportDimensions = viewport_dimensions(VWidth, VHeight),
	Viewport = viewport(_VLeft, _VTop, VWidth, VHeight),
	viewport_follows_hero(MovedMobs, LevelDimensions, Viewport),

	NextTick #= Tick + 1,
	write(current_output, "# "), write(current_output, next_state(NextTick, MovedMobs)), nl,
	write_state(Tick, Viewport, MovedMobs),

	( StartMobs = MovedMobs; game(NextTick, NewAgentState, MovedMobs, TargetBox, LevelDimensions, ViewportDimensions)).


run_game(StartMobs, TargetBox, LevelDimensions, ViewportDimensions):-
	game(0, % Tick 
		none, % AgentState
		StartMobs,
		TargetBox,
		LevelDimensions,
		ViewportDimensions).