% Constraint library for dealing with geometry and positions.
:- use_module(library(clpfd)).
:- use_module(library(http/json)).

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
% 16 x 9 screen
% 1280 x 720 abstract units
% 
%
% (for comparison, NES is 240 x 240 non-square pixels)
% Mega man standing is 72 units high by 60 units wide

% facing is left or right
% mob(type_identifier, xposition, yposition, xspeed, yspeed, facing)

sprite(mob(megaman, _XPosition, _YPosition, 0, 0, left), _Tick, standLeft).

sprite(mob(megaman, _XPosition, _YPosition, 0, 0, right), _Tick, standRight).

sprite(mob(megaman, _XPosition, _YPosition, XSpeed, 0, right), Tick, runRight1):-
	Tick mod 3 =:= 0,
	XSpeed \= 0.

sprite(mob(megaman, _XPosition, _YPosition, XSpeed, 0, right), Tick, runRight2):-
	Tick mod 3 =:= 1,
	XSpeed \= 0.

sprite(mob(megaman, _XPosition, _YPosition, XSpeed, 0, right), Tick, runRight3):-
	Tick mod 3 =:= 2,
	XSpeed \= 0.

sprite(mob(megaman, _XPosition, _YPosition, XSpeed, 0, left), Tick, runLeft1):-
	Tick mod 3 =:= 0,
	XSpeed \= 0.

sprite(mob(megaman, _XPosition, _YPosition, XSpeed, 0, left), Tick, runLeft2):-
	Tick mod 3 =:= 1,
	XSpeed \= 0.

sprite(mob(megaman, _XPosition, _YPosition, XSpeed, 0, left), Tick, runLeft3):-
	Tick mod 3 =:= 2,
	XSpeed \= 0.

sprite(mob(megaman, _XPosition, _YPosition, _XSpeed, YSpeed, right), _Tick, jumpRight):-
	YSpeed \= 0.

sprite(mob(megaman, _XPosition, _YPosition, _XSpeed, YSpeed, left), _Tick, jumpLeft):-
	YSpeed \= 0.

% bounds(MobId, Sprite, Width, Height).
% These are measured from the actual sprites. Metrics are in "scxreen units"

dimensions(megaman, standLeft, 60, 72).

dimensions(megaman, standRight, 60, 72).

dimensions(megaman, runRight1, 72, 66).

dimensions(megaman, runRight2, 48, 72).

dimensions(megaman, runRight3, 63, 66).

dimensions(megaman, runLeft1, 72, 66).

dimensions(megaman, runLeft2, 48, 72).

dimensions(megaman, runLeft3, 63, 66).

dimensions(megaman, jumpRight1, 78, 90).

dimensions(megaman, jumpLeft1, 78, 90).

% TODO: Returning 

hero_move(OldX, OldY, XSpeed, YSpeed, right, NewX, NewY):-
	NewX is OldX + XSpeed,
	NewY is OldY + YSpeed.

hero_move(OldX, OldY, XSpeed, YSpeed, left, NewX, NewY):-
	NewX is OldX - XSpeed,
	NewY is OldY + YSpeed.

hero_agent_update(OldState, _Tick, NewState):-
	pluck_from_list(OldState,
		mob(megaman, XPosition, YPosition, XSpeed, YSpeed, Facing),
		RemainingState),
	hero_move(XPosition, YPosition, XSpeed, YSpeed, Facing, NewX, NewY),
	NewState = [mob(megaman, NewX, NewY, XSpeed, YSpeed, Facing)|RemainingState].

is_endgame([], _Tick, _Bounds).

is_endgame(State, Tick, level_bounds(LevelWidth, LevelHeight)):-
	pluck_from_list(State,
		mob(megaman, XPosition, YPosition, XSpeed, YSpeed, Facing),
		_Rest),
	sprite(mob(megaman, XPosition, YPosition, XSpeed, YSpeed, Facing),
		Tick,
		Sprite),
	dimensions(megaman, Sprite, HeroWidth, HeroHeight),
	XPosition + HeroWidth < 0,
	XPosition > LevelWidth,
	YPosition + HeroHeight < 0,
	YPosition > LevelHeight.

% TODO: We should know the sprite sheet geometry here

writable_mob(Tick, mob(TypeId, XPosition, YPosition, XSpeed, YSpeed, Facing),  writable{type_id:TypeId, sprite:Sprite, x:XPosition, y:YPosition}):-
	sprite(mob(TypeId, XPosition, YPosition, XSpeed, YSpeed, Facing),
		Tick,
		Sprite).

write_state(Tick, State):-
	maplist(writable_mob(Tick), State, Writables),
	json_write_dict(current_output, state{
		tick: Tick,
		sprites: Writables
	}),
	nl.


% Right now, game state is just a list of Mobs

% A game produces "done" after writing a list of frames

game(OldState, Tick):-
	is_endgame(OldState,
		Tick,
		level_bounds(1280, 720));
Tick > 100;
!,
	NextTick is Tick + 1,
	hero_agent_update(OldState, Tick, NewState),
	% TODO: game state is gonna be a list of sprites and positions in a viewport
	write_state(Tick, NewState),
	game(NewState, NextTick).

test_game():-
	game([mob(megaman, 0, 0, 4, 0, right)],
		0).