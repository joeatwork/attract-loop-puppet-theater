:- module(assets, [sheet_geometry/11, hitbox_dimensions/3, sprite_sheet/6]).

:- use_module(library(clpfd)).

% Maps and metadata for images (and maybe sounds or whatever if we have them.)

% The hero is a 32x44 box that doesn't change shape. We associate
% rendering offsets with the current sprite.

% Offsets are relative to the TOP LEFT of the hitbox (unlike in physics where everything is bottom left)

% sheet_geometry(
%          MobType,
%          SheetIdentifier, 
%		   RenderOffsetX, RenderOffsetY, % offset of image from hitbox, in game coordinates 
%          LevelWidth, LevelHeight, % width and height in game coordinates
%          ImageFileName,
%          ImageOffsetX, ImageOffsetY, ImageWidth, ImageHeight % cropping for compositor, image coords
%         )

:- discontiguous(hitbox_dimensions/3).
:- discontiguous(sheet_geometry/11).

hitbox_dimensions(hero, 32, 44).

sheet_geometry(hero, standLeft, -2, -2, 60, 72,  "megaman/stand-left.png", 0, 0, 21, 24).

sheet_geometry(hero, standRight, -3, -2, 60, 72,  "megaman/stand-right.png", 0, 0, 21, 24).

% TODO: run cycle filenames are out of order and confusing.

sheet_geometry(hero, runLeft1, 0, -2, 48, 72, "megaman/run-left-2.png", 0, 9, 16, 24).

sheet_geometry(hero, runLeft2, -4, 0, 72, 66, "megaman/run-left-1.png", 0, 0, 24, 22).

sheet_geometry(hero, runLeft3, 0, -2, 48, 72, "megaman/run-left-2.png", 0, 9, 16, 24).

sheet_geometry(hero, runLeft4, -2, 0, 63, 66, "megaman/run-left-3.png", 0, 0, 21, 24).

sheet_geometry(hero, runRight1, 0, -2, 48, 72, "megaman/run-right-2.png", 0, 0, 16, 24).

sheet_geometry(hero, runRight2, -4, 0, 72, 66, "megaman/run-right-1.png", 0, 0, 24, 22).

sheet_geometry(hero, runRight3, 0, -2, 48, 72, "megaman/run-right-2.png", 0, 0, 16, 24).

sheet_geometry(hero, runRight4, -3, 0, 63, 66, "megaman/run-right-3.png", 0, 0, 21, 24).

sheet_geometry(hero, jumpLeft, -4, -2, 78, 90, "megaman/jump-left.png", 0, 0, 26, 30).

sheet_geometry(hero, jumpRight, -6, -2, 72, 90, "megaman/jump-right.png", 0, 0, 26, 30).

%%%%%%%%%%%

hitbox_dimensions(brick, 32, 32).

sheet_geometry(brick, brick, 0, 0, 32, 32, "placeholders/brick-16x16.png", 0, 0, 16, 16).

%%%%%%%%%%%%

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

sprite_sheet(hero, XSpeed, 0, left, Tick, runLeft3):-
	anim_frame(Tick, 3),
	XSpeed #\= 0.

sprite_sheet(hero, XSpeed, 0, left, Tick, runLeft4):-
	anim_frame(Tick, 4),
	XSpeed #\= 0.

% BUG: "jump if Yspeed #\= 0" means we have a weird stand at the top of the jump?

sprite_sheet(hero, _XSpeed, YSpeed, right, _Tick, jumpRight):-
	YSpeed #\= 0.

sprite_sheet(hero, _XSpeed, YSpeed, left, _Tick, jumpLeft):-
	YSpeed #\= 0.

sprite_sheet(brick, _XSpeed, _YSpeed, _Facing, _Tick, brick).

