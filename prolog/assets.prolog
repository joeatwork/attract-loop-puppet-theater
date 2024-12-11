:- module(assets, [sheet_geometry/11, hitbox_dimensions/3, sprite_sheet/6]).

:- use_module(library(clpfd)).

% Maps and metadata for images (and maybe sounds or whatever if we have them.)

% The hero is a 32x44 box that doesn't change shape. We associate
% rendering offsets with the current sprite.

% Offsets are relative to the TOP LEFT of the hitbox (unlike in physics where everything is bottom left)

% sheet_geometry(
%          MobType,
%          SheetIdentifier, 
%		   RenderOffsetX, RenderOffsetY, % offset of top left of the image from hitbox, in game coordinates 
%          LevelWidth, LevelHeight, % width and height in game coordinates
%          ImageFileName,
%          ImageOffsetX, ImageOffsetY, ImageWidth, ImageHeight % cropping for compositor, image coords
%         )

:- discontiguous(hitbox_dimensions/3).
:- discontiguous(sheet_geometry/11).


hitbox_dimensions(hero, 48, 66).

sheet_geometry(hero, standLeft, -16, -5, 70, 75, "lonely_robot/stand-left.png", 0, 0, 140, 150).

sheet_geometry(hero, standRight, -13, -5, 70, 75, "lonely_robot/stand-right.png", 0, 0, 140, 150).

sheet_geometry(hero, runLeft1, -9, -8, 58, 78,  "lonely_robot/run-left-centered.png", 0, 0, 116, 156).

sheet_geometry(hero, runLeft2, -29, -5, 91, 70, "lonely_robot/run-left-inner-leg.png", 0, 0, 182, 140).

sheet_geometry(hero, runLeft3, -9, -8, 58, 78,  "lonely_robot/run-left-centered.png", 0, 0, 116, 156).

sheet_geometry(hero, runLeft4, -10, -5, 78, 70, "lonely_robot/run-left-outer-leg.png", 0, 0, 156, 140).

sheet_geometry(hero, runRight1, -10, -8, 58, 78,  "lonely_robot/run-right-centered.png", 0, 0, 116, 156).

sheet_geometry(hero, runRight2, -22, -5, 91, 70, "lonely_robot/run-right-inner-leg.png", 0, 0, 182, 140).

sheet_geometry(hero, runRight3, -10, -8, 58, 78,  "lonely_robot/run-right-centered.png", 0, 0, 116, 156).

sheet_geometry(hero, runRight4, -29, -5, 78, 70, "lonely_robot/run-right-outer-leg.png", 0, 0, 156, 140).

sheet_geometry(hero, jumpLeft, -24, -20, 68, 116, "lonely_robot/jump-left.png", 0, 0, 136, 232).

sheet_geometry(hero, jumpRight, -6, -20, 68, 116, "lonely_robot/jump-right.png", 0, 0, 136, 232).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hitbox_dimensions(megaman, 48, 66).

sheet_geometry(megaman, standLeft, -6, -6, 60, 72,  "megaman/stand-left.png", 0, 0, 21, 24).

sheet_geometry(megaman, standRight, -9, -6, 60, 72,  "megaman/stand-right.png", 0, 0, 21, 24).

% TODO: run cycle filenames are out of order and confusing.

sheet_geometry(megaman, runLeft1, 0, -6, 48, 72, "megaman/run-left-2.png", 0, 9, 16, 24).

sheet_geometry(megaman, runLeft2, -12, 0, 72, 66, "megaman/run-left-1.png", 0, 0, 24, 22).

sheet_geometry(megaman, runLeft3, 0, -6, 48, 72, "megaman/run-left-2.png", 0, 9, 16, 24).

sheet_geometry(megaman, runLeft4, -6, 0, 63, 66, "megaman/run-left-3.png", 0, 0, 21, 22).

sheet_geometry(megaman, runRight1, 0, -6, 48, 72, "megaman/run-right-2.png", 0, 0, 16, 24).

sheet_geometry(megaman, runRight2, -12, 0, 72, 66, "megaman/run-right-1.png", 0, 0, 24, 22).

sheet_geometry(megaman, runRight3, 0, -6, 48, 72, "megaman/run-right-2.png", 0, 0, 16, 24).

sheet_geometry(megaman, runRight4, -9, 0, 63, 66, "megaman/run-right-3.png", 0, 0, 21, 22).

sheet_geometry(megaman, jumpLeft, -12, -6, 78, 90, "megaman/jump-left.png", 0, 0, 26, 30).

sheet_geometry(megaman, jumpRight, -18, -6, 72, 90, "megaman/jump-right.png", 0, 0, 26, 30).

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

