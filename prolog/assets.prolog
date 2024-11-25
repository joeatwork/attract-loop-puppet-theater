:- module(assets, [sheet_geometry/9, sprite_height/3, sprite_width/3, sprite_sheet/6]).

:- use_module(library(clpfd)).


% Maps and metadata for images (and maybe sounds or whatver if we have them.)

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

%%%%%%%%%%%

sheet_geometry(brick, brick, 32, 32, "placeholders/brick-16x16.png", 0, 0, 16, 16).

%%%%%%%%%%%%

sheet_geometry(goal, goal, 32, 32, "placeholders/goal-32x32.png", 0, 0, 32, 32).

sprite_height(TypeId, Sprite, Height):-
	sheet_geometry(TypeId, Sprite, _Width, Height, _SheetName, _SheetOffsetX, _SheetOffsetY, _SheetWidth, _SheetHeight).

sprite_width(TypeId, Sprite, Width):-
	sheet_geometry(TypeId, Sprite, Width, _Height, _SheetName, _SheetOffsetX, _SheetOffsetY, _SheetWidth, _SheetHeight).

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

