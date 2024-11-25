:- module(mobs, [mob_type/2, mob_bottom/2, mob_speed/2, mob_with_speed/3, sprite_box/3]).

:- use_module(library(clpfd)).

:- use_module(assets).

mob_type(TypeId, mob(TypeId, _Left, _Bottom, _XSpeed, _YSpeed, _Facing)).

mob_speed(speed(XSpeed, YSpeed, Facing), mob(_TypeId, _Left, _Bottom, XSpeed, YSpeed, Facing)).

mob_bottom(Bottom, mob(_TypeId, _Left, Bottom, _XSpeed, _YSpeed, _Facing)).

mob_with_speed(
	speed(NewXSpeed, NewYSpeed, NewFacing),
	mob(TypeId, Left, Bottom, _OldXSpeed, _OldYSpeed, _OldFacing),
	mob(TypeId, Left, Bottom, NewXSpeed, NewYSpeed, NewFacing)
	).

sprite_box(Tick, Mob, box(XPosition, BoxY, Width, Height)):-
	Mob = mob(TypeId, XPosition, YPosition, XSpeed, YSpeed, Facing),
	sprite_sheet(TypeId, XSpeed, YSpeed, Facing, Tick, Sprite),
	sheet_geometry(TypeId, Sprite, Width, Height, _Sheet, _SheetX, _SheetY, _SheetWidth, _SheetHeight),
	% YPosition is bottom left, box position is top left.
	BoxY #= YPosition - Height.
