:- module(mobs, [mob_type/2, mob_bottom/2, mob_left/2, mob_speed/2, mob_with_speed/3, mob_shift/4, mob_box/2]).

:- use_module(library(clpfd)).

:- use_module(assets).

mob_type(TypeId, mob(TypeId, _Left, _Bottom, _XSpeed, _YSpeed, _Facing)).

mob_speed(speed(XSpeed, YSpeed, Facing), mob(_TypeId, _Left, _Bottom, XSpeed, YSpeed, Facing)).

mob_left(Left, mob(_TypeId, Left, _Bottom, _XSpeed, _YSpeed, _Facing)).

mob_bottom(Bottom, mob(_TypeId, _Left, Bottom, _XSpeed, _YSpeed, _Facing)).

mob_with_speed(
	speed(NewXSpeed, NewYSpeed, NewFacing),
	mob(TypeId, Left, Bottom, _OldXSpeed, _OldYSpeed, _OldFacing),
	mob(TypeId, Left, Bottom, NewXSpeed, NewYSpeed, NewFacing)
	).

mob_box(Mob, box(Left, Top, Width, Height)):-
	mob_type(TypeId, Mob),
	mob_left(Left, Mob),
	mob_bottom(Bottom, Mob),
	hitbox_dimensions(TypeId, Width, Height),
	Top #= Bottom - Height.

mob_shift(ShiftLeft, ShiftTop, Mob, Shifted):-
	Mob = mob(TypeId, OriginalLeft, OriginalTop, XSpeed, YSpeed, Facing),
	NewLeft #= ShiftLeft + OriginalLeft,
	NewTop #= ShiftTop + OriginalTop,
	Shifted = mob(TypeId, NewLeft, NewTop, XSpeed, YSpeed, Facing). 
