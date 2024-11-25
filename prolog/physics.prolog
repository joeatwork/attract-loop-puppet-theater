:- module(physics, 
    [
        after_physics/3,
        box_top/2,
        box_bottom/2,
        box_left/2,
        box_right/2,
		box_distance_squared/3,
        move_box/4,
		overlaps/2,
        collisions/3,
        move_down_until_collision/3
    ]).

:- use_module(library(clpfd)).
:- use_module(mobs).

box_top(Top, box(_Left, Top, _Width, _Height)).
box_bottom(Bottom, box(_Left, Top, _Width, Height)):-
	Bottom #= Top + Height.
box_left(Left, box(Left, _Top, _Width, _Height)).
box_right(Right, box(Left, _Top, Width, _Height)):-
	Right #= Left + Width.

box_distance_squared(B1, B2, DistanceSquared):-
	box_left(Left1, B1), box_top(Top1, B1), box_right(Right1, B1), box_bottom(Bot1, B1),
	box_left(Left2, B2), box_top(Top2, B2), box_right(Right2, B2), box_bottom(Bot2, B2),
	Right #= max(Left1, Left2),
	Left #= min(Right1, Right2),
	Bottom #= max(Top1, Top2),
	Top #= min(Bot1, Bot2),
	Horiz #= max(0, Right - Left),
	Vert #= max(0, Bottom - Top),
	DistanceSquared #= (Horiz * Horiz) + (Vert * Vert).


move_box(MoveX, MoveY, box(Left, Top, Width, Height), box(NewLeft, NewTop, Width, Height)):-
    NewLeft #= Left + MoveX,
    NewTop #= Top + MoveY.

overlaps(box(Left1, Top1, Width1, Height1), box(Left2, Top2, Width2, Height2)):-
	% We allow (exactly) shared bounds without considering them overlap
	Left1End #= Left1 + Width1,
	Left2End #= Left2 + Width2,
	Top1End #= Top1 + Height1,
	Top2End #= Top2 + Height2,
	( between(Left2, Left2End, Left1); between(Left1, Left1End, Left2) ),
	( between(Top2, Top2End, Top1); between(Top1, Top1End, Top2)). 

collisions(TargetBox, OtherBoxes, Collisions):-
	include(overlaps(TargetBox), OtherBoxes, Collisions).

min_pair(K1-V1, K2-V2, K-V):-
	(K1 #< K2, K=K1, V=V1);
	(K=K2, V=V2).

minimum_absolute_delta(Target, Other1, Other2, Min):-
	AD1 #= abs(Target - Other1),
	AD2 #= abs(Target - Other2),
	min_pair(AD1-Other1, AD2-Other2, _Abs-Min).

move_right_until_collision(TargetBox, Collidables, LeftBarrier):-
	collisions(TargetBox, Collidables, Collisions),
	maplist(box_left(), [Lft | LftList], Collisions),
	box_right(Frontier, TargetBox),
	foldl(minimum_absolute_delta(Frontier), LftList, Lft, LeftBarrier).

move_left_until_collision(TargetBox, Collidables, RightBarrier):-
	collisions(TargetBox, Collidables, Collisions),
	maplist(box_right(), [Rt | RtList ], Collisions),
	box_left(Frontier, TargetBox),
	foldl(minimum_absolute_delta(Frontier), RtList, Rt, RightBarrier).

move_up_until_collision(TargetBox, Collidables, BarrierAbove):-
	collisions(TargetBox, Collidables, Collisions),
	maplist(box_bottom(), [Btm|BtmList], Collisions),
	box_top(Frontier, TargetBox),
	foldl(minimum_absolute_delta(Frontier), BtmList, Btm, BarrierAbove).

move_down_until_collision(TargetBox, Collidables, BarrierBelow):-
	collisions(TargetBox, Collidables, Collisions),
	maplist(box_top(), [Top|TopList], Collisions),
	box_bottom(Frontier, TargetBox),
	foldl(minimum_absolute_delta(Frontier), TopList, Top, BarrierBelow).

mob_move_x_toward_facing(Mob, Moved):-
	Mob = mob(TypeId, XPosition, YPosition, XSpeed, YSpeed, right),
	integer(XSpeed),
	TargetX #= XPosition + XSpeed,
	Moved = mob(TypeId, TargetX, YPosition, XSpeed, YSpeed, right).

mob_move_x_toward_facing(Mob, Moved):-
	Mob = mob(TypeId, XPosition, YPosition, XSpeed, YSpeed, left),
	integer(XSpeed),
	TargetX #= XPosition - XSpeed,
	Moved = mob(TypeId, TargetX, YPosition, XSpeed, YSpeed, left).


mob_move_x(Mob, _Tick, _Collidables, Mob):-
	Mob = mob(_TypeId, _XPosition, _YPosition, none, _YSpeed, _Facing).

mob_move_x(Mob, Tick, Collidables, Moved):-
	mob_move_x_toward_facing(Mob, MoveTarget),
	sprite_box(Tick, MoveTarget, TargetBox),
	MoveTarget = mob(TypeId, _TargetX, YPosition, _XSpeed, YSpeed, Facing),
	(
		(
			Facing = right,
			move_right_until_collision(TargetBox, Collidables, LeftBarrier),
			sprite_sheet(TypeId, 0, YSpeed, Facing, Tick, Sprite),
			sprite_width(TypeId, Sprite, Width),
			FinalX #= LeftBarrier - Width,
			Moved = mob(TypeId, FinalX, YPosition, 0, YSpeed, Facing)
		);
		(
			Facing = left, 
			move_left_until_collision(TargetBox, Collidables, RightBarrier),
			Moved = mob(TypeId, RightBarrier, YPosition, 0, YSpeed, Facing)
		);
		(Moved = MoveTarget)
	).

mob_move_y(Mob, _Tick, _Collidables, Mob):-
	Mob = mob(_TypeId, _XPosition, _YPosition, _XSpeed, none, _Facing).	

mob_move_y(Mob, Tick, Collidables, Moved):-
	Mob = mob(TypeId, XPosition, YPosition, XSpeed, YSpeed, Facing),
	TargetY #= YPosition + YSpeed,
	integer(YSpeed),

	% Glitch Here! We calculate collisions with the destination bounding box,
	% But if a collision happens, we may render a different sprite!
	TargetMove = mob(TypeId, XPosition, TargetY, XSpeed, YSpeed, Facing),
	sprite_box(Tick, TargetMove, TargetBox),

	(
		(
			move_down_until_collision(TargetBox, Collidables, BarrierBelow),
			YSpeed #> 0,
			FinalY #= BarrierBelow - 1,
			Moved = mob(TypeId, XPosition, FinalY, XSpeed, 0, Facing)
		);
		(
			move_up_until_collision(TargetBox, Collidables, BarrierAbove),
			YSpeed #< 0,
			sprite_sheet(TypeId, XSpeed, 0, Tick, Facing, Sprite),
			sprite_height(TypeId, Sprite, Height),
			FinalY #= BarrierAbove + Height + 1,
			Moved = mob(TypeId, XPosition, FinalY, XSpeed, 0, Facing)
		);
		(Moved = TargetMove)
	).

mob_move(Mob, Tick, Collidables, Moved):-
	mob_move_x(Mob, Tick, Collidables, XMoved),
	mob_move_y(XMoved, Tick, Collidables, Moved).

% Refactor now that you know how to spell maplist
gravity([], []).
gravity([H | L], [H | L1]):-
	H = mob(_TypeId, _XPosition, _YPosition, _XSpeed, none, _Facing),
	gravity(L, L1).
gravity([H | L], [H1 | L1]):-
	H = mob(TypeId, XPosition, YPosition, XSpeed, YSpeed, Facing),
	integer(YSpeed),
	NewSpeed #= YSpeed + 1,
	H1 = mob(TypeId, XPosition, YPosition, XSpeed, NewSpeed, Facing),
	gravity(L, L1).

after_physics(Mobs, Tick, [Moved|Others]):-
	gravity(Mobs, Accellerated),
	partition(mob_type(hero), Accellerated, [Mover], Others),
    % TODO: Support multiple movers in here!
	maplist(sprite_box(Tick), Others, Collidables),
	mob_move(Mover, Tick, Collidables, Moved).
