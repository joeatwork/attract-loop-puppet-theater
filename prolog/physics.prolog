:- module(physics, 
    [
        after_physics/2,
        box_top/2,
        box_bottom/2,
        box_left/2,
        box_right/2,
		box_distance/4,
        move_box/4,
		overlaps/2,
        collisions/3,
        move_down_until_collision/3,
		standing/2
    ]).

:- use_module(library(clpfd)).

:- use_module(assets).
:- use_module(mobs).

box_top(Top, box(_Left, Top, _Width, _Height)).
box_bottom(Bottom, box(_Left, Top, _Width, Height)):-
	Bottom #= Top + Height.
box_left(Left, box(Left, _Top, _Width, _Height)).
box_right(Right, box(Left, _Top, Width, _Height)):-
	Right #= Left + Width.

% An Intersection MAY be an invalid box (with negative sizes)
box_intersection(B1, B2, Intersection):-
	box_left(Left1, B1), box_top(Top1, B1), box_right(Right1, B1), box_bottom(Bot1, B1),
	box_left(Left2, B2), box_top(Top2, B2), box_right(Right2, B2), box_bottom(Bot2, B2),
	Right #= max(Left1, Left2),
	Left #= min(Right1, Right2),
	Bottom #= max(Top1, Top2),
	Top #= min(Bot1, Bot2),
	Width #= Right - Left,
	Height #= Bottom - Top,
	Intersection = box(Top, Left, Width, Height).

% This could also calculate the intersection box if we need it.
% Horiz and Vert will always be non-negative
box_distance(B1, B2, Horiz, Vert):-
	box_intersection(B1, B2, box(_Left, _Top, IWidth, IHeight)),
	Horiz #= max(0, IWidth),
	Vert #= max(0, IHeight).

box_distance_squared(B1, B2, DistanceSquared):-
	box_distance(B1, B2, Horiz, Vert),
	DistanceSquared #= (Horiz * Horiz) + (Vert * Vert).

closest_box(Source, Other1, Other2, Min):-
	box_distance_squared(Source, Other1, D1),
	box_distance_squared(Source, Other2, D2),
	min_pair(D1-Other1, D2-Other2, _DMin-Min).

move_box(MoveX, MoveY, box(Left, Top, Width, Height), box(NewLeft, NewTop, Width, Height)):-
    NewLeft #= Left + MoveX,
    NewTop #= Top + MoveY.

% We could express this in terms of box_distance, too...
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
	maplist(box_left(), [Lft|LftList], Collisions),
	box_right(Frontier, TargetBox),
	foldl(minimum_absolute_delta(Frontier), LftList, Lft, LeftBarrier).

move_left_until_collision(TargetBox, Collidables, RightBarrier):-
	collisions(TargetBox, Collidables, Collisions),
	maplist(box_right(), [Rt|RtList], Collisions),
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

mob_move_x(Mob, _Collidables, Mob):-
	Mob = mob(_TypeId, _XPosition, _YPosition, none, _YSpeed, _Facing).

mob_move_x(Mob, Collidables, Moved):-
	mob_move_x_toward_facing(Mob, MoveTarget),
	mob_box(MoveTarget, TargetBox),
	MoveTarget = mob(TypeId, _TargetX, YPosition, _XSpeed, YSpeed, Facing),
	(
		
		Facing = right ->
			(
				move_right_until_collision(TargetBox, Collidables, LeftBarrier) ->
					hitbox_dimensions(TypeId, Width, _Height),
					FinalX #= (LeftBarrier - Width) - 1,
					Moved = mob(TypeId, FinalX, YPosition, 0, YSpeed, Facing)
				;
				Moved = MoveTarget
			)
		;
		Facing = left -> 
		(
			move_left_until_collision(TargetBox, Collidables, RightBarrier) ->
				FinalX #= RightBarrier + 1,
				Moved = mob(TypeId, FinalX, YPosition, 0, YSpeed, Facing)
			;
			Moved = MoveTarget
		)
	).

mob_move_y(Mob, _Collidables, Mob):-
	Mob = mob(_TypeId, _XPosition, _YPosition, _XSpeed, none, _Facing).	

mob_move_y(Mob, Collidables, Moved):-
	Mob = mob(TypeId, XPosition, YPosition, XSpeed, YSpeed, Facing),
	TargetY #= YPosition + YSpeed,
	TargetMove = mob(TypeId, XPosition, TargetY, XSpeed, YSpeed, Facing),
	mob_box(TargetMove, TargetBox),
	(
		(YSpeed #> 0, move_down_until_collision(TargetBox, Collidables, BarrierBelow)) ->
				FinalY #= BarrierBelow - 1,
				Moved = mob(TypeId, XPosition, FinalY, XSpeed, 0, Facing)
		;
		(YSpeed #< 0, move_up_until_collision(TargetBox, Collidables, BarrierAbove)) -> 
			hitbox_dimensions(TypeId, _Width, Height),
			FinalY #= BarrierAbove + Height + 1,
			Moved = mob(TypeId, XPosition, FinalY, XSpeed, 0, Facing)
		;
		Moved = TargetMove
	).

mob_move(Mob, Collidables, Moved):-
	mob_move_x(Mob, Collidables, XMoved),
	mob_move_y(XMoved, Collidables, Moved).

standing(Mob, Others):-
	mob_speed(speed(_XSpeed, 0, _Facing), Mob),
	mob_box(Mob, Bounds),
	move_box(0, 1, Bounds, Sink),
	% Slightly wrong - we are assuming NO collisions before the move.
	collisions(Sink, Others, [_Footing|_Rest]).

gravity(Mob, Moved):-
	Mob = mob(TypeId, XPosition, YPosition, XSpeed, YSpeed, Facing),
	(
		YSpeed = none
		-> Moved = Mob
		;
		YSpeed #>= 45
		-> NewSpeed #= 45,
		Moved = mob(TypeId, XPosition, YPosition, XSpeed, NewSpeed, Facing)
		;
		NewSpeed #= YSpeed + 1,
		Moved = mob(TypeId, XPosition, YPosition, XSpeed, NewSpeed, Facing)
	).

after_physics(Mobs, [Moved|Others]):-
	maplist(gravity(), Mobs, Accellerated),
	partition(mob_type(hero), Accellerated, [Mover], Others),
    % TODO: Support multiple movers in here!
	maplist(mob_box(), Others, Collidables),
	mob_move(Mover, Collidables, Moved).

:- begin_tests(physics).

test(clear_left_path):-
	findall(Moved, 
		physics:mob_move_x(
			mob(hero, 315, 311, 5, 1, left), 
			[box(288, 312, 32, 32), box(320, 312, 32, 32)], Moved),[mob(hero, 310, 311, 5, 1, left)]).

test(clear_right_path):-
	findall(Moved, 
		physics:mob_move_x(
			mob(hero, 315, 311, 5, 1, right), 
			[box(288, 312, 32, 32), box(320, 312, 32, 32)], Moved),[mob(hero, 320, 311, 5, 1, right)]).

test(after_physics):-
	findall(MovedHero,
	physics:after_physics(
		[
			mob(hero, 315, 311, 5, 0, left),
			mob(brick, 288, 344, none, none, neutral),
			mob(brick, 320, 344, none, none, neutral)
		],
		[
			MovedHero,
			mob(brick, 288, 344, none, none, neutral),
			mob(brick, 320, 344, none, none, neutral)
		]
	), [mob(hero, 310, 311, 5, 0, left)]).

:- end_tests(physics).
