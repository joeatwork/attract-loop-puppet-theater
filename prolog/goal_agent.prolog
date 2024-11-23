:- module(goal_agent, [control_hero/4]).

:- use_module(library(clpfd)).

:- use_module(mobs).
:- use_module(physics).

% Walk/run speed is positive 5 or 0 (facing determines direction)
% Jump is a one-time positive 18, if Y speed is zero AND there is a platform


% Produces a next move for a hero from a start to some target

control_hero(box(TargetLeft, _TargetTop, _TargetWidth, _TargetHeight), Mobs, Tick, NewMobs):-
    partition(mob_type(hero), Mobs, [Hero], Remainder),
    % include(mob_speed(speed(none, none, _Facing), Remainder, Platforms),
    sprite_box(Tick, Hero, box(HeroLeft, _HeroTop, _HeroWidth, _HeroHeight)),
    mob_speed(speed(_OldXSpeed, YSpeed, OldFacing), Hero),
    (
        (TargetLeft #< HeroLeft, Facing = left, XSpeed = 5);
        (TargetLeft #> HeroLeft, Facing = right, XSpeed = 5);
        (Facing = OldFacing, XSpeed = 0)
    ),
    mob_with_speed(speed(XSpeed, YSpeed, Facing), Hero, NewHero),
    NewMobs = [NewHero | Remainder].