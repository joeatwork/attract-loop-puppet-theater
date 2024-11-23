:- module(goal_agent, [control_hero/4]).

:- use_module(library(clpfd)).

:- use_module(mobs).

% Produces a next move for a hero from a start to some target

control_hero(box(TargetLeft, _TargetTop, _TargetWidth, _TargetHeight), Mobs, Tick, NewMobs):-
    partition(mob_type(hero), Mobs, [Hero], Remainder),
    % include(mob_speed(speed(none, none, _Facing), Remainder, Platforms),
    sprite_box(Tick, Hero, box(HeroLeft, _HeroTop, _HeroWidth, _HeroHeight)),
    mob_speed(speed(_OldXSpeed, YSpeed, OldFacing), Hero),
    (
        (TargetLeft #< HeroLeft, Facing = left, XSpeed = 4);
        (TargetLeft #> HeroLeft, Facing = right, XSpeed = 4);
        (Facing = OldFacing, XSpeed = 0)
    ),
    mob_with_speed(speed(XSpeed, YSpeed, Facing), Hero, NewHero),
    NewMobs = [NewHero | Remainder].