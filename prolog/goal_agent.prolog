:- module(goal_agent, [control_hero/3]).

:- use_module(library(clpfd)).
:- use_module(library(rbtrees)).
:- use_module(library(pairs)).
:- use_module(library(heaps)).

:- use_module(mobs).
:- use_module(physics).

% Walk/run speed is positive 5 or 0 (facing determines direction)
% Jump is a one-time positive 18, if Y speed is zero AND there is a platform

% This is here because I don't know how to make (-)/2 work reliably
strategy_parts(Move, MovedHero, strategy(Move, MovedHero)).

% Produces a next move for a hero from a start to some target

% We can jump our Y speed is zero, and if we'd collide if we moved down.
standing(Mob, OtherBoxen):-
    mob_speed(speed(_XSpeed, 0, _Facing), Mob),
    mob_box(Mob, Bounds),
    move_box(0, 1, Bounds, Sink),
    collisions(Sink, OtherBoxen, [_Footing|_Rest]).


control_hero(TargetBox, Mobs, [NewHero | Remainder]):-
    partition(mob_type(hero), Mobs, [Hero], Remainder),
    include(mob_speed(speed(none, none, _Facing)), Remainder, Platforms),

    % TODO: Hero should STAND when arriving at the goal.
    moves_to_target(TargetBox, [Hero | Platforms], [strategy(NextMove, _Moved)| _Path]),
    mob_with_speed(NextMove, Hero, NewHero).


% Quality varies with distance to TargetBox
evaluate_move(TargetBox, Hero, Platforms, Move, MovedHero, Quality):-
    mob_with_speed(Move, Hero, NextHero),
    after_physics([NextHero|Platforms], Moved),
    include(mob_type(hero), Moved, [MovedHero]),
    mob_box(MovedHero, DestBox),
    box_distance_squared(TargetBox, DestBox, Quality).


% Moves = [Quality-Strategy | _]
moves_from_here(TargetBox, Hero, Platforms, Results):-
    mob_speed(speed(_XSpeed, YSpeed, _Facing), Hero),
    maplist(mob_box(), Platforms, PlatformBoxen),
    ( 
        % When standing, you can run or leap
        standing(Hero, PlatformBoxen)
        ->  Moves = [ speed(5, 0, right), speed(5, 0, left), speed(5, -18, right), speed(5, -18, left)]
        ;
        % When leaping / falling you can steer X but not Y
        Moves = [ speed(5, YSpeed, right), speed(5, YSpeed, left) ] 
    ),
    maplist(evaluate_move(TargetBox, Hero, Platforms), Moves, MovedHeroes, Qualities),
    maplist(strategy_parts(), Moves, MovedHeroes, Strategies),
    pairs_keys_values(Results, Qualities, Strategies).


moves_to_target(TargetBox, Mobs, Moves):-
    empty_heap(Fringe), 
    rb_empty(Marks),
    moves_to_target(TargetBox, Mobs, Marks, Fringe, Moves).


mark_key(Mob, Key):-
    mob_left(Left, Mob),
    mob_bottom(Bottom, Mob),
    mob_speed(speed(_XSpeed, YSpeed, _Facing), Mob),
    Key = seen(Left, Bottom, YSpeed).


next_unmarked_from_fringe(OldFringe, OldMarks, NewFringe, NewMarks, NewPriority, NewStrategy):-
    get_from_heap(OldFringe, NextPriority, NextStrategy, NextFringe),
    NextStrategy = strategy(_NextMove, MovedHero),
    mark_key(MovedHero, Mark),
    (
        rb_lookup(Mark, seen, OldMarks)
        -> next_unmarked_from_fringe(NextFringe, OldMarks, NewFringe, NewMarks, NewPriority, NewStrategy) 
        ;
        rb_insert_new(OldMarks, Mark, seen, NewMarks),
        NewStrategy = NextStrategy,
        NewPriority = NextPriority,
        NewFringe = NextFringe
    ).


% Prevent cycles by comitting to a single direction at any standing level.
moves_to_target(TargetBox, Mobs, Marks, Fringe, [NextMove| Rest]):-
    partition(mob_type(hero), Mobs, [Hero], Platforms),

    % TODO: Fring grows without bound. So BOUND it, by moving target into some close proximity
    moves_from_here(TargetBox, Hero, Platforms, NewMoves),
    list_to_heap(NewMoves, NewHeap),
    merge_heaps(Fringe, NewHeap, AllFringe),

    next_unmarked_from_fringe(AllFringe, Marks, NextFringe, NextMarks, Priority, NextMove),
    (
        Priority #= 0
        ->  Rest = []
        ;
        NextMove = strategy(_M, MovedHero),
        moves_to_target(TargetBox, [MovedHero|Platforms], NextMarks, NextFringe, Rest)
    ).


:- begin_tests(goal_agent).
% swipl -g run_tests -t halt prolog/goal_agent.prolog

test(standing_up) :-
    findall(true,
        goal_agent:standing(
                mob(hero, 315, 311, 5, 0, left), 
                [box(224, 312, 32, 32), box(320, 312, 32, 32)]
            ), [true]).

test(evaluate_move) :-
            goal_agent:evaluate_move(
                box(360,2000,32,32),
                mob(hero,315,311,5,0,left),
                [mob(brick,288,344,none,none,neutral),mob(brick,320,344,none,none,neutral)],
                speed(5,0,left),
                mob(hero,310,311,5,0,left),
                2852725).

test(move_around) :- 
    goal_agent:moves_to_target(
        box(360, 2000, 32, 32), 
        [
            mob(hero, 320, 311, 5, 0, right),
            mob(brick, 288, 344, none, none, neutral),
            mob(brick, 320, 344, none, none, neutral)
        ], failThisTest).

test(high_level) :-
    goal_agent:control_hero(
        box(360, 2000, 32, 32),
        [mob(hero, 321, 311, 0, 0, right), mob(brick, 288, 344, none, none, neutral), mob(brick, 320, 344, none, none, neutral)],
        fixMe).

:- end_tests(goal_agent).