:- module(goal_agent, [control_hero/4]).

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
standing(Tick, Mob, OtherBoxen):-
    mob_speed(speed(_XSpeed, 0, _Facing), Mob),
    sprite_box(Tick, Mob, Bounds),
    move_box(0, 1, Bounds, Sink),
    collisions(Sink, OtherBoxen, [_Footing|_Rest]).

control_hero(TargetBox, Mobs, Tick, [NewHero | Remainder]):-
    partition(mob_type(hero), Mobs, [Hero], Remainder),
    include(mob_speed(speed(none, none, _Facing)), Remainder, Platforms),
    empty_heap(Fringe), 
    rb_empty(Marks),
    
    % This didn't exactly *fail*, but it did something spooky...
    moves_to_target(TargetBox, [Hero | Platforms], Tick, Marks, Fringe, [NextMove| _Path]),
    mob_with_speed(NextMove, Hero, NewHero).


% Quality varies with distance to TargetBox
evaluate_move(TargetBox, Tick, Hero, Platforms, Move, MovedHero, Quality):-
    mob_with_speed(Move, Hero, NextHero),
    after_physics([NextHero|Platforms], Tick, Moved),
    include(mob_type(hero), Moved, [MovedHero]),
    /*
    We always calculate the heuristic using the same
    tick / sprite so we're measuring apples to apples
    */
    sprite_box(1, MovedHero, DestBox),
    box_distance_squared(TargetBox, DestBox, Quality).


% Moves = [Quality-Strategy | _]
moves_from_here(TargetBox, Tick, Hero, Platforms, Results):-
    mob_speed(speed(_XSpeed, YSpeed, _Facing), Hero),
    maplist(sprite_box(Tick), Platforms, PlatformBoxen),
    ( 
      (
            % When standing, you can run or leap
            standing(Tick, Hero, PlatformBoxen),
            Moves = [ speed(5, 0, right), speed(5, 0, left), speed(5, -18, right), speed(5, -18, left)]
        );
        % When leaping / falling you can steer X but not Y
        Moves = [ speed(5, YSpeed, right), speed(5, YSpeed, left) ] 
    ),
    maplist(evaluate_move(TargetBox, Tick, Hero, Platforms), Moves, MovedHeroes, Qualities),
    maplist(strategy_parts(), Moves, MovedHeroes, Strategies),
    pairs_keys_values(Results, Qualities, Strategies).
    

% Prevent cycles by comitting to a single direction at any standing level.
moves_to_target(TargetBox, Mobs, Tick, Marks, Fringe, [NextMove| Rest]):-
    partition(mob_type(hero), Mobs, [Hero], Platforms),

    % TODO: Fring grows without bound. So BOUND it, by moving target into some close proximity
    moves_from_here(TargetBox, Tick, Hero, Platforms, NewMoves),
    list_to_heap(NewMoves, NewHeap),
    merge_heaps(Fringe, NewHeap, AllFringe),
    get_from_heap(AllFringe, Priority, NextStrategy, NextFringe),

    (
        (
            Priority #= 0,
            NextMove = speed(0, 0, right)
        );
        (
            NextStrategy = strategy(NextMove, MovedHero),
            rb_insert_new(Marks, MovedHero, true, NextMarks),
            NextTick #= Tick + 1,
            moves_to_target(TargetBox, [MovedHero|Platforms], NextTick, NextMarks, NextFringe, Rest)
        )
    ).


:- begin_tests(goal_agent).
% swipl -g run_tests -t halt prolog/goal_agent.prolog

test(standing_up) :-
    standing(71,
        mob(hero, 315, 311, 5, 0, left), 
        [box(224, 312, 32, 32), box(320, 312, 32, 32)]).

test(moves) :- 
    empty_heap(Fringe), 
    rb_empty(Marks), 
    moves_to_target(
        box(360, 2000, 32, 32),
        [
            mob(hero, 315, 311, 5, 0, left),
            mob(brick, 96, 344, none, none, neutral),
            mob(brick, 128, 344, none, none, neutral),
            mob(brick, 160, 344, none, none, neutral),
            mob(brick, 192, 344, none, none, neutral),
            mob(brick, 224, 344, none, none, neutral), 
            mob(brick, 320, 344, none, none, neutral)
        ],
        71,
        Marks,
        Fringe,
        failThisTest).

test(walk_around) :-
    control_hero(
        box(360, 2000, 32, 32),
        [mob(hero, 315, 311, 5, 0, left),
        mob(brick, 96, 344, none, none, neutral),
        mob(brick, 128, 344, none, none, neutral),
        mob(brick, 160, 344, none, none, neutral),
        mob(brick, 192, 344, none, none, neutral),
        mob(brick, 224, 344, none, none, neutral),
        mob(brick, 256, 344, none, none, neutral),
        mob(brick, 288, 344, none, none, neutral),
        mob(brick, 320, 344, none, none, neutral)
    ], 71, failThisTest).

:- end_tests(goal_agent).