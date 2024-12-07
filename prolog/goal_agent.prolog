:- module(goal_agent, [control_hero/5]).
:- use_module(library(clpfd)).
:- use_module(library(rbtrees)).
:- use_module(library(pairs)).
:- use_module(library(heaps)).

:- use_module(mobs).
:- use_module(physics).

% Walk/run speed is positive 5 or 0 (facing determines direction)
% Jump is a one-time positive 18, if Y speed is zero AND there is a platform

strategy_parts(Source, Move, MovedHero, strategy(Source, Move, MovedHero)).

% We can jump our Y speed is zero, and if we'd collide if we moved down.
standing(Mob, OtherBoxen):-
    mob_speed(speed(_XSpeed, 0, _Facing), Mob),
    mob_box(Mob, Bounds),
    move_box(0, 1, Bounds, Sink),
    collisions(Sink, OtherBoxen, [_Footing|_Rest]).

control_hero(TargetBox, Mobs, agent_state(_Other, []), UpdatedState, UpdatedMobs):-
    write("#> Changing strategy, ran out of plan"), nl,
    control_hero(TargetBox, Mobs, none, UpdatedState, UpdatedMobs).

control_hero(NewTarget, Mobs, agent_state(OldTarget, _Plan), UpdatedState, UpdatedMobs):-
    dif(NewTarget, OldTarget),
    write("#> Changing strategy, target was "), write(OldTarget), write(" now "), write(NewTarget), nl,
    control_hero(NewTarget, Mobs, none, UpdatedState, UpdatedMobs).

control_hero(TargetBox, Mobs, none, UpdatedState, UpdatedMobs):-
    write("#> Resetting state"), nl,
    include(mob_type(hero), Mobs, [Hero]),
    include(mob_speed(speed(none, none, _Facing)), Mobs, Platforms),
    moves_to_target(TargetBox, [Hero | Platforms], [PlanH| PlanL]),
    control_hero(TargetBox, Mobs, agent_state(TargetBox, [PlanH|PlanL]), UpdatedState, UpdatedMobs).

control_hero(TargetBox, Mobs, agent_state(TargetBox, [PlanH|PlanL]), agent_state(TargetBox, PlanL), [NewHero|Remainder]):-
    partition(mob_type(hero), Mobs, [Hero], Remainder),
    PlanH = strategy(_Source, NextMove, _Moved),
    mob_with_speed(NextMove, Hero, NewHero).

% Quality varies with distance to TargetBox
evaluate_move(TargetBox, Hero, Platforms, Move, MovedHero, Quality):-
    mob_with_speed(Move, Hero, NextHero),
    after_physics([NextHero|Platforms], Moved),
    include(mob_type(hero), Moved, [MovedHero]),
    mob_box(MovedHero, DestBox),
    box_distance_squared(TargetBox, DestBox, RealQuality),
    % Tiny tie-breaker for facing
    mob_speed(speed(_XSpeed, _YSpeed, Facing), MovedHero),
    ( 
        Facing = left -> Quality #= RealQuality + 1
        ;
        Quality = RealQuality
    ).


moves_from_here(TargetBox, Hero, Platforms, Results):-
    mob_speed(speed(_XSpeed, YSpeed, _Facing), Hero),
    maplist(mob_box(), Platforms, PlatformBoxen),
    ( 
        % When standing, you can run or leap
        standing(Hero, PlatformBoxen)
        ->  Moves = [ speed(5, 0, right), speed(5, 0, left), speed(5, -18, right), speed(5, -18, left)]
        ;
        % When leaping / falling you can steer X but not Y
        Moves = [ speed(5, YSpeed, right), speed(5, YSpeed, left), speed(0, YSpeed, right) ]
    ),
    maplist(evaluate_move(TargetBox, Hero, Platforms), Moves, MovedHeroes, Qualities),
    maplist(strategy_parts(Hero), Moves, MovedHeroes, Strategies),
    pairs_keys_values(Results, Qualities, Strategies).

% PathTree is a map from DEST to strategy(_, _, DEST), so
% we can climb from any point to the root of a path.

% This can fail if NewTree is already in
% the tree and points to a different OldTree.
path_tree_add(OldTree, Strategy, NewTree):-
    Strategy = strategy(SourceHero, _NextMove, DestHero),
    mob_box(SourceHero, SourceKey),
    mob_box(DestHero, DestKey),
    (
        rb_lookup(SourceKey, _Preceeding, OldTree) ->
            Initialized = OldTree
        ;
        rb_insert_new(OldTree, SourceKey, root, Initialized)
    ),
    rb_insert_new(Initialized, DestKey, Strategy, NewTree).

path_to_root(_PathTree, root, []).

path_to_root(PathTree, Tail, [Tail | Rest]):-
    Tail = strategy(Source, _NextMove, _Dest),
    mob_box(Source, SourceKey),
    rb_lookup(SourceKey, Next, PathTree),
    path_to_root(PathTree, Next, Rest).

moves_to_target(TargetBox, Mobs, Moves):-
    empty_heap(Fringe), 
    rb_empty(PathTree),
    moves_to_target(TargetBox, Mobs, PathTree, Fringe, Moves).

next_unseen_from_fringe(OldFringe, OldPaths, NewFringe, NewPaths, NewPriority, NewStrategy):-
    get_from_heap(OldFringe, NextPriority, NextStrategy, NextFringe),
    (
        path_tree_add(OldPaths, NextStrategy, NewPaths) ->
            NewStrategy = NextStrategy,
            NewPriority = NextPriority,
            NewFringe = NextFringe
        ;
        next_unseen_from_fringe(NextFringe, OldPaths, NewFringe, NewPaths, NewPriority, NewStrategy) 
    ).

% Prevent cycles by comitting to a single direction at any standing level.
moves_to_target(TargetBox, Mobs, Paths, Fringe, FromStart):-
    partition(mob_type(hero), Mobs, [Hero], Platforms),
    moves_from_here(TargetBox, Hero, Platforms, NewMoves),
    list_to_heap(NewMoves, NewHeap),
    merge_heaps(Fringe, NewHeap, AllFringe),
    next_unseen_from_fringe(AllFringe, Paths, NextFringe, NextPaths, Priority, NextMove),
    % What happens when we exhaust the fringe?
    (
        Priority #= 0 ->  
        path_to_root(NextPaths, NextMove, Path),
        reverse(Path, FromStart),
        write("# > Winning PATH: "), write(FromStart), nl
        ;
        NextMove = strategy(_SourceHero, _Move, MovedHero),
        moves_to_target(TargetBox, [MovedHero|Platforms], NextPaths, NextFringe, FromStart)
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