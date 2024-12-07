#!/usr/bin/env swipl

:- use_module('../prolog/game').

:- initialization(main, main).

% Most basic test - hero has to run to the edge of a platform and then angle back in
main(_Argv):-
    % Need a better way to describe the initial state of a level
    game(
        [
            mob(hero, 320, 311, 0, 0, right),
        
            mob(brick, 288, 344, none, none, neutral),
            mob(brick, 320, 344, none, none, neutral),
            mob(brick, 352, 344, none, none, neutral),
            mob(brick, 384, 344, none, none, neutral),
            mob(brick, 416, 344, none, none, neutral),
            mob(brick, 448, 344, none, none, neutral)
        ],
        0,
        none, % intiial agent state
        level_dimensions(1280, 1280),
        viewport_dimensions(1280, 720)).