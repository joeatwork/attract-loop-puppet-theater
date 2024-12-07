#!/usr/bin/env swipl

:- use_module('../prolog/game').

:- initialization(main, main).

% Jump test
main(_Argv):-
    % Need a better way to describe the initial state of a level
    run_game(
        [
            mob(hero, 32, 256, 0, 0, right),    

            mob(brick, 32, 352, none, none, neutral),
            mob(brick, 64, 352, none, none, neutral),
            mob(brick, 96, 352, none, none, neutral),
            mob(brick, 128, 352, none, none, neutral),
            mob(brick, 160, 352, none, none, neutral),
            mob(brick, 192, 352, none, none, neutral),
            
            % Drop

            mob(brick, 288, 512, none, none, neutral),
            mob(brick, 320, 512, none, none, neutral),
            mob(brick, 352, 512, none, none, neutral),
            
            % Small gap (384 - 448 / 64 units)
            
            mob(brick, 448, 512, none, none, neutral),
            mob(brick, 480, 512, none, none, neutral),
            mob(brick, 512, 512, none, none, neutral),

            % large gap ( 128 units)

            mob(brick, 672, 512, none, none, neutral),
            mob(brick, 704, 512, none, none, neutral),
            mob(brick, 736, 512, none, none, neutral),

            % Jump upward

            mob(brick, 800, 448, none, none, neutral),
            mob(brick, 832, 448, none, none, neutral),
            mob(brick, 864, 448, none, none, neutral)
        ],
        box(870, 416, 32, 32),
        level_dimensions(1280, 720),
        viewport_dimensions(1280, 720)).
