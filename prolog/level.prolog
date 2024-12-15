
:- use_module(library(clpfd)).

:- use_module(physics).
:- use_module(assets).

% A level is 
% A serpent of left-to-right runs and vertical up or down shafts

% A level section
% - Shares a border with a previous section
% - Final screen shares a border 
% - Fills up three "screens" worth of viewport space
% - Has an exit offset ??


% consumes 1 bit
choose_turn(Seed, turn):-
    0 #= Seed /\ 1.

choose_turn(Seed, straight):-
    1 #= Seed /\ 1.

% consumes 1 bit
choose_vertical_direction(Seed, up):-
    0 #= Seed /\ 1.

choose_vertical_direction(Seed, down):-
    1 #= Seed /\ 1.

% Bounds = box
% Level = level(Bounds, [Mobs], Target)
% Offset = offset(Top, Left)
% Previous Bounds? 

% viewport_dimensions(Width, Height)

% Offset is (Left, Bottom)
horizontal_section(offset(Left, Top), ViewportDimensions, Section):-
    ViewportDimensions = viewport_dimensions(VWidth, VHeight),
    RunWidth #= VWidth * 3,
    horizontal_floor(RunWidth, VHeight, OriginBricks, OriginTargetBox),
    maplist(mob_shift(Left, Top), OriginBricks, Bricks),
    move_box(Left, Top, OriginTargetBox, TargetBox),
    ExitOffsetLeft #= Left + RunWidth,
    Section = section(Bricks, TargetBox, offset(ExitOffsetLeft, Top)).

horizontal_floor(Width, Height, Bricks, Target):-
    hitbox_dimensions(brick, BrickWidth, BrickHeight),
    BrickBottom #= Height - (2 * BrickHeight),
    range_covered_with_intervals(0, Width, BrickWidth, BrickLefts),
    maplist(brick_at(BrickBottom), BrickLefts, Bricks),
    TargetTop #= BrickBottom - 32,
    Target = box(Width, TargetTop, 32, 32).

brick_at(Bottom, Left, mob(brick, Left, Bottom, none, none, neutral)).

% Range with stride, extends to OR PAST End from Start
range_covered_with_intervals(Start, End, _Width, []):-
    Start #>= End.

range_covered_with_intervals(Start, End, Width, [Start|Rest]):-
    Start #< End,
    Offset #= Start + Width,
    range_covered_with_intervals(Offset, End, Width, Rest).

:- begin_tests(level).

test(turn_down):-
    next_direction(8, down).

test(turn_up):-
    next_direction(9, up).

test(turn_left):-
    next_direction(10, up).

test(turn_left):-
    next_direction(11, same).

test(range_covering):-
    range_covered_with_intervals(0, 390, 32,
        [0, 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384]).

test(hfloor):-
    horizontal_floor(
        130, 256,
        [
            mob(brick, 0, 192, none, none, neutral),
            mob(brick, 32, 192, none, none, neutral),
            mob(brick, 64, 192, none, none, neutral),
            mob(brick, 96, 192, none, none, neutral),
            mob(brick, 128, 192, none, none, neutral)
        ], 
        box(130, 192, 32, 32)).

test(horiz):-
    horizontal_section(
        offset(100, 200),
        viewport_dimensions(60, 256),
        section(
            [
                mob(brick, 100, 392, none, none, neutral),
                mob(brick, 132, 392, none, none, neutral),
                mob(brick, 164, 392, none, none, neutral),
                mob(brick, 196, 392, none, none, neutral),
                mob(brick, 228, 392, none, none, neutral),
                mob(brick, 260, 392, none, none, neutral)
            ],
            box(280, 360, 32, 32),
            offset(280, 200)
        ) 
    ).

:- end_tests(level).


