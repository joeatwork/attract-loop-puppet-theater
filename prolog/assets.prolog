:- module(assets, [sheet_geometry/9]).

sheet_geometry(hero, standLeft, 60, 72, "megaman/stand-left.png", 0, 0, 21, 24).

sheet_geometry(hero, standRight, 60, 72, "megaman/stand-right.png", 0, 0, 21, 24).

% TODO: run cycle filenames are out of order and confusing.

sheet_geometry(hero, runLeft1, 48, 72, "megaman/run-left-2.png", 0, 9, 16, 24).

sheet_geometry(hero, runLeft2, 72, 66, "megaman/run-left-1.png", 0, 0, 24, 22).

sheet_geometry(hero, runLeft3, 48, 72, "megaman/run-left-2.png", 0, 9, 16, 24).

sheet_geometry(hero, runLeft4, 63, 66, "megaman/run-left-3.png", 0, 0, 21, 24).

sheet_geometry(hero, runRight1, 48, 72, "megaman/run-right-2.png", 0, 0, 16, 24).

sheet_geometry(hero, runRight2, 72, 66, "megaman/run-right-1.png", 0, 0, 24, 22).

sheet_geometry(hero, runRight3, 48, 72, "megaman/run-right-2.png", 0, 0, 16, 24).

sheet_geometry(hero, runRight4, 63, 66, "megaman/run-right-3.png", 0, 0, 21, 24).

sheet_geometry(hero, jumpLeft, 78, 90, "megaman/jump-left.png", 0, 0, 26, 30).

sheet_geometry(hero, jumpRight, 78, 90, "megaman/jump-right.png", 0, 0, 26, 30).

sheet_geometry(brick, brick, 32, 32, "placeholders/brick-16x16.png", 0, 0, 16, 16).
