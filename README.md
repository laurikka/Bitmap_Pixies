# Bitmap Pixies

A game for Commodore 64

Play advice: move around with joystick in either port and collect all pixies, no fire button needed in this game. Collecting the colors in correct order is rewarded both in time and points so pay attention to the sequence.
The game is designed to be played on tac-2 or similar quick action joystick. A low latency setup like a real hardware or mister on crt will make obtaining high scores much easier.

I'm working towards version 1.0 and I think I'm pretty close to that so compiling the latest sources should be in good playable state. Version 0.9 is perfectly playable though and doesn't seem to have any major bugs.
I am now happy user of VSCodium running in Linux but the initial development was done in VS Code on Windows machine

Compiling the game:
- Coded for Vasm assembler: http://sun.hasenbraten.de/vasm/index.php?view=relsrc
Vasm needs to be compiled with CPU=6502 and SYNTAX=oldstyle.
- The executable is compressed with Exomizer: https://bitbucket.org/magli143/exomizer/src/master/

Only Vasm is strictly necessary for getting a runnable c64-program. Exomizer adds autorun but that can also be skipped by zeroing the COMPRESS-directive at the beginning of game.s to include basic autorun that doesn't rely on exomizer. Edit the makefile if you don't want to autorun exomizer or run the program after compiling.

- I can recommend Denise C64 emulator: https://bitbucket.org/piciji/denise/src/master/
- Retro Debugger is great for checking what's going on under the hood https://github.com/slajerek/RetroDebugger
