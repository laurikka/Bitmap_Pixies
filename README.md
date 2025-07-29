# Bitmap Pixies

A game for Commodore 64

Play advice: move around with joystick in port 2 and collect all pixies, no fire button needed in this game. Collecting the colors in correct order is rewarded both in time and points so pay attention to the sequence.
The game is designed to be played on tac-2 or similar quick action joystick. A low latency setup like a real hardware or mister on crt will make obtaining high scores much easier.

Compiling the game: My setup is based on VS Code with WSL terminal at the bottom. I used Vasm assembler: http://sun.hasenbraten.de/vasm/index.php?view=relsrc.
Vasm needs to be compiled with CPU=6502 and SYNTAX=oldstyle.
The executable is compressed with Exomizer: https://bitbucket.org/magli143/exomizer/src/master/.
I used Denise C64 emulator, mostly because it seems to start a bit faster than Vice: https://bitbucket.org/piciji/denise/src/master/.
