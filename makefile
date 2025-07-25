all: game.prg

game.prg: game.s titlescreen.s subroutines.s sound.s bin/spritesheet_0.bin bin/font.bin bin/sprite_logo_0.bin scroller.txt
		vasm6502_oldstyle -Fbin -cbm-prg -opt-branch game.s -o game.prg
		exomizer sfx 2048 -x1 game.prg -o game.prg
		denise.exe game.prg

bin/spritesheet_0.bin: python/spriteconv.py assets/spritesheet_0.tga
		python3 python/spriteconv.py assets/spritesheet_0.tga bin/spritesheet_0.bin

font.bin: python/font_from_tga.py assets/font_0.tga
		python3 python/font_from_tga.py assets/font_0.tga

sprite_logo_0.bin: python/spriteconv.py assets/sprite_logo_0.tga
		python3 python/spriteconv.py assets/sprite_logo_0.tga
