all: game.prg

game.prg: game.s titlescreen.s subroutines.s sound.s bin/music.bin bin/spritesheet_0.bin bin/sprite_ball_0.bin bin/font.bin scroller.txt
		vasm6502_oldstyle -Fbin -cbm-prg -opt-branch game.s -o game.prg
		exomizer sfx 2048 -x1 game.prg -o game.prg
		denise.exe game.prg

spritesheet_0.bin: python/spriteconv.py assets/spritesheet_0.tga
		python3 python/spriteconv.py assets/spritesheet_0.tga

sprite_ball_0.bin: python/spriteconv.py assets/sprite_ball_0.tga
		python3 python/spriteconv.py assets/sprite_ball_0.tga

font.bin: python/font_from_tga.py assets/font_0.tga
		python3 python/font_from_tga.py assets/font_0.tga

sprite_logo_0.bin: python/spriteconv.py assets/sprite_logo_0.tga
		python3 python/spriteconv.py assets/sprite_logo_0.tga

sound.prg: sound.s
		vasm6502_oldstyle -Fbin -cbm-prg -opt-branch sound.s -o sound.prg
		exomizer sfx 4096 -x1 sound.prg -o sound.prg
		denise.exe sound.prg