all: | qt5adax86-64
	gprbuild -p covidsim.gpr

qt5adax86-64:
	wget -nv -c https://r3fowwcolhrzycn2yzlzzw-on.drv.tw/AdaStudio/qtada/qt5adax86-64.tar.bz2
	tar -xf qt5adax86-64.tar.bz2

run:
	LD_LIBRARY_PATH="qt5adax86-64/qt5adax86-64/usr/local/lib" bin/covidsim

clean:
	rm -f qt5adax86-64.tar.bz2
	rm -rf qt5adax86-64
	gprclean covidsim.gpr
