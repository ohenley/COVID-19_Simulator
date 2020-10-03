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

AppImage:
	wget -nv -c https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-x86_64.AppImage
	chmod +x linuxdeploy-x86_64.AppImage
	./linuxdeploy-x86_64.AppImage --executable bin/covidsim \
	--library qt5adax86-64/qt5adax86-64/usr/local/lib/libqt5c.so \
	--desktop-file distri/covidsim.desktop \
	--appdir AppDir --output appimage
