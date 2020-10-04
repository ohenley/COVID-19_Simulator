QT_VERSION = 5.15

QT_LIB_DIR = linux/qt-$(QT_VERSION)x86-64/usr/local/Qt-$(QT_VERSION).0/lib

all: | linux/qt5adax86-64 linux/qt-$(QT_VERSION)x86-64
	gprbuild -p covidsim.gpr

linux/qt5adax86-64:
	mkdir -p linux
	wget -nv -c https://github.com/mgrojo/qt5ada/releases/download/v$(QT_VERSION)/qt5adax86-64.tar.bz2
	tar -C linux -xf qt5adax86-64.tar.bz2

linux/qt-$(QT_VERSION)x86-64:
	mkdir -p linux
	wget -nv -c https://github.com/mgrojo/qt5ada/releases/download/v$(QT_VERSION)/qt$(QT_VERSION)x86-64.tar.bz2
	tar -C linux -xf qt$(QT_VERSION)x86-64.tar.bz2

run:
	cd $(QT_LIB_DIR) ; \
    LD_LIBRARY_PATH=".:$(PWD)/linux/qt5adax86-64/qt5adax86-64/usr/local/lib" ./covidsim

clean:
	gprclean covidsim.gpr
	rm -rf linux

AppImage:
	mkdir -p AppDir/src/form AppDir/deps/xph_covid19/data
	rsync -a distri/* AppDir
	rsync -a linux AppDir
	rsync -a src/form/*.ui AppDir/src/form
	rsync -a deps/xph_covid19/data/*.csv AppDir/deps/xph_covid19/data 
	wget -nv -c https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-x86_64.AppImage
	chmod +x linuxdeploy-x86_64.AppImage
	LD_LIBRARY_PATH="$(QT_LIB_DIR):linux/qt5adax86-64/qt5adax86-64/usr/local/lib" \
      ./linuxdeploy-x86_64.AppImage \
      --executable AppDir/$(QT_LIB_DIR)/covidsim \
      --desktop-file distri/covidsim.desktop --icon-file=distri/covidsim.png \
      --library linux/qt5adax86-64/qt5adax86-64/usr/local/lib/libqt5c.so \
      --appdir AppDir --output appimage
