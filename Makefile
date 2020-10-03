QT_VERSION = 5.15

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
	cd linux/qt-$(QT_VERSION)x86-64/usr/local/Qt-$(QT_VERSION).0/lib ; \
    LD_LIBRARY_PATH=".:$(PWD)/linux/qt5adax86-64/qt5adax86-64/usr/local/lib" ./covidsim

clean:
	gprclean covidsim.gpr
	rm -rf linux
