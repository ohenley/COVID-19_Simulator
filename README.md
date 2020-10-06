# COVID-19 Simulator
Multi engine/algorithms COVID-19 simulator. Ada, Qt code under the hood. :astonished: 🤯 :metal:

## Table of Contents
<details>
<summary>Click to expand</summary>

1. [Presentation](#Presentation)
2. [Status](#Status)
3. [Prerequisites](#Prerequisites)  
4. [Dependencies](#Dependencies)
5. [Building](#Building)
6. [Limitations](#Limitations)
7. [Usage](#Usage)
8. [Acknowledgments](#Acknowledgments)

</details>

## Presentation
<div align="center">

<a>
<img border="0" src="https://github.com/ohenley/covidsim/blob/master/xph_uk_run.gif" style="max-width:100%;">
</a>
  
</div>

> Ada driven. Qt5 driven.  
> **Lancet** engine based on computations by https://github.com/zertovitch/mathpaqs. Implementing https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(20)30260-9/fulltext.    
> **XPH Pharmaceutical** engine based on computations by https://github.com/ohenley/xph_covid19. Implementing https://github.com/ohenley/xph_covid19/blob/master/doc/2020.04.03.20052985v1.full.pdf


<!---![alt text](https://github.com/ohenley/readme-template/blob/master/thug_war.png)--->

## Status
- Usable, WIP.
- Works on MS-Windows-10
- Works on Ubuntu Linux 18.04 LTS (not with native packages but using the libraries provided by [upstream qt5ada](https://r3fowwcolhrzycn2yzlzzw-on.drv.tw/AdaStudio/qtada/qt5ada.html))

## Prerequisites
- [git](https://git-scm.com/download/win)
- [gnat-ce](https://www.adacore.com/download) (tested with 2019, 2020).

## Dependencies
#### git submodules
- [xph_covid19](https://github.com/ohenley/xph_covid19)
- mathpaqs : [fork](https://github.com/ohenley/mathpaqs) of https://github.com/zertovitch/mathpaqs
- qt5ada : [custom git repo](https://github.com/ohenley/qt5ada) of https://r3fowwcolhrzycn2yzlzzw-on.drv.tw/AdaStudio/qtada/qt5ada.html

## Building
#### MS-Windows-10
```
> git clone --recursive https://github.com/ohenley/covidsim.git
> cd covidsim
> gprbuild covidsim.gpr
```
#### Ubuntu Linux
```
$ sudo apt install gnat-7 gprbuild make
$ git clone --recursive https://github.com/ohenley/covidsim.git
$ cd covidsim
$ make
```

## Limitations
See issues board.

## Usage
#### MS-Windows-10
```
> cd deps/qt5ada/qt5/5.15/bin
> ./covidsim.exe
```
#### Linux
```
$ make run
```

## Acknowledgments
- Based on simulation work by https://github.com/zertovitch
- Based on simulation work by [XPH Pharmaceutical](https://xph.co.nz/), original Ada code by Anatoly Chernyshev
- Using a custom packaged version of qt5ada thick binding by [Leonid Dulman](https://r3fowwcolhrzycn2yzlzzw-on.drv.tw/AdaStudio/qtada/qt5ada.html)

A big thanks to everyone!
