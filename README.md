# CovidSim
Qt5 COVID-19 simulator. Ada code under the hood.     

Ada! :astonished: but why not my favorite language!? 🤯   
    
... :metal: because :metal: ...  

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
<img border="0" src="https://github.com/ohenley/covidsim/blob/master/covidsim.png" style="max-width:100%;">
</a>
  
</div>

> Ada driven.  
> Qt5 driven.  
> Based on computations by https://github.com/zertovitch/mathpaqs.  
> Implementing https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(20)30260-9/fulltext.

<!---![alt text](https://github.com/ohenley/readme-template/blob/master/thug_war.png)--->

## Status
- Works on MS-Windows-10 using gnat-ce 2019
- Not working on Linux (planned as Qt and Ada are both portable)

## Prerequisites
- MS-Windows platform
- [gnat-ce](https://www.adacore.com/download) 2019 and up.

## Dependencies
#### git submodules
- mathpaqs : [fork](https://github.com/ohenley/mathpaqs) of https://github.com/zertovitch/mathpaqs
- qt5ada : [custom git repo](https://github.com/ohenley/qt5ada) of https://r3fowwcolhrzycn2yzlzzw-on.drv.tw/AdaStudio/qtada/qt5ada.html

## Building
#### MS-Windows-10 (using powershell-7)
```
> git clone --recursive https://github.com/ohenley/covidsim.git
> cd covidsim
> gprbuild covidsim.gpr
```

## Limitations
None so far.

## Usage
#### MS-Windows-10 (using powershell-7)
```
> cd deps/qt5ada/qt5/5.15/bin
> ./covidsim.exe
```

## Acknowledgments
- Based on simulation work by https://github.com/zertovitch
- Using a custom package version of qt5ada thick binding by [Leonid Dulman](https://r3fowwcolhrzycn2yzlzzw-on.drv.tw/AdaStudio/qtada/qt5ada.html)

A big thanks to both!
