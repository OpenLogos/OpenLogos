# OpenLogos

The open-source version of the commercial Logos machine translation system in production use for over thirty years, OpenLogos was ported to Linux and PostgreSQL by the researchers at DFKI.

## Disclaimer

The contents of this repo and `README` have been taken verbatim from the [OpenLogos SourceForge page](https://sourceforge.net/projects/openlogos-mt/) and the [SourceForge Wiki](https://sourceforge.net/p/openlogos-mt/wiki/Home/). The authors of the text are Michael Roberts and Walter Kasper.


## The LOGOS Machine Translation System

The LOGOS machine translation system is a commercial system that was developed over the course of more than 30 years. It is currently owned by [Group Business Software AG](http://www.group.de/en/index.php), who also offer commercial licenses.

In 2005, Group AG made the fantastic decision to open the source code. The descendant of that code, including over 1600 files for the engine and another 1850 or so for the linguistic resource development tools, is what you see here at SourceForge, and it is available under the terms of the GPL, along with the extremely extensive contents of the linguistic database. The Language Technology Lab at [DFKI (the German Research Institute for Artificial Intelligence)](https://www.dfki.de/web) did the hard work in producing this fully open-source-compatible distribution, which compiles on Linux against a PostgreSQL database.

German and English are the source languages currently available. The target languages for English include the major European languages (such as French, Italian, Spanish and Portuguese). Development of new source languages is hard. Target languages are merely challenging. Examples do not yet exist.

In addition to the machine translation engine itself, there are a set of three GUI tools written in Java that can be used to adapt the dictionaries, run the engine on actual text, and perform general administrative tasks.

This Wiki exists in order to provide some insight into the structure of the system for the developer. There are some resources available, and over time we will be providing them here.

## Licensing

OpenLogos has two licensing options. From a licensing perspective, we have two different products depending on usage and distribution, though technically they have the same source code.

First, the system is available under the terms of the GPL. Under these terms, you may use and redistribute the software at no cost provided that the complete source code for any application you distribute that includes any component of OpenLogos must itself be available and freely redistributable under reasonable conditions. Group Business Software AG, the owners of Logos, bases its interpretation of the GPL on the Free Software Foundation's Frequently Asked Questions.

If you don't want to, or cannot, comply with the terms of the open-source license, a commercial license is available from the owners of the LOGOS system, [Group Business Software AG](http://www.group.de/en/index.php), Hospitalstraße 6, D-99817 Eisenach, Germany. The commercial license allows you to provide commercial software licenses to your customers or distribute Logos MT based applications or to use OpenLogos for commercial purposes. This is for organizations that do not want to release the source code for their applications as open source / free software; in other words, they do not want to comply with the GNU General Public License (GPL).

Both licenses apply equally to the code and to the database contents provided as part of the OpenLogos distribution.

PDFs explaining details of the licensing terms are included in the source code distributions.

## Obtaining and installing OpenLogos

There are currently three options for obtaining and installing OpenLogos.

### 1. Download the source and compile it yourself

Source downloads are available at the download page. Compilation then proceeds with the normal Unix toolchain.

#### Ubuntu Linux:

My notes on installation on Ubuntu 10.10 are at `Installation_and_use/Ubuntu-10_10`.

The definitive article on Ubuntu installation and configuration is [Torsten Scheck's 2006 article](http://www.pro-linux.de/artikel/2/253/openlogos-101-installation-und-anwendung.html) in German and translated here on the Wiki at [Installation_and_Use/Ubuntu-5_x](https://sourceforge.net/p/openlogos-mt/wiki/Installation_and_Use/Ubuntu-5_x). There is also a list of updated [instructions for Ubuntu 9.x here](http://logos-os.dfki.de/Install_for_Ubuntu_9.X.html).

#### Slackware Linux up to 12.2:

(Apparently Slackware 13.0 has broken the build.) Very explicit notes at 1, with thanks to Martin Dowd.

### 2. Use a precompiled VirtualBox machine

VirtualBox is a Sun (now Oracle) product that lets you host virtual machines on the computer of your choice. I'm currently setting up an Ubuntu 10.10 install on my Windows 7 machine, for instance - but there is a completed VirtualBox installation already available [here](http://vikitraduko.saluton.dk/openlogos/). Check the README there for instructions on how to use it.

### 3. Use the precompiled Windows version

Norman Reid has contributed a version of OpenLogos compiled with MinGW under Windows. Download it from DFKI - Norman's source changes and Eclipse project are available in the [Windows source version](http://logos-os.dfki.de/release/OpenLogos_Windows_SRC.zip). Some of those changes have been rolled into the main source tree; eventually, a standard build with the MinGW toolchain will be part of the standard offering here at SourceForge, but ... not yet.

## Modifying OpenLogos

Modifying OpenLogos is not an easy task. The code is poorly understood, and vast. A major function of this Wiki will be to start documenting its structure, and you can start at OpenLogos code overview.
The open-source version of the commercial Logos machine translation system.

