#ifndef GLOBALS_HXX
#define GLOBALS_HXX

// Copyright 1995, Logos Corporation, Mt. Arlington, NJ 07856
//================================================================================
//
// This file contains global declarations/definitions.
//
// Author: Ashwin Kalbag
// Created: 11/20/94
//
// $Log: globals.hxx,v $
// Revision 1.1  2005/08/09 08:47:52  kiefer
//
//
// Initial check in.
//
//      
//         Rev 1.1   07/22/95 15:06:02   aboshkin
//      Request Number:	none
//      Description:
//      changed debugging output scheme
//      
//         Rev 1.0   05/02/95 09:30:58   ashwin
//      Initial revision.
//
//================================================================================

// ostream
//#include <iostream.h>

// docSetup
#include <ltxdoc/docsetup.hxx>

// tmBool
#include <logos_libs/tm/tmtypes.hxx>

extern const docSetup* env;

// debugging globals
extern ostream*   dout_ptr;

// use #define instead of inline in order not to spend time/memory evaluating expr if debug is off
#define debug(expr) if (dout_ptr != 0) { (*dout_ptr) << (expr) << endl; }

#endif
