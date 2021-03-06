#!/usr/bin/env python
#-----------------------------------------------------------------------------
# Title      : PyRogue SsiPrbsRateGen
#-----------------------------------------------------------------------------
# File       : SsiPrbsRateGen.py
# Created    : 2017-04-12
#-----------------------------------------------------------------------------
# Description:
# PyRogue SsiPrbsTx
#-----------------------------------------------------------------------------
# This file is part of the rogue software platform. It is subject to
# the license terms in the LICENSE.txt file found in the top-level directory
# of this distribution and at:
#    https://confluence.slac.stanford.edu/display/ppareg/LICENSE.html.
# No part of the rogue software platform, including this file, may be
# copied, modified, propagated, or distributed except according to the terms
# contained in the LICENSE.txt file.
#-----------------------------------------------------------------------------

import pyrogue as pr
import rogue

class SsiPrbsRateGen(pr.Device):
    def __init__(   self,       
            name        = "SsiPrbsRateGen",
            description = "SsiPrbsRateGen",
            **kwargs):
        super().__init__(name=name, description=description, **kwargs) 

        ##############################
        # Variables
        ##############################

        self.add (pr.RemoteCommand(  
            name         = "statReset",
            description  = "",
            offset       =  0x00,
            bitSize      =  1,
            bitOffset    =  0x00,
            mode         = "RW",
            function     = pr.Command.toggle
        ))

        self.add(pr.RemoteVariable(    
            name         = "packetLength",
            description  = "",
            offset       =  0x04,
            bitSize      =  32,
            bitOffset    =  0x00,
            base         = pr.UInt,
            mode         = "RW",
        ))

        self.add(pr.RemoteVariable(    
            name         = "period",
            description  = "",
            offset       =  0x08,
            bitSize      =  32,
            bitOffset    =  0x00,
            base         = pr.UInt,
            mode         = "RW",
        ))

        self.add(pr.RemoteVariable(    
            name         = "txEn",
            description  = "",
            offset       =  0x0C,
            bitSize      =  1,
            bitOffset    =  0x00,
            base         = "bool",
            mode         = "RW",
        ))

        self.add(pr.RemoteCommand (   
            name         = "oneShot",
            description  = "",
            offset       =  0x0C,
            bitSize      =  1,
            bitOffset    =  0x01,
            base         = pr.UInt,
            mode         = "WO",
            function     = pr.Command.toggle
        ))

        self.add(pr.RemoteVariable(    
            name         = "missed",
            description  = "",
            offset       =  0x10,
            bitSize      =  32,
            bitOffset    =  0x00,
            base         = "int",
            pollInterval = 1,
            mode         = "RO",
        ))

        self.add(pr.RemoteVariable(    
            name         = "frameRate",
            description  = "",
            offset       =  0x14,
            bitSize      =  32,
            bitOffset    =  0x00,
            base         = "int",
            pollInterval = 1,
            mode         = "RO",
        ))

        self.add(pr.RemoteVariable(    
            name         = "frameRateMax",
            description  = "",
            offset       =  0x18,
            bitSize      =  32,
            bitOffset    =  0x00,
            base         = "int",
            pollInterval = 1,
            mode         = "RO",
        ))

        self.add(pr.RemoteVariable(    
            name         = "frameRateMin",
            description  = "",
            offset       =  0x1C,
            bitSize      =  32,
            bitOffset    =  0x00,
            base         = "int",
            pollInterval = 1,
            mode         = "RO",
        ))

        self.add(pr.RemoteVariable(    
            name         = "bandWidth",
            description  = "",
            offset       =  0x20,
            bitSize      =  64,
            bitOffset    =  0x00,
            base         = "int",
            pollInterval = 1,
            mode         = "RO",
        ))

        self.add(pr.RemoteVariable(    
            name         = "bandWidthMax",
            description  = "",
            offset       =  0x28,
            bitSize      =  64,
            bitOffset    =  0x00,
            base         = "int",
            pollInterval = 1,
            mode         = "RO",
        ))

        self.add(pr.RemoteVariable(    
            name         = "bandWidthMin",
            description  = "",
            offset       =  0x30,
            bitSize      =  64,
            bitOffset    =  0x00,
            base         = "int",
            pollInterval = 1,
            mode         = "RO",
        ))

        self.add(pr.RemoteVariable(    
            name         = "frameCount",
            description  = "",
            offset       =  0x40,
            bitSize      =  64,
            bitOffset    =  0x00,
            base         = "int",
            pollInterval = 1,
            mode         = "RO",
        ))

