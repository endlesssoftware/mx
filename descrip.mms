#
#  MMK Makefile for building MX out of the CMS repository.
#
.IFNDEF MX_TOP
MX_TOP = MX
.ENDIF

.FIRST
	@ if f$type(dbg).nes."" then delete/symbol/global dbg
        @ if f$type(pkzip) .nes. "" then zip = pkzip

ALL 	 : $(SRCDIR)MMSDEFS.MMS,$(SRCDIR)MX_FLQ_SHR.OPT,$(SRCDIR)MX_SHR.OPT,$(SRCDIR)VERSION.OPT,-
    	   $(SRCDIR)00README.TXT
	$(MMS)$(MMSQUALIFIERS)/WORK=MG_SRC:[$(MX_TOP).COMMON] FLQ_SHARED_FILES
	$(MMS)$(MMSQUALIFIERS)/WORK=MG_SRC:[$(MX_TOP).OPENSSL]
	$(MMS)$(MMSQUALIFIERS)/WORK=MG_SRC:[$(MX_TOP).FLQ]
	$(MMS)$(MMSQUALIFIERS)/WORK=MG_SRC:[$(MX_TOP).COMMON]
	$(MMS)$(MMSQUALIFIERS)/WORK=MG_SRC:[$(MX_TOP).MAILSHR]
	$(MMS)$(MMSQUALIFIERS)/WORK=MG_SRC:[$(MX_TOP).LOCAL]
	$(MMS)$(MMSQUALIFIERS)/WORK=MG_SRC:[$(MX_TOP).SITE]
	$(MMS)$(MMSQUALIFIERS)/WORK=MG_SRC:[$(MX_TOP).SMTP]
	$(MMS)$(MMSQUALIFIERS)/WORK=MG_SRC:[$(MX_TOP).MLFAKE]
	$(MMS)$(MMSQUALIFIERS)/WORK=MG_SRC:[$(MX_TOP).ALIAS]
	$(MMS)$(MMSQUALIFIERS)/WORK=MG_SRC:[$(MX_TOP).MCP]
	$(MMS)$(MMSQUALIFIERS)/WORK=MG_SRC:[$(MX_TOP).MLF]
	$(MMS)$(MMSQUALIFIERS)/WORK=MG_SRC:[$(MX_TOP).ROUTER]
    	$(MMS)$(MMSQUALIFIERS)/WORK=MG_SRC:[$(MX_TOP).DOC]
    	$(MMS)$(MMSQUALIFIERS)/WORK=MG_SRC:[$(MX_TOP).EXAMPLES]
    	$(MMS)$(MMSQUALIFIERS)/WORK=MG_SRC:[$(MX_TOP).KIT]

CLEAN :
        - DELETE/NOLOG MG_KIT:[$(MX_TOP)...]*.*;*/EXCLUDE=*.dir
    	- DELETE/NOLOG MG_SRC:[$(MX_TOP)...]*.L32E;*
    	- DELETE/NOLOG MG_SRC:[$(MX_TOP)...]*.L32I;*
    	- DELETE/NOLOG MG_SRC:[$(MX_TOP)...]*.L32;*
    	- DELETE/NOLOG MG_SRC:[$(MX_TOP).DOC]*.TEX;*,.INT_*;*
	- delete/nolog MG_BIN:[$(MX_TOP)...]*.obj;*,*.olb;*,*.exe;*
    	- DELETE/NOLOG MG_ETC:[$(MX_TOP)...]*.LIS;*,.MAP;*,.SDML;*

REBUILD  :	CLEAN,ALL
	@ write sys$output "MX rebuild from scratch complete"

KIT 	 :
    	$(MMS)$(MMSQUALIFIERS)/WORK=MG_SRC:[$(MX_TOP).KIT]/CMS_LIBRARY=MG_CMS:[MX.KIT] ZIPFILE

SOURCE	:
    	@ SET DEFAULT MG_SRC:[$(MX_TOP)]
    	@ IF F$SEARCH("MG_KIT:[$(MX_TOP)]$(MX_TOP)_SRC.ZIP") .NES. "" THEN DELETE/NOLOG MG_KIT:[$(MX_TOP)]$(MX_TOP)_SRC.ZIP;*
    	zip/vms/recursive MG_KIT:[$(MX_TOP)]$(MX_TOP)_SRC.ZIP *.*/exclude=(*.l32*,*.*obj,*.*olb,*.*exe)

#  Copyright (c) 2008, Matthew Madison.
#  
#  All rights reserved.
#  
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
#  
#      * Redistributions of source code must retain the above
#        copyright notice, this list of conditions and the following
#        disclaimer.
#      * Redistributions in binary form must reproduce the above
#        copyright notice, this list of conditions and the following
#        disclaimer in the documentation and/or other materials provided
#        with the distribution.
#      * Neither the name of the copyright owner nor the names of any
#        other contributors may be used to endorse or promote products
#        derived from this software without specific prior written
#        permission.
#  
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
