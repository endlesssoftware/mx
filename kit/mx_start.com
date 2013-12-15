$ V = 'F$VERIFY(0)
$!
$! Copyright (c) 2008, Matthew Madison.
$! 
$! All rights reserved.
$! 
$! Redistribution and use in source and binary forms, with or without
$! modification, are permitted provided that the following conditions
$! are met:
$! 
$!     * Redistributions of source code must retain the above
$!       copyright notice, this list of conditions and the following
$!       disclaimer.
$!     * Redistributions in binary form must reproduce the above
$!       copyright notice, this list of conditions and the following
$!       disclaimer in the documentation and/or other materials provided
$!       with the distribution.
$!     * Neither the name of the copyright owner nor the names of any
$!       other contributors may be used to endorse or promote products
$!       derived from this software without specific prior written
$!       permission.
$! 
$! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
$! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
$! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
$! A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
$! OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
$! SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
$! LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
$! DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
$! THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
$! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
$! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
$!
$! MX Startup command file
$!
$ SET = "SET"
$ SET NOON
$ SET SYMBOL/SCOPE=(NOLOCAL,NOGLOBAL)
$ DSE := DEFINE/SYSTEM/EXEC/NOLOG
$ DS  := DEFINE/SYSTEM/NOLOG
$ SAY := WRITE SYS$OUTPUT
$!
$ NODE = F$GETSYI ("NODENAME")
$ IF NODE .NES. "" THEN NODE = "_" + NODE
$!
$ IF F$SEARCH ("MX_DIR:MX_START.LOG;-1") .NES. "" THEN -
    PURGE/KEEP=6 MX_DIR:MX_START.LOG
$!
$ XSTART = "RUN/AST_LIMIT=100/BUFFER=100000/ENQUE=300/MAXIMUM=8192"+-
    "/FILE_LIM=100/IO_BUF=100/IO_DIR=100/JOB_TABLE=0/EXTENT=32768"+-
    "/QUEUE=100/TIME_LIMIT=0/DETACH/PRIV=ALL/PRIO=4/UIC=[1,4]"
$!
$ IF "''P1'" .EQS. ""
$ THEN
$   PARAM = ",MAILSHR"
$ ELSE
$  I = 0
$  PARAM = ""
$PLOOP:
$  I = I + 1
$  P = "P''I'"
$  IF "''&P'" .NES. ""
$  THEN
$   PARAM = PARAM + ",''&P'"
$   GOTO PLOOP
$  ENDIF
$ ENDIF
$!
$ THINGS = "FLQ_MGR/ROUTER/LOCAL/MAILSHR/SMTP/SMTP_SERVER/UUCP/MLF/SITE/NETLIB/DNSMTP/HOLD/"
$ MULTI_THINGS = "ROUTER/LOCAL/SMTP/DNSMTP/SITE/"
$!
$ I = 0
$LOOP:
$ smtp_server_comfile = "MX_EXE:SMTP_SERVER.COM"
$ smtp_server_prcnam  = "MX SMTP Server"
$ I = I + 1
$ WHAT = F$ELEMENT (I, ",", PARAM)
$ IF "''WHAT'" .EQS. "," THEN EXIT 1+0*F$VERIFY(V)
$ IF F$EXTRACT (0,4,WHAT) .EQS. "HOLD" 
$ THEN
$   holdwhich = F$INTEGER (F$EXTRACT (4,-1,WHAT))
$   WHAT = "HOLD"
$   IF holdwhich .LT. 1 THEN EXIT 1+0*F$VERIFY(v)
$ ENDIF
$ ITERATION = ""
$ IT  = F$ELEMENT (1, "#", WHAT)
$ WHAT = F$ELEMENT (0, "#", WHAT)
$ IF IT .NES. "#" .AND. -
    	F$LOCATE (WHAT+"/",MULTI_THINGS) .LT. F$LENGTH (MULTI_THINGS)
$ THEN
$   NUM = F$INTEGER (IT)
$   IF NUM .GT. 0 .AND. NUM .LT. 200
$   THEN
$	ITERATION = "#''NUM'"
$	FILEID = NODE + "_''NUM'"
$   ELSE
$	FILEID = NODE
$   ENDIF
$ ELSE
$   tag = F$ELEMENT (1, "=", what)
$   IF tag .EQS. "="
$   THEN
$       fileid = node
$   ELSE
$       what = F$ELEMENT (0, "=", what)
$       IF what .EQS. "SMTP_SERVER" .AND. F$SEARCH("MX_EXE:SMTP_SERVER_''tag'.COM") .NES. ""
$       THEN
$           fileid = node + "_" + tag
$           smtp_server_comfile = "MX_EXE:SMTP_SERVER_" + tag + ".COM"
$           smtp_server_prcnam  = "SMTPSrv ''tag'"
$       ELSE
$           say "%MX_START-F-INVSPEC, invalid specifier: ''what'=''tag'"
$           EXIT 0+F$VERIFY(v)
$       ENDIF
$   ENDIF
$ ENDIF
$ IF F$LOCATE (WHAT+"/", THINGS) .GE. F$LENGTH (THINGS) THEN GOTO LOOP
$ tmp = F$TRNLNM ("MX_''WHAT'_PGFLQUO") 
$ IF tmp .EQS. ""
$ THEN START = XSTART + "/PAGE_FILE=65536"
$ ELSE START = XSTART + "/PAGE_FILE=''tmp'"
$ ENDIF
$ GOTO START_'WHAT'
$!
$START_MAILSHR:
$ !
$ !  Try to deinstall MAILQUEUE in case the old one has been deleted by
$ !  an installation on another node.
$ !
$ define/user sys$output nl:
$ define/user sys$error nl:
$ install remove mx_exe:mailqueue.exe
$ IF F$TRNLNM ("MX_FLQ_SHR") .NES. "" THEN -
    IF F$FILE_ATTR ("MX_FLQ_SHR:.EXE","KNOWN") THEN INSTALL REMOVE MX_FLQ_SHR
$ IF F$TRNLNM ("MX_MAILSHR") .NES. "" THEN -
    IF F$FILE_ATTR ("MX_MAILSHR:.EXE","KNOWN") THEN INSTALL REMOVE MX_MAILSHR
$ IF F$TRNLNM ("MX_MAILSHRP") .NES. "" THEN -
    IF F$FILE_ATTR ("MX_MAILSHRP:.EXE","KNOWN") THEN INSTALL REMOVE MX_MAILSHRP
$ IF F$TRNLNM ("MX_SHR") .NES. "" THEN -
    IF F$FILE_ATTR ("MX_SHR:.EXE","KNOWN") THEN INSTALL REMOVE MX_SHR
$ IF F$TRNLNM ("MX_MSG") .NES. "" THEN -
    IF F$FILE_ATTR ("MX_MSG:.EXE","KNOWN") THEN INSTALL REMOVE MX_MSG
$ IF F$TRNLNM ("MX_SMTP_MSG") .NES. "" THEN -
    IF F$FILE_ATTR ("MX_SMTP_MSG:.EXE","KNOWN") THEN INSTALL REMOVE MX_SMTP_MSG
$!
$ IF F$TRNLNM ("MX_SITE_ADDRESS_REWRITER") .EQS. ""
$ THEN
$   IF F$SEARCH ("MX_EXE:ADDRESS_REWRITER.EXE") .NES. "" THEN -
    	DSE MX_SITE_ADDRESS_REWRITER MX_EXE:ADDRESS_REWRITER
$ ENDIF
$ addrew = F$PARSE ("MX_SITE_ADDRESS_REWRITER:",".EXE") - ";"
$ IF addrew .NES. "" THEN -
    IF F$SEARCH (addrew) .NES. "" THEN -
    	IF F$FILE_ATTR (addrew, "KNOWN") THEN INSTALL REMOVE 'addrew'
$!
$ IF F$TRNLNM ("MX_SITE_DOM_EXPANSION") .EQS. ""
$ THEN
$   IF F$SEARCH ("MX_EXE:DOMAIN_EXPANSION.EXE") .NES. "" THEN -
    	DSE MX_SITE_DOM_EXPANSION MX_EXE:DOMAIN_EXPANSION
$ ENDIF
$ domexp = F$PARSE ("MX_SITE_DOM_EXPANSION:",".EXE") - ";"
$ IF domexp .NES. "" THEN -
    IF F$SEARCH (domexp) .NES. "" THEN -
    	IF F$FILE_ATTR (domexp, "KNOWN") THEN INSTALL REMOVE 'domexp'
$!
$ IF F$TRNLNM ("MX_SITE_NAME_CONVERSION") .EQS. ""
$ THEN
$   IF F$SEARCH ("MX_EXE:NAME_CONVERSION.EXE") .NES. "" THEN -
    	DSE MX_SITE_NAME_CONVERSION MX_EXE:NAME_CONVERSION
$ ENDIF
$ namecon = F$PARSE ("MX_SITE_NAME_CONVERSION:",".EXE") - ";"
$ IF namecon .NES. "" THEN -
    IF F$SEARCH (namecon) .NES. "" THEN -
    	IF F$FILE_ATTR (namecon, "KNOWN") THEN INSTALL REMOVE 'namecon'
$ DSE MX_FLQ_SHR    MX_EXE:MX_FLQ_SHR
$ DSE MX_SHR        MX_EXE:MX_SHR
$ DSE MX_MSG        MX_EXE:MX_MSG
$ DSE MX_SMTP_MSG   MX_EXE:MX_SMTP_MSG
$ DSE MX_MAILSHR    MX_EXE:MX_MAILSHR
$ DSE MX_MAILSHRP   MX_EXE:MX_MAILSHRP
$ icmd := create
$ IF F$FILE_ATTR ("MX_SHR:.EXE","KNOWN") THEN icmd := replace
$ INSTALL 'icmd' MX_SHR/OPEN/SHARE/HEADER
$ icmd := create
$ IF F$FILE_ATTR ("MX_MSG:.EXE","KNOWN") THEN icmd := replace
$ INSTALL 'icmd' MX_MSG/OPEN/SHARE/HEADER
$ icmd := create
$ IF F$FILE_ATTR ("MX_SMTP_MSG:.EXE","KNOWN") THEN icmd := replace
$ INSTALL 'icmd' MX_SMTP_MSG/OPEN/SHARE/HEADER
$ icmd := create
$ IF F$FILE_ATTR ("MX_MAILSHR:.EXE","KNOWN") THEN icmd := replace
$ INSTALL 'icmd' MX_MAILSHR/OPEN/SHARE/HEADER
$ icmd := create
$ IF F$FILE_ATTR ("MX_MAILSHRP:.EXE","KNOWN") THEN icmd := replace
$ INSTALL 'icmd' MX_MAILSHRP/OPEN/SHARE/HEADER/PROTECT
$ icmd := create
$ IF F$FILE_ATTR ("MX_FLQ_SHR:.EXE","KNOWN") THEN icmd := replace
$ INSTALL 'icmd' MX_FLQ_SHR/OPEN/SHARE/HEADER
$! --------------------[ Install Address_Rewriter ]----------------
$ IF addrew .NES. ""
$ THEN
    IF F$SEARCH (addrew) .NES. ""
$   THEN
$       icmd := create
$       IF F$FILE_ATTR (addrew,"KNOWN") THEN icmd := replace
$       INSTALL 'icmd' 'addrew'/OPEN/SHARE/HEADER
$!$       Write sys$output ("Install ''icmd' ''addrew'")
$   ENDIF
$ ENDIF
$! --------------------[ Install Domain_Expansion ]----------------
$ IF domexp .NES. ""
$ THEN
    IF F$SEARCH (domexp) .NES. ""
$   THEN
$       icmd := create
$       IF F$FILE_ATTR (domexp,"KNOWN") THEN icmd := replace
$       INSTALL 'icmd' 'domexp'/OPEN/SHARE/HEADER
$!$       Write sys$output ("Install ''icmd' ''domexp'")
$   ENDIF
$ ENDIF
$ IF namecon .NES. ""
$ THEN
    IF F$SEARCH (namecon) .NES. ""
$   THEN
$   	icmd := create
$   	IF F$FILE_ATTR (namecon,"KNOWN") THEN icmd := replace
$   	INSTALL 'icmd' 'namecon'/OPEN/SHARE/HEADER
$   ENDIF
$ ENDIF
$ icmd := create
$ IF F$FILE_ATTR ("MX_EXE:MAILQUEUE.EXE","KNOWN") THEN icmd := replace
$ INSTALL 'icmd' MX_EXE:MAILQUEUE/PRIV=(SYSPRV,SYSLCK)
$ GOTO LOOP
$!
$START_NETLIB:
$ file = f$search("SYS$STARTUP:NETLIB_STARTUP.COM")
$ if file.nes."" then @'file'
$ GOTO LOOP
$!
$START_FLQ_MGR:
$ needprv = "SYSLCK,SYSPRV,EXQUOTA"
$ oldprv = F$SETPRV (needprv)
$ IF .NOT. F$PRIVILEGE (needprv)
$ THEN
$   SAY "%MX-F-INSFPRV, need ''needprv' to start ''WHAT' process"
$   oldprv = F$SETPRV (oldprv)
$   GOTO LOOP
$ ENDIF
$ icmd := create
$ IF F$FILE_ATTR ("MX_EXE:MX_FLQ_MGR.EXE","KNOWN") THEN icmd := replace
$ INSTALL 'icmd' MX_EXE:MX_FLQ_MGR.EXE/OPEN/SHARE/HEADER
$ DSE MX_ROUTER_DIR MX_ROOT:[ROUTER]
$ IF F$SEARCH ("MX_ROUTER_DIR:MX_FLQ_MGR''FILEID'.LOG;-1") .NES. "" THEN -
	PURGE/KEEP=5 MX_ROUTER_DIR:MX_FLQ_MGR'FILEID'.LOG
$ START/INPUT=MX_EXE:MX_FLQ_MGR.COM/OUTPUT=MX_ROUTER_DIR:MX_FLQ_MGR'FILEID'.LOG-
    /PROC="MX FLQ Manager" SYS$SYSTEM:LOGINOUT.EXE
$ oldprv = F$SETPRV (oldprv)
$ GOTO LOOP
$!
$START_ROUTER:
$ needprv = "SYSLCK,SYSPRV,EXQUOTA"
$ oldprv = F$SETPRV (needprv)
$ IF .NOT. F$PRIVILEGE (needprv)
$ THEN
$   SAY "%MX-F-INSFPRV, need ''needprv' to start ''WHAT' process"
$   oldprv = F$SETPRV (oldprv)
$   GOTO LOOP
$ ENDIF
$ icmd := create
$ IF F$FILE_ATTR ("MX_EXE:MX_ROUTER.EXE","KNOWN") THEN icmd := replace
$ INSTALL 'icmd' MX_EXE:MX_ROUTER.EXE/OPEN/SHARE/HEADER
$ DSE MX_ROUTER_DIR MX_ROOT:[ROUTER]
$ IF F$SEARCH ("MX_ROUTER_DIR:MX_ROUTER''FILEID'.LOG;-1") .NES. "" THEN -
	PURGE/KEEP=5 MX_ROUTER_DIR:MX_ROUTER'FILEID'.LOG
$ START/INPUT=MX_EXE:MX_ROUTER.COM/OUTPUT=MX_ROUTER_DIR:MX_ROUTER'FILEID'.LOG-
    /PROC="MX Router''ITERATION'" SYS$SYSTEM:LOGINOUT.EXE
$ oldprv = F$SETPRV (oldprv)
$ GOTO LOOP
$!
$START_SMTP:
$ IF F$SEARCH("MX_EXE:MX_SMTP.EXE") .EQS. ""
$ THEN
$   WRITE SYS$OUTPUT "%MX-E-NOTINST, SMTP agent not installed; not started"
$   GOTO LOOP
$ ENDIF
$ needprv = "SYSLCK,SYSPRV,EXQUOTA,SYSNAM,PHY_IO"
$ oldprv = F$SETPRV (needprv)
$ IF .NOT. F$PRIVILEGE (needprv)
$ THEN
$   SAY "%MX-F-INSFPRV, need ''needprv' to start ''WHAT' process"
$   oldprv = F$SETPRV (oldprv)
$   GOTO LOOP
$ ENDIF
$ icmd := create
$ IF F$FILE_ATTR ("MX_EXE:MX_SMTP.EXE","KNOWN") THEN icmd := replace
$ INSTALL 'icmd' MX_EXE:MX_SMTP.EXE/OPEN/SHARE/HEADER
$ DSE MX_SMTP_DIR MX_ROOT:[SMTP]
$ DSE MX_SMTP_LOCK_DIR MX_ROOT:[SMTP.LOCK]
$ IF F$SEARCH ("MX_SMTP_LOCK_DIR:*.*_LOCK") .NES. ""
$ THEN
$   DEFINE/USER SYS$ERROR NL:
$   DELETE/NOLOG MX_SMTP_LOCK_DIR:*.*_LOCK;*/BEFORE="-4:00:00"/MODIFIED
$ ENDIF
$ IF F$SEARCH ("MX_SMTP_DIR:MX_SMTP''FILEID'.LOG;-1") .NES. "" THEN -
	PURGE/KEEP=5 MX_SMTP_DIR:MX_SMTP'FILEID'.LOG
$ START/INPUT=MX_EXE:MX_SMTP.COM/OUTPUT=MX_SMTP_DIR:MX_SMTP'FILEID'.LOG-
    /PROC="MX SMTP''ITERATION'" SYS$SYSTEM:LOGINOUT.EXE
$ oldprv = F$SETPRV (oldprv)
$ GOTO LOOP
$!
$START_HOLD:
$ IF F$SEARCH("MX_EXE:MX_SMTP.EXE") .EQS. ""
$ THEN
$   WRITE SYS$OUTPUT "%MX-E-NOTINST, SMTP agent not installed; ''what' not started"
$   GOTO LOOP
$ ENDIF
$ needprv = "SYSLCK,SYSPRV,EXQUOTA,SYSNAM,PHY_IO"
$ oldprv = F$SETPRV (needprv)
$ IF .NOT. F$PRIVILEGE (needprv)
$ THEN
$   SAY "%MX-F-INSFPRV, need ''needprv' to start ''WHAT' process"
$   oldprv = F$SETPRV (oldprv)
$   GOTO LOOP
$ ENDIF
$ DSE MX_SMTP_DIR MX_ROOT:[SMTP]
$ DSE MX_SMTP_LOCK_DIR MX_ROOT:[SMTP.LOCK]
$ tmp = F$FAO("MX_!AS!2ZL", WHAT, holdwhich)
$ START/INPUT=MX_EXE:PROCESS_HOLDING_QUEUE.COM-
       /OUTPUT=MX_SMTP_DIR:PROCESS_HOLDING_QUEUE.LOG-
       /PROC="''tmp'" SYS$SYSTEM:LOGINOUT.EXE
$ oldprv = F$SETPRV (oldprv)
$ GOTO LOOP
$!
$START_SMTP_SERVER:
$ IF F$SEARCH("MX_EXE:SMTP_SERVER.EXE") .EQS. "" .OR. -
     F$SEARCH(smtp_server_comfile) .EQS. ""
$ THEN
$   WRITE SYS$OUTPUT "%MX-E-NOTINST, SMTP server not installed; not started"
$   GOTO LOOP
$ ENDIF
$ needprv = "SYSLCK,SYSPRV,EXQUOTA,SYSNAM,PHY_IO"
$ oldprv = F$SETPRV (needprv)
$ IF .NOT. F$PRIVILEGE (needprv)
$ THEN
$   SAY "%MX-F-INSFPRV, need ''needprv' to start ''WHAT' process"
$   oldprv = F$SETPRV (oldprv)
$   GOTO LOOP
$ ENDIF
$ DSE MX_SMTP_DIR MX_ROOT:[SMTP]
$ IF F$SEARCH ("MX_SMTP_DIR:SMTP_SERVER''FILEID'.LOG;-1") .NES. "" THEN -
	PURGE/KEEP=5 MX_SMTP_DIR:SMTP_SERVER'FILEID'.LOG
$!
$ threads = F$INTEGER (F$TRNLNM ("MX_SMTP_SERVER_THREADS"))
$ ssvflags = ""
$ IF threads .GT. 8 
$ THEN
$   IF F$TRNLNM ("MX_SMTP_SERVER_PGFLQUO") .EQS. "" THEN ssvflags = ssvflags + "/PAGE_FILE=131072"
$   tmpi =  100 + threads * 2
$   ssvflags = ssvflags + "/AST_LIMIT=''tmpi'"
$   ssvflags = ssvflags + "/FILE_LIM=''tmpi'"
$   ssvflags = ssvflags + "/IO_BUF=''tmpi'"
$   ssvflags = ssvflags + "/IO_DIR=''tmpi'"
$   ssvflags = ssvflags + "/QUEUE=''tmpi'"
$ ENDIF
$ START'ssvflags'/INPUT='smtp_server_comfile'/OUTPUT=MX_SMTP_DIR:SMTP_SERVER'FILEID'.LOG-
    /PROC="''smtp_server_prcnam'" SYS$SYSTEM:LOGINOUT.EXE
$ oldprv = F$SETPRV (oldprv)
$ GOTO LOOP
$!
$START_DNSMTP:
$ IF F$SEARCH("MX_EXE:MX_DNSMTP.EXE") .EQS. ""
$ THEN
$   WRITE SYS$OUTPUT "%MX-E-NOTINST, SMTP-DECnet agent not installed; not started"
$   GOTO LOOP
$ ENDIF
$ needprv = "SYSLCK,SYSPRV,EXQUOTA,SYSNAM,PHY_IO"
$ oldprv = F$SETPRV (needprv)
$ IF .NOT. F$PRIVILEGE (needprv)
$ THEN
$   SAY "%MX-F-INSFPRV, need ''needprv' to start ''WHAT' process"
$   oldprv = F$SETPRV (oldprv)
$   GOTO LOOP
$ ENDIF
$ icmd := create
$ IF F$FILE_ATTR ("MX_EXE:MX_DNSMTP.EXE","KNOWN") THEN icmd := replace
$ INSTALL 'icmd' MX_EXE:MX_DNSMTP.EXE/OPEN/SHARE/HEADER
$ DSE MX_DNSMTP_DIR MX_ROOT:[DNSMTP]
$ IF F$SEARCH ("MX_DNSMTP_DIR:MX_DNSMTP''FILEID'.LOG;-1") .NES. "" THEN -
	PURGE/KEEP=5 MX_DNSMTP_DIR:MX_DNSMTP'FILEID'.LOG
$ START/INPUT=MX_EXE:MX_DNSMTP.COM/OUTPUT=MX_DNSMTP_DIR:MX_DNSMTP'FILEID'.LOG-
    /PROC="MX DNSMTP''ITERATION'" SYS$SYSTEM:LOGINOUT.EXE
$ oldprv = F$SETPRV (oldprv)
$ GOTO LOOP
$!
$START_UUCP:
$ IF F$SEARCH("MX_EXE:MX_UUCP.EXE") .EQS. ""
$ THEN
$   WRITE SYS$OUTPUT "%MX-E-NOTINST, ''WHAT' agent not installed; not started"
$   GOTO LOOP
$ ENDIF
$ needprv = "SYSLCK,SYSPRV,EXQUOTA"
$ oldprv = F$SETPRV (needprv)
$ IF .NOT. F$PRIVILEGE (needprv)
$ THEN
$  SAY "%MX-F-INSFPRV, need ''needprv' to start ''WHAT' process"
$   GOTO LOOP
$ ENDIF
$ DSE MX_UUCP_DIR MX_ROOT:[UUCP]
$ uuname = F$TRNLNM ("UUCP_DOMAIN_NAME")
$ IF uuname .EQS. ""
$ THEN
$   uuname = F$TRNLNM ("UUCP_HOST_NAME")
$   IF uuname .NES. "" THEN uuname = uuname + ".UUCP"
$ ENDIF
$ IF uuname .NES. "" THEN DSE MX_UUCP_HOST_NAME "''uname'"
$!
$ IF F$SEARCH ("MX_UUCP_DIR:MX_UUCP''FILEID'.LOG;-1") .NES. "" THEN -
	PURGE/KEEP=5 MX_UUCP_DIR:MX_UUCP'FILEID'.LOG
$ START/INPUT=MX_EXE:MX_UUCP.COM/OUTPUT=MX_UUCP_DIR:MX_UUCP'FILEID'>LOG-
	/PROC="MX uucp Intfc''ITERATION'" SYS$SYSTEM:LOGINOUT
$ oldprv = F$SETPRV (oldprv)
$ GOTO LOOP
$!
$START_LOCAL:
$ needprv = "SYSLCK,SYSPRV,EXQUOTA"
$ oldprv = F$SETPRV (needprv)
$ IF .NOT. F$PRIVILEGE (needprv)
$ THEN
$   SAY "%MX-F-INSFPRV, need ''needprv' to start ''WHAT' process"
$   oldprv = F$SETPRV (oldprv)
$   GOTO LOOP
$ ENDIF
$ icmd := create
$ IF F$FILE_ATTR ("MX_EXE:MX_LOCAL.EXE","KNOWN") THEN icmd := replace
$ INSTALL 'icmd' MX_EXE:MX_LOCAL.EXE/OPEN/SHARE/HEADER
$ DSE MX_LOCAL_DIR MX_ROOT:[LOCAL]
$ IF F$SEARCH ("MX_LOCAL_DIR:MX_LOCAL''FILEID'.LOG;-1") .NES. "" THEN -
	PURGE/KEEP=5 MX_LOCAL_DIR:MX_LOCAL'FILEID'.LOG
$ START/INPUT=MX_EXE:MX_LOCAL.COM/OUTPUT=MX_LOCAL_DIR:MX_LOCAL'FILEID'.LOG-
    /PROC="MX Local''ITERATION'" SYS$SYSTEM:LOGINOUT.EXE
$ oldprv = F$SETPRV (oldprv)
$ GOTO LOOP
$!
$START_MLF:
$ IF F$SEARCH("MX_EXE:MX_MLF.EXE") .EQS. ""
$ THEN
$   WRITE SYS$OUTPUT "%MX-E-NOTINST, Mailing List/File Server agent not installed; not started"
$   GOTO LOOP
$ ENDIF
$ needprv = "SYSLCK,SYSPRV,EXQUOTA"
$ oldprv = F$SETPRV (needprv)
$ IF .NOT. F$PRIVILEGE (needprv)
$ THEN
$   SAY "%MX-F-INSFPRV, need ''needprv' to start ''WHAT' process"
$   oldprv = F$SETPRV (oldprv)
$   GOTO LOOP
$ ENDIF
$ DSE MX_MLF_DIR MX_ROOT:[MLF]
$ MX_MLIST_DIR := MX_ROOT:[MLF.MAILING_LISTS]
$ IF F$PARSE ("MX_ROOT:[LOCAL.MLIST]") .NES. "" THEN -
    MX_MLIST_DIR = MX_MLIST_DIR + ",MX_ROOT:[LOCAL.MLIST]"
$ icmd := create
$ IF F$FILE_ATTR ("MX_EXE:MX_MLF.EXE","KNOWN") THEN icmd := replace
$ INSTALL 'icmd' MX_EXE:MX_MLF.EXE/OPEN/SHARE/HEADER
$ DSE MX_MLIST_DIR 'MX_MLIST_DIR'
$ IF F$SEARCH ("MX_MLF_DIR:MX_MLF''FILEID'.LOG;-1") .NES. "" THEN -
	PURGE/KEEP=5 MX_MLF_DIR:MX_MLF'FILEID'.LOG
$ START/INPUT=MX_EXE:MX_MLF.COM/OUTPUT=MX_MLF_DIR:MX_MLF'FILEID'.LOG-
    /PROC="MX MLF''ITERATION'" SYS$SYSTEM:LOGINOUT.EXE
$ oldprv = F$SETPRV (oldprv)
$ GOTO LOOP
$!
$START_SITE:
$ IF F$SEARCH("MX_EXE:MX_SITE.EXE") .EQS. ""
$ THEN
$   WRITE SYS$OUTPUT "%MX-E-NOTINST, SITE agent not installed; not started"
$   GOTO LOOP
$ ENDIF
$ needprv = "SYSLCK,SYSPRV,EXQUOTA"
$ oldprv = F$SETPRV (needprv)
$ IF .NOT. F$PRIVILEGE (needprv)
$ THEN
$   SAY "%MX-F-INSFPRV, need ''needprv' to start ''WHAT' process"
$   oldprv = F$SETPRV (oldprv)
$   GOTO LOOP
$ ENDIF
$ icmd := create
$ IF F$FILE_ATTR ("MX_EXE:MX_SITE.EXE","KNOWN") THEN icmd := replace
$ INSTALL 'icmd' MX_EXE:MX_SITE.EXE/OPEN/SHARE/HEADER
$ DSE MX_SITE_DIR MX_ROOT:[SITE]
$ IF F$SEARCH ("MX_SITE_DIR:MX_SITE''FILEID'.LOG;-1") .NES. "" THEN -
	PURGE/KEEP=5 MX_SITE_DIR:MX_SITE'FILEID'.LOG
$ START/INPUT=MX_EXE:MX_SITE.COM/OUTPUT=MX_SITE_DIR:MX_SITE'FILEID'.LOG-
    /PROC="MX Site Agent''ITERATION'" SYS$SYSTEM:LOGINOUT.EXE
$ oldprv = F$SETPRV (oldprv)
$ GOTO LOOP
$ exit
