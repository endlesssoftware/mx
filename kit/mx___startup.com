$! MX___STARTUP.COM
$! Internal startup command procedure for Message Exchange software.
$! Copyright (c) 2008, Matthew Madison.
$! Copyright (c) 2012, Endless Software Solutions.
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
$ SET := SET
$ SET NOON
$ SET SYMBOL/SCOPE=(NOLOCAL,NOGLOBAL)
$!
$ SAY := WRITE SYS$OUTPUT
$ NODE = F$GETSYI ("NODENAME")
$ required_privileges = "CMKRNL,SYSPRV,SYSNAM"
$ prev_privs = f$setprv(required_privileges)
$ if .not. f$privilege(required_privileges) then goto NO_PRIVILEGES
$ IF "''P1'" .EQS. "" THEN P1 = "*"
$ PLIST = P1
$ P = -1
$ queue_exists = 0
$!
$PLOOP:
$ P = P + 1
$ P1 = F$EDIT (F$ELEMENT (P, ",", PLIST), "TRIM,UPCASE")
$ IF P1 .EQS. "," THEN GOTO BYE
$ IF P1 .EQS. "*" .OR. P1 .EQS. "LOGICALS"
$ THEN
$   GOSUB DO_LOGICALS
$ ELSE
$   queue_exists = F$SEARCH ("MX_FLQ_DIR:MX_SYSTEM_QUEUE.FLQ_CTL") .NES. ""
$ ENDIF
$ IF P1 .EQS. "LOGICALS" .OR. .NOT. queue_exists THEN GOTO BYE
$ IF F$EXTRACT (0, 4, P1) .EQS. "HOLD"
$ THEN
$   holdwhich = F$INTEGER (F$EXTRACT (4, -1, P1))
$   IF holdwhich .GE. 1 .AND. holdwhich .LE. 32 THEN @MX_EXE:MX_START 'p1'
$   GOTO PLOOP
$ ENDIF
$ tag = ""
$ i = F$LOCATE ("=", p1)
$ IF i .GE. F$LENGTH (p1) THEN i = F$LOCATE ("#", p1)
$ IF i .LT. F$LENGTH (p1)
$ THEN
$   tag = F$EDIT (F$EXTRACT (i+1, -1, p1), "TRIM")
$   p1  = F$EDIT (F$EXTRACT (0, i, p1), "TRIM")
$ ENDIF
$!
$ OPEN/READ/ERR=NODATA STUP MX_DIR:MX_STARTUP_INFO.DAT
$ GOTO READ_INFO
$NODATA:
$ SAY "%MX-F-NODATA, could not open MX_STARTUP_INFO.DAT file"
$ EXIT
$!
$NO_PRIVILEGES:
$       WRITE SYS$OUTPUT "Insufficient privileges to start the MX Mailer"
$	WRITE SYS$OUTPUT "Requires ",REQUIRED_PRIVILEGES," privileges."
$BYE:
$   PREV_PRIVS = F$SETPRV(PREV_PRIVS)
$   EXIT
$!
$READ_INFO:
$ READ/END=CLOSE_INFO/ERR=CLOSE_INFO STUP INFOREC
$ PROCESS = F$EXTRACT (3,-1,F$EDIT (F$ELEMENT (0, ":", INFOREC), "TRIM,UPCASE"))
$ IF P1 .NES. "*" .AND. P1 .NES. PROCESS THEN GOTO READ_INFO
$ NODELIST = F$EDIT (F$ELEMENT (1, ":", INFOREC), "TRIM,UPCASE")
$ IF NODELIST .EQS. ":" THEN NODELIST = ""
$ I = -1
$NODE_LOOP:
$ I = I + 1
$ N = F$EDIT (F$ELEMENT (I, ",", NODELIST), "TRIM")
$ IF N .EQS. "," THEN GOTO READ_INFO
$ COUNT = F$EDIT (F$ELEMENT (1, "=", N), "TRIM")
$ IF COUNT .EQS. "="
$ THEN
$   COUNT = "1"
$ ELSE
$   N = F$EDIT (F$ELEMENT (0, "=", N), "TRIM")
$ ENDIF
$ IF N .NES. "*" .AND. N .NES. NODE THEN GOTO NODE_LOOP
$ CT = F$INTEGER (COUNT)
$ IF tag  .NES. ""
$ THEN
$   IF process .EQS. "SMTP_SERVER"
$   THEN
$       @MX_EXE:MX_START "''process'=''tag'"
$   ELSE
$       tag = F$INTEGER (tag)
$       IF tag .GT. ct THEN GOTO close_info
$       tmp = process
$       IF tag .GT. 1 THEN tmp = process + "#''tag'"
$       @MX_EXE:MX_START "''tmp'"
$   ENDIF
$   GOTO close_info
$ ENDIF
$ @MX_EXE:MX_START 'PROCESS
$ J = 1
$COUNT_LOOP:
$ IF J .GE. CT THEN GOTO node_loop
$ J = J + 1
$ @MX_EXE:MX_START "''PROCESS'#''J'"
$ GOTO COUNT_LOOP
$!
$CLOSE_INFO:
$ CLOSE STUP
$ GOTO PLOOP
$!
$DO_LOGICALS:
$ hw_model = F$GETSYI("HW_MODEL")
$ IF hw_model .GT. 0 .AND. hw_model .LT. 1024
$ THEN exedir = "EXE"
$ ELSE exedir = "''F$EDIT(F$GETSYI("ARCH_NAME"),"TRIM,UPCASE")'_EXE"
$ ENDIF
$ PRC = F$ENVIRONMENT ("PROCEDURE")
$ DEV = F$PARSE (PRC,,, "DEVICE", "NO_CONCEAL")
$ DIR = F$PARSE (PRC,,, "DIRECTORY", "NO_CONCEAL")- "][" - ".''exedir']"
$ SUBDIRS = "ROUTER,SMTP,DNSMTP,UUCP,LOCAL,SITE,MLF,EXAMPLES"
$!
$ DEFINE/NOLOG/SYSTEM/EXEC MX_DEVICE 'DEV'/TRANSLATION=(CONCEALED,TERMINAL)
$ DEFINE/NOLOG/SYSTEM/EXEC MX_ROOT  MX_DEVICE:'DIR'.]/TRANSLATION=CONCEALED
$ DEFINE/NOLOG/SYSTEM/EXEC MX_DIR   MX_DEVICE:'DIR']
$ DEFINE/NOLOG/SYSTEM/EXEC MX_EXE   MX_ROOT:['exedir']
$ DEFINE/NOLOG/SYSTEM/EXEC MX_MCP_HELPLIB MX_DIR:MX_MCP_HELPLIB
$ DEFINE/NOLOG/SYSTEM/EXEC MX_ALIAS_HELPLIB MX_DIR:MX_ALIAS_HELPLIB
$ DEFINE/NOLOG/SYSTEM/EXEC MX_REJMAN_HELPLIB MX_DIR:MX_REJMAN_HELPLIB
$ I = -1
$SUBDIR_LOGICAL_LOOP:
$ I = I + 1
$ SUBDIR = F$ELEMENT (I,",",SUBDIRS)
$ IF SUBDIR .EQS. "," THEN GOTO DO_DOC_LOGICALS
$ IF F$PARSE ("MX_ROOT:[''SUBDIR']") .NES. ""
$ THEN
$   DEFINE/NOLOG/SYSTEM/EXEC MX_'SUBDIR'_DIR MX_ROOT:['SUBDIR']
$   IF SUBDIR .EQS. "MLF"
$   THEN
$   	MX_MLIST_DIR := MX_ROOT:[MLF.MAILING_LISTS]
$   	IF F$PARSE ("MX_ROOT:[LOCAL.MLIST]") .NES. "" THEN -
    	    MX_MLIST_DIR = MX_MLIST_DIR + ",MX_ROOT:[LOCAL.MLIST]"
$    	DEFINE/NOLOG/SYSTEM/EXEC MX_MLIST_DIR 'MX_MLIST_DIR'
$   ENDIF
$ ENDIF
$ GOTO SUBDIR_LOGICAL_LOOP
$DO_DOC_LOGICALS:
$ IF F$PARSE ("MX_ROOT:[DOC]") .NES. "" THEN DEFINE/SYSTEM/EXEC/NOLOG MX_DOC MX_ROOT:[DOC]
$ OPEN/READ/ERR=NOLOGI LOGI MX_DIR:MX_LOGICALS.DAT
$ GOTO READ_LOGI
$NOLOGI:
$ SAY "%MX-F-NOLOGI, could not open MX_LOGICALS.DAT file"
$ RETURN
$!
$READ_LOGI:
$ READ/END=CLOSE_LOGI/ERR=CLOSE_LOGI LOGI LOGIREC
$ LOGICAL = F$EDIT (F$ELEMENT (0, "\", LOGIREC), "TRIM,UPCASE")
$ QUALIFIERS = F$EDIT (F$ELEMENT (1, "\", LOGIREC), "TRIM,UPCASE")
$ VALUE = F$EDIT (F$ELEMENT (2, "\", LOGIREC), "TRIM")
$ DEFINE/NOLOG'QUALIFIERS "''LOGICAL'" "''VALUE'"
$ IF logical .EQS. "MX_FLQ_DIR" THEN mx_flq_dir = value
$ GOTO READ_LOGI
$CLOSE_LOGI:
$ CLOSE LOGI
$!
$ @MX_EXE:MX_START MAILSHR
$!
$ IF "''mx_flq_dir'" .EQS. "" .OR. F$SEARCH ("MX_FLQ_DIR:MX_SYSTEM_QUEUE.FLQ_CTL") .EQS. ""
$ THEN
$   SAY "%MX-W-NOQUECTL, MX message queue control file does not exist; configure queues with MX_DIR:MXCONFIG.COM"
$   RETURN
$ ENDIF
$ queue_exists = 1
$ mx_flq_base = F$EXTRACT (0, F$LENGTH (mx_flq_dir)-1, mx_flq_dir)
$ i = 0
$FLQ_Subdir_Loop:
$ IF F$PARSE ("''mx_flq_base'.''i']") .EQS. ""
$ THEN
$   CREATE/DIRECTORY 'mx_flq_base'.'i']
$   SET ACL/DELETE=ALL 'mx_flq_base']'i'.DIR;1
$   SET ACL/ACL=(DEFAULT_PROTECTION,S:RWED,O:RWED,G,W) 'mx_flq_base']'i'.DIR;1
$ ENDIF
$ i = i + 1
$ IF i .LE. 9 THEN GOTO FLQ_Subdir_Loop
$!
$ RETURN
