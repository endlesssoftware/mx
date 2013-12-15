$! [MX.KIT]KITINSTAL.COM
$!
$!  KITINSTAL procedure for installing MX.
$!
$! Copyright (c) 2008, Matthew Madison.
$! Copyright (c) 2011, Endless Software Solutions.
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
$ ON CONTROL_Y THEN GOTO MX_CONTROL_Y
$ ON WARNING THEN GOTO MX_FAIL
$!
$ DELETE_SYMBOL := DELETE/SYMBOL
$!
$ IF P1 .EQS. "VMI$_INSTALL" THEN GOTO MX_INSTALL
$ IF P1 .EQS. "VMI$_POSTINSTALL" THEN GOTO MX_POSTINSTALL
$ EXIT VMI$_UNSUPPORTED
$!
$MX_CONTROL_Y:
$ IF F$TRNLNM ("MX_STMP") .NES. "" THEN CLOSE MX_STMP
$ IF F$TRNLNM ("MX_LOGI") .NES. "" THEN CLOSE MX_LOGI
$ IF F$TRNLNM ("MX_OLDLOG") .NES. "" THEN CLOSE MX_OLDLOG
$ IF F$TRNLNM ("MX_DEVICE", "LNM$PROCESS") .NES. "" THEN DEASSIGN MX_DEVICE
$ IF F$TRNLNM ("MX_ROOT", "LNM$PROCESS") .NES. "" THEN DEASSIGN MX_ROOT
$ IF F$TRNLNM ("MX_INSTALL_ROOT", "LNM$PROCESS") .NES. "" THEN -
    DEASSIGN MX_INSTALL_ROOT
$ IF F$TRNLNM ("MX_FLQ_SHR","LNM$PROCESS") .NES. "" THEN DEASSIGN MX_FLQ_SHR
$ IF F$TRNLNM ("MX_SHR","LNM$PROCESS") .NES. "" THEN DEASSIGN MX_SHR
$ IF F$TRNLNM ("MX_MSG","LNM$PROCESS") .NES. "" THEN DEASSIGN MX_MSG
$ IF F$TRNLNM ("MX_SMTP_MSG","LNM$PROCESS") .NES. "" THEN DEASSIGN MX_SMTP_MSG
$ VMI$CALLBACK CONTROL_Y
$!
$MX_FAIL:
$ MX_STATUS == $STATUS
$ IF F$TRNLNM ("MX_OLD_FLQ") .NES. "" THEN CLOSE MX_OLD_FLQ
$ IF F$TRNLNM ("MX_LOGI") .NES. "" THEN CLOSE MX_LOGI
$ IF F$TRNLNM ("MX_OLDLOG") .NES. "" THEN CLOSE MX_OLDLOG
$ IF F$TRNLNM ("MX_STMP") .NES. "" THEN CLOSE MX_STMP
$ IF F$TRNLNM ("MX_T") .NES. "" THEN CLOSE MX_T
$ IF F$TRNLNM ("MX_U") .NES. "" THEN CLOSE MX_U
$ IF F$TRNLNM ("MX_TMP") .NES. "" THEN DEASSIGN MX_TMP
$ IF F$TRNLNM ("MX_DEVICE", "LNM$PROCESS") .NES. "" THEN DEASSIGN MX_DEVICE
$ IF F$TRNLNM ("MX_ROOT", "LNM$PROCESS") .NES. "" THEN DEASSIGN MX_ROOT
$ IF F$TRNLNM ("MX_INSTALL_ROOT", "LNM$PROCESS") .NES. "" THEN DEASSIGN MX_INSTALL_ROOT
$ IF F$TRNLNM ("MX_FLQ_SHR","LNM$PROCESS") .NES. "" THEN DEASSIGN MX_FLQ_SHR
$ IF F$TRNLNM ("MX_SHR","LNM$PROCESS") .NES. "" THEN DEASSIGN MX_SHR
$ IF F$TRNLNM ("MX_MSG","LNM$PROCESS") .NES. "" THEN DEASSIGN MX_MSG
$ IF F$TRNLNM ("MX_SMTP_MSG","LNM$PROCESS") .NES. "" THEN DEASSIGN MX_SMTP_MSG
$ IF F$TRNLNM ("BIN_DIR","LNM$PROCESS") .NES. "" THEN DEASSIGN BIN_DIR
$ EXIT 'MX_STATUS
$!
$!
$MX_INSTALL:
$!
$ IF P2 THEN SET VERIFY
$ VMI$CALLBACK TELL_QA "MX KITINSTAL has TELL_QA callbacks enabled."
$ VMI$CALLBACK SET POSTINSTALL YES
$ IF F$TRNLNM("MX_RUN_MXCONFIG","LNM$PROCESS")  .NES. "" THEN DEASSIGN MX_RUN_MXCONFIG
$!
$!
$ MX_SAY := WRITE SYS$OUTPUT
$ DEFINE BIN_DIR VMI$KWD:
$ MX_UPGRADING = 0
$!
$! Determine Architecture type
$!
$ tmp = F$GETSYI ("HW_MODEL")
$ IF tmp .GT. 0 .AND. tmp .LT. 1024
$ THEN
$   mx_arch = "VAX"
$   mx_system_type = mx_arch
$   opt = ".OPT"
$   mx_exe_dir = "EXE"
$ ELSE
$   mx_system_type = F$GETSYI ("ARCH_NAME")
$   mx_arch = F$EDIT (mx_system_type, "TRIM,UPCASE")
$   opt = ".''mx_arch'_OPT"
$   mx_exe_dir = "''mx_arch'_EXE"
$ ENDIF
$ IF mx_arch .EQS. "VAX"
$ THEN
$   MX_REQD_VMSVER = "V6.2"
$   MX_REQD_VMSVER_OLD = "062"
$   base_saveset   = "D"
$   smtp_saveset   = "G"
$   other_saveset  = "J"
$ ENDIF
$ IF mx_arch .EQS. "ALPHA"
$ THEN
$   MX_REQD_VMSVER = "V6.2"
$   MX_REQD_VMSVER_OLD = "062"
$   base_saveset   = "B"
$   smtp_saveset   = "E"
$   other_saveset  = "H"
$ ENDIF
$ IF mx_arch .EQS. "IA64"
$ THEN
$   MX_REQD_VMSVER = "V8.2"
$   MX_REQD_VMSVER_OLD = "082"
$   base_saveset   = "C"
$   smtp_saveset   = "F"
$   other_saveset  = "I"
$ ENDIF
$ VMI$CALLBACK CHECK_VMS_VERSION MX_VMSVEROK 'MX_REQD_VMSVER_OLD'
$ IF .NOT. MX_VMSVEROK
$ THEN
$   VMI$CALLBACK MESSAGE E VMSVER -
        "This product requires OpenVMS ''mx_system_type' ''MX_REQD_VMSVER' to run."
$   EXIT VMI$_FAILURE
$ ENDIF
$!
$ VMI$CALLBACK CHECK_NET_UTILIZATION MX_ENOUGHDISK 2500 30 2500
$ IF .NOT. MX_ENOUGHDISK THEN EXIT VMI$_FAILURE
$!
$ VMI$CALLBACK SET SAFETY CONDITIONAL 4000
$!
$ MX_INSTALL_NODE = F$GETSYI ("NODENAME")
$!
$ OPEN/READ MX_T VMI$KWD:MX_INSTALLING_VERSION.DAT
$ READ MX_T mx___tmp  ! first line blank
$ READ MX_T mx___tmp
$ CLOSE MX_T
$ IF F$LOCATE (":", mx___tmp) .LT. F$LENGTH (mx___tmp)
$ THEN
$   mx_installing_version = F$EDIT (F$ELEMENT (1, ":", mx___tmp), "TRIM,COMPRESS")
$ ELSE
$   mx_installing_version = mx___tmp
$ ENDIF
$! Pull off architecture name
$ mx_installing_version = f$element(1," ", mx_installing_version)
$!
$ mx_say ""
$ mx_say F$FAO("                Message Exchange !AS Installation Procedure", mx_installing_version)
$ TYPE SYS$INPUT:

        Copyright (c) 2008, Matthew Madison.
        Copyright (c) 2012, Endless Software Solutions.

        All rights reserved.

        Redistribution and use in source and binary forms, with or without
        modification, are permitted provided that the following conditions
        are met:

            * Redistributions of source code must retain the above
              copyright notice, this list of conditions and the following
              disclaimer.
            * Redistributions in binary form must reproduce the above
              copyright notice, this list of conditions and the following
              disclaimer in the documentation and/or other materials provided
              with the distribution.
            * Neither the name of the copyright owner nor the names of any
              other contributors may be used to endorse or promote products
              derived from this software without specific prior written
              permission.

        THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
        "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
        LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
        A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
        OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
        SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
        LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
        DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
        THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
        (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
        OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

$!
$ mx_upgrading = 0
$ mx_reinstalling = 0
$!
$ IF F$SEARCH ("SYS$STARTUP:MX_STARTUP.COM") .EQS. "" THEN GOTO Def_Root_From_Logicals
$!
$ MX_DEF_ROOT = ""
$ OPEN/READ/ERR=Def_Root_From_Logicals mx_stmp SYS$STARTUP:MX_STARTUP.COM
$Loop_Read_Stup:
$ READ/END=End_Read_Stup/ERR=End_Read_Stup mx_stmp mx_tmp
$ mx_tmp = F$EDIT (F$ELEMENT (1, " ", mx_tmp), "UPCASE")
$ IF F$EXTRACT (0,1,mx_tmp) .NES. "@" THEN GOTO Loop_Read_Stup
$ mx_tmp = F$EXTRACT (1, -1, mx_tmp)
$ mx_missing = "$$MISSING$$:[$$MISSING$$]$$MISSING$$.COM"
$ IF F$PARSE (mx_tmp, mx_missing,,"NAME","SYNTAX_ONLY") .NES. "MX___STARTUP" THEN GOTO Loop_Read_Stup
$ mx_tmp = F$PARSE (mx_tmp, mx_missing,,"DEVICE","SYNTAX_ONLY") +-
    	    	(F$PARSE (mx_tmp, mx_missing,,"DIRECTORY","SYNTAX_ONLY") - ".''mx_exe_dir'") - "><" - "]["
$ IF F$PARSE (mx_tmp) .EQS. "" THEN GOTO End_Read_Stup
$ IF F$PARSE (mx_tmp,,,"DEVICE","SYNTAX_ONLY") .NES. "MX_DEVICE:" THEN MX_DEF_ROOT = mx_tmp - "]"
$End_Read_Stup:
$ CLOSE mx_stmp
$ IF "''MX_DEF_ROOT'" .NES. "" THEN GOTO Do_Upgrade_Check
$!
$Def_Root_From_Logicals:
$ MX_DEF_ROOT = F$PARSE ("MX_ROOT:[000000]",,,,"NO_CONCEAL")
$ IF "''MX_DEF_ROOT'" .EQS. ""
$ THEN
$   MX_DEF_ROOT = "SYS$SYSDEVICE:[MX.]"
$ ELSE
$   MX_DEF_ROOT = F$PARSE ("MX_ROOT:[000000]",,,"DEVICE","NO_CONCEAL")+-
    	    	  F$PARSE ("MX_ROOT:[000000]",,,"DIRECTORY","NO_CONCEAL")--
    	    	  "[000000]"
$ ENDIF
$ MX_DEF_ROOT = MX_DEF_ROOT - ".]"
$!
$Do_Upgrade_Check:
$ IF F$SEARCH ("''mx_def_root'.''mx_exe_dir']MX_SHR.EXE") .EQS. "" THEN GOTO No_Upgrade_Check
$ SET NOON
$ DEFINE/USER MX_SHR 'mx_def_root'.'mx_exe_dir']MX_SHR.EXE
$ DEFINE/USER MX_FLQ_SHR 'mx_def_root'.'mx_exe_dir']MX_FLQ_SHR.EXE
$ DEFINE/USER SYS$OUTPUT VMI$KWD:STATUS_OUTPUT.TMP
$ DEFINE/USER SYS$ERROR _NL:
$ MCR 'mx_def_root'.'mx_exe_dir']MCP /NOFILE STATUS
$ IF F$SEARCH ("VMI$KWD:STATUS_OUTPUT.TMP") .EQS. "" THEN GOTO Skip_Running_Check
$ OPEN/READ mx_t VMI$KWD:STATUS_OUTPUT.TMP
$ READ/END=Skip_Running_Check/ERR=Skip_Running_Check mx_t mx___tmp
$ CLOSE mx_t
$ IF F$ELEMENT (0, " ", F$EDIT (mx___tmp, "TRIM,COMPRESS,UPCASE")) .NES. "PID" THEN GOTO Skip_Running_Check
$ TYPE SYS$INPUT:

    ** WARNING **
    The installation procedure has detected that one or more MX
    delivery agents are still running.  It is strongly recommended
    that you exit the installation procedure now and restart it
    after shutting down all delivery agents.

$!
$ VMI$CALLBACK MESSAGE I MXNOTDOWN "MX delivery agents have not been shut down."
$ VMI$CALLBACK ASK mx_ok "Do you want to continue this installation anyway" "NO" B
$ IF .NOT. mx_ok THEN EXIT VMI$_FAILURE
$Skip_Running_Check:
$ IF F$TRNLNM ("MX_TMP","LNM$PROCESS") .NES. "" THEN CLOSE MX_TMP
$!
$ DEFINE/USER MX_SHR 'mx_def_root'.'mx_exe_dir']MX_SHR.EXE
$ DEFINE/USER MX_FLQ_SHR 'mx_def_root'.'mx_exe_dir']MX_FLQ_SHR.EXE
$ DEFINE/USER SYS$OUTPUT _NL:
$ DEFINE/USER SYS$ERROR _NL:
$ MCR 'mx_def_root'.'mx_exe_dir']MCP /NOFILE SHOW VERSION/OUTPUT=VMI$KWD:CURRENT_VERSION.TMP
$ SET ON
$ OPEN/READ/ERROR=No_Upgrade_Check MX_T VMI$KWD:CURRENT_VERSION.TMP
$ READ/ERROR=Upgrade_Check_Error/END=Upgrade_Check_Error MX_T mx___tmp ! first line blank
$ READ/ERROR=Upgrade_Check_Error/END=Upgrade_Check_Error MX_T mx___tmp
$ CLOSE MX_T
$ mx_version = F$ELEMENT (1," ", F$EDIT(F$ELEMENT(1,":",mx___tmp),"TRIM,COMPRESS"))
$ VMI$CALLBACK MESSAGE I INSTALDET "An installation of MX ''mx_version' has been detected at ''mx_def_root']."
$Ask_Reinstall:
$ IF mx_version .EQS. mx_installing_version
$ THEN
$   VMI$CALLBACK ASK mx_ok "Do you wish to update the existing installation" "YES" B
$   IF .NOT. mx_ok THEN GOTO No_Upgrade_Check
$   mx_root = mx_def_root + "]"
$   mx_reinstalling = 1
$   DEFINE MX_RUN_MXCONFIG NONE
$   GOTO After_Upgrade_Check
$ ENDIF
$!
$ mx_ft_letter	   = F$EXTRACT (0, 1, MX_VERSION)
$ mx_major_version = F$INTEGER (F$EXTRACT (1, -1, F$ELEMENT (0, ".", MX_VERSION)))
$ mx_minor_version = F$INTEGER (F$ELEMENT (0, "-", F$ELEMENT (1, ".", MX_VERSION)))
$ mx_maint_version = F$EDIT (F$ELEMENT (1, "-", F$ELEMENT (1, ".", MX_VERSION)), "TRIM,COMPRESS,UPCASE") - "X"
$ mx_install_ft_letter	   = F$EXTRACT (0, 1, MX_INSTALLING_VERSION)
$ mx_install_major_version = F$INTEGER (F$EXTRACT (1, -1, F$ELEMENT (0, ".", MX_INSTALLING_VERSION)))
$ mx_install_minor_version = F$INTEGER (F$ELEMENT (0, "-", F$ELEMENT (1, ".", MX_INSTALLING_VERSION)))
$ mx_install_maint_version = F$EDIT (F$ELEMENT (1, "-", F$ELEMENT (1, ".", MX_INSTALLING_VERSION)), "TRIM,COMPRESS,UPCASE")
$ IF mx_major_version .LT. 6 THEN GOTO Cannot_Upgrade
$ IF mx_major_version .LT. mx_install_major_version THEN GOTO Ask_Upgrade
$ IF mx_major_version .GT. mx_install_major_version THEN GOTO Cannot_Upgrade
$ IF mx_minor_version .LT. mx_install_minor_version THEN GOTO Ask_Upgrade
$ IF mx_minor_version .GT. mx_install_minor_version THEN GOTO Cannot_Upgrade
$ IF mx_maint_version .LTS. mx_install_maint_version THEN GOTO Ask_Upgrade
$ IF mx_maint_version .GTS. mx_install_maint_version THEN GOTO Cannot_Upgrade
$ IF mx_ft_letter .LTS. mx_install_ft_letter THEN GOTO Ask_Upgrade
$ IF mx_ft_letter .GTS. mx_install_ft_letter THEN GOTO Cannot_Upgrade
$!  Hmm.  Everything equal?  Should have been caught above.
$ GOTO Ask_Reinstall
$!
$Ask_Upgrade:
$ VMI$CALLBACK ASK mx_ok "Do you wish to upgrade this installation to ''mx_installing_version'" "YES" B
$ IF .NOT. mx_ok THEN GOTO No_Upgrade_Check
$ mx_root = mx_def_root + "]"
$ mx_upgrading = 1
$ DEFINE MX_RUN_MXCONFIG UPGRADE
$ GOTO After_Upgrade_Check
$!
$Cannot_Upgrade:
$ VMI$CALLBACK MESSAGE I INCOMPATVER "The installed version cannot be upgraded to ''mx_installing_version'."
$ TYPE SYS$INPUT:

    ** WARNING **
    The installation procedure has detected that you are attempting
    to upgrade a pre-V6.0 installation.  This is NOT supported.  It
    is recommended that you consult the release note for a suitable
    upgrade path.  If you have already performed the necessary
    actions or are unconcerned with the consequences of such action
    then answer "YES" to the following question.

$ VMI$CALLBACK ASK mx_ok "Do you wish to proceed with the installation" "NO" B
$ IF .NOT. mx_ok THEN EXIT VMI$_FAILURE
$ GOTO No_Upgrade_Check
$!
$Upgrade_Check_Error:
$ CLOSE mx_tmp
$!
$No_Upgrade_Check:
$ DEFINE MX_RUN_MXCONFIG INSTALL
$ IF mx_def_root .EQS. ""
$ THEN
$   mx_def_root = "SYS$SYSDEVICE:[MX]"
$ ELSE
$   mx_def_root = mx_def_root + "]"
$ ENDIF
$!
$ TYPE SYS$INPUT:

    MX Root Directory Location
    --------------------------

    MX places most of its files in a private directory tree.  This tree
    can be located on any disk that has sufficient space available to
    hold the installed files plus all temporary files and logs created
    by MX agent processes.

    In a VMScluster, the disk device selected should be accessible to
    all VMScluster nodes that will be using MX.

$!
$Ask_MX_Top:
$ VMI$CALLBACK ASK MX_ROOT "Where should the MX root directory be located?" "''MX_DEF_ROOT'"
$ IF F$PARSE(MX_ROOT,"$$NOSUCHDEV$$:[$$NOSUCHDIR$$]",,"DEVICE","SYNTAX_ONLY") .EQS. "$$NOSUCHDEV$$:" .OR. -
     F$PARSE(MX_ROOT,"$$NOSUCHDEV$$:[$$NOSUCHDIR$$]",,"DIRECTORY","SYNTAX_ONLY") .EQS. "[$$NOSUCHDIR$$]" .OR. -
     F$PARSE(MX_ROOT,,,,"SYNTAX_ONLY") .EQS. "" .OR. -
     F$LOCATE(">[",MX_ROOT) .LT. F$LENGTH(MX_ROOT) .OR. F$LOCATE("]<",MX_ROOT) .LT. F$LENGTH(MX_ROOT)
$ THEN
$   TYPE SYS$INPUT:

    Please enter a device and directory specification.

$   GOTO Ask_MX_Top
$ ENDIF
$!
$After_Upgrade_Check:
$ MX_ROOT = F$PARSE(MX_ROOT,,,"DEVICE","SYNTAX_ONLY") + -
              "[" + (F$EXTRACT(1,-1,F$PARSE(MX_ROOT,,,"DIRECTORY","SYNTAX_ONLY")) --
    	    	    	 "][" - "><" - ">[" - "]<" - "]" - ">")
$ MX_IROOT = F$PARSE(MX_ROOT+"]",,,"DEVICE","SYNTAX_ONLY,NO_CONCEAL") + -
              "[" + (F$EXTRACT(1,-1,F$PARSE(MX_ROOT+"]",,,"DIRECTORY","NO_CONCEAL,SYNTAX_ONLY")) --
    	    	    	 "][" - "><" - ">[" - "]<" - "]" - ">") + "]"
$ MX_INSTALL_DEVICE = F$PARSE (MX_IROOT,,,"DEVICE")
$ MX_INSTALL_ROOT = "MX_DEVICE:"+ F$PARSE (MX_IROOT,,,"DIRECTORY") - "]" + ".]"
$ DEFINE MX_DEVICE 'MX_INSTALL_DEVICE'/TRANSLATION=(CONCEALED,TERMINAL)
$ DEFINE MX_INSTALL_ROOT 'MX_INSTALL_ROOT'/TRANSLATION=CONCEALED
$ DEFINE MX_ROOT 'MX_INSTALL_ROOT'/TRANSLATION=CONCEALED
$!
$ IF F$PARSE ("''MX_ROOT']").eqs."" then -
    VMI$CALLBACK CREATE_DIRECTORY USER 'MX_ROOT'] -
    		"/OWNER=[1,4]/PROT=(S:RWE,O:RWE,G:RE,W:E)"
$ IF F$PARSE("''MX_ROOT'.EXE]").eqs."" then -
	VMI$CALLBACK CREATE_DIRECTORY USER 'MX_ROOT'.EXE] -
    		"/OWNER=[1,4]/PROT=(S:RWE,O:RWE,G:RE,W:E)"
$ IF F$PARSE ("''MX_ROOT'.ALPHA_EXE]").eqs."" then -
	VMI$CALLBACK CREATE_DIRECTORY USER 'MX_ROOT'.ALPHA_EXE] -
	    	"/OWNER=[1,4]/PROT=(S:RWE,O:RWE,G:RE,W:E)"
$ IF F$PARSE ("''MX_ROOT'.IA64_EXE]").eqs."" then -
	VMI$CALLBACK CREATE_DIRECTORY USER 'MX_ROOT'.IA64_EXE] -
	    	"/OWNER=[1,4]/PROT=(S:RWE,O:RWE,G:RE,W:E)"
$!
$ mx_install_exe_only = 0
$ mx_cluster = f$getsyi("CLUSTER_MEMBER")
$ IF .NOT. mx_cluster THEN GOTO Skip_ExeOnly_Check
$ GOSUB Get_Install_Info
$ IF Installed_VERS .NES. mx_installing_version THEN installed_ARCH = ""
$ IF installed_ARCH .EQS. "" THEN GOTO Skip_ExeOnly_Check
$ IF F$LOCATE ("[''mx_arch']", installed_ARCH) .LT. F$LENGTH (installed_ARCH) THEN GOTO Skip_ExeOnly_Check
$ VMI$CALLBACK MESSAGE I MIXCLUSDET "Mixed-architecture VMScluster installation detected"
$ TYPE SYS$INPUT:

    Mixed-Architecture Cluster Support
    ----------------------------------

    MX can support a mixed-architecture VMScluster in a single
    directory tree.  In a mixed-architecture cluster, you must
    install MX once per architecture.  Only one full installation
    is required; on the second architecture, only the architecture-
    specific files need to be installed.

$ VMI$CALLBACK ASK mx_ok "Do you wish to install only ''mx_system_type'-specific files" "YES" B
$ mx_install_exe_only = mx_ok
$ IF mx_install_exe_only THEN DEFINE MX_RUN_MXCONFIG "NONE"
$!
$Skip_ExeOnly_Check:
$ VMI$CALLBACK SET PURGE ASK
$!
$ MX_OPT_NAMES = "?" +-
    	":Base MX software"+-
    	":SMTP interface support"+-
    	":SMTP-over-DECnet support"+-
	":Site-provided interface support"+-
	":Mailing List/File Server support"
$ MX_OPTS = "?:MX_DO_BASE:MX_DO_SMTP:MX_DO_DNSMTP:MX_DO_SITE:MX_DO_MLF"
$!
$ IF mx_install_exe_only
$ THEN
$   MX_DO_DOC = " "
$   MX_DO_EXAMPLES = " "
$ ELSE
$   MX_OPT_NAMES = MX_OPT_NAMES +-
	":Documentation"+-
	":Example files and programs"
$   MX_OPTS = MX_OPTS + ":MX_DO_DOC:MX_DO_EXAMPLES"
$ ENDIF
$!
$ MX_I = 0
$MX_INIT_LOOP:
$ MX_I = MX_I + 1
$ MX_OPT = F$ELEMENT (MX_I,":",MX_OPTS)
$ IF MX_OPT .EQS. ":" THEN GOTO MX_END_INIT_LOOP
$ 'MX_OPT = " "
$ GOTO MX_INIT_LOOP
$!
$MX_END_INIT_LOOP:
$!
$ IF .NOT. mx_reinstalling THEN GOSUB Find_Agents_To_Update
$!
$MX_SELECT_MENU:
$ TYPE SYS$INPUT:

                          Component Selection

    Select the MX components you wish to install from the menu below.
    An asterisk appears next to the packages that have already been
    selected.  You can remove a package from the list by selecting it
    again.  You may enter more than one selection by separating your
    choices with commas.

$ MX_M = 0
$MX_SEL_LOOP:
$ MX_M = MX_M + 1
$ MX_N = F$ELEMENT (MX_M,":",MX_OPT_NAMES)
$ IF MX_N .EQS. ":" THEN GOTO MX_SELECT_ASK
$ MX_T = F$ELEMENT (MX_M,":",MX_OPTS)
$ IF MX_T .EQS. "MX_DO_BASE" .AND. .NOT. mx_reinstalling THEN MX_N = MX_N + " (REQUIRED)"
$ MX_SAY F$FAO ("    !2UL. [!AS] !AS", MX_M, 'MX_T, MX_N)
$ GOTO MX_SEL_LOOP
$MX_SELECT_ASK:
$ MX_SAY ""
$ MX_SAY F$FAO ("    !2UL.     Exit", MX_M)
$ MX_SAY ""
$ MX_SAY ""
$!
$   VMI$CALLBACK ASK MX_CHOICE_INPUT -
    	"      Your choice" "''MX_M'"
$ MX_I = -1
$MX_PARSE_LOOP:
$ MX_I = MX_I + 1
$ MX_CHOICE = F$ELEMENT (MX_I, ",", MX_CHOICE_INPUT)
$ IF MX_CHOICE .EQS. "," THEN GOTO MX_SELECT_MENU
$ MX_CHOICE = F$INTEGER (MX_CHOICE)
$ IF MX_CHOICE .EQ. MX_M THEN GOTO MX_CONFIRM
$ IF MX_CHOICE .LT. 1 .OR. MX_CHOICE .GT. MX_M
$ THEN
$   VMI$CALLBACK MESSAGE E BADCHOICE -
    	"Choice ''MX_CHOICE' invalid; choices range from 1 to ''MX_M'."
$ ELSE
$   MX_T = F$ELEMENT (MX_CHOICE, ":", MX_OPTS)
$   IF MX_T .EQS. "MX_DO_BASE"
$   THEN
$   	IF 'MX_T .EQS. "*" .AND. .NOT. mx_reinstalling
$   	THEN
$   	    VMI$CALLBACK MESSAGE E MUSTDOBASE "You MUST install the ''MX_INSTALLING_VERSION' base software."
$   	    GOTO MX_PARSE_LOOP
$   	ENDIF
$   ENDIF
$   IF 'MX_T .EQS. "*"
$   THEN
$   	'MX_T = " "
$   ELSE
$   	'MX_T = "*"
$   ENDIF
$ ENDIF
$ GOTO MX_PARSE_LOOP
$!
$MX_CONFIRM:
$ MX_SAY ""
$ MX_SAY "    You have selected the following components:"
$ MX_SAY ""
$ MX_CNT = 0
$ MX_M = 0
$MX_CONF_LOOP:
$ MX_M = MX_M + 1
$ MX_N = F$ELEMENT (MX_M,":",MX_OPT_NAMES)
$ IF MX_N .EQS. ":" THEN GOTO MX_CONF_ASK
$ MX_T = F$ELEMENT (MX_M,":",MX_OPTS)
$ IF 'MX_T .EQS. "*"
$ THEN
$   MX_PRI = MX_M
$   MX_SAY "        ",MX_N
$   MX_CNT = MX_CNT + 1
$ ENDIF
$ GOTO MX_CONF_LOOP
$!
$MX_CONF_ASK:
$ IF MX_CNT .EQ. 0 THEN MX_SAY "        (None)"
$ MX_SAY ""
$ MX_SAY ""
$ VMI$CALLBACK ASK MX_OK "Is this correct" "YES" B
$ IF .NOT. MX_OK THEN GOTO MX_SELECT_MENU
$!
$ mx_exe = mx_root + "." + mx_exe_dir + "]"
$ DEFINE MX_SHR 'MX_EXE'MX_SHR
$ DEFINE MX_MSG 'MX_EXE'MX_MSG
$ DEFINE MX_SMTP_MSG 'MX_EXE'MX_SMTP_MSG
$ DEFINE MX_FLQ_SHR 'MX_EXE'MX_FLQ_SHR
$!
$!--- no more questions after this point ---
$!
$ TYPE SYS$INPUT:

    The installation will continue for another 5 to 45 minutes,
    depending on your CPU type, distribution media, etc.  No
    further input is required during the installation.  Once
    installation has completed, you may be asked additional
    configuration questions.

$!
$ IF MX_DO_BASE .NES. "*" THEN GOTO Skip_Do_Base
$ 
$ VMI$CALLBACK RESTORE_SAVESET 'base_saveset'
$ VMI$CALLBACK MESSAGE I INSTALL_BASE "Installing the MX base software..."
$ IF F$SEARCH("''MX_ROOT']ROUTER.DIR").eqs."" then -
	VMI$CALLBACK CREATE_DIRECTORY USER 'MX_ROOT'.ROUTER] -
    		"/OWNER=[1,4]/PROT=(S:RWE,O:RWE,G:RE,W:E)"
$ IF F$SEARCH("''MX_ROOT']LOCAL.DIR").eqs."" then -
	VMI$CALLBACK CREATE_DIRECTORY USER 'MX_ROOT'.LOCAL] -
    		"/OWNER=[1,4]/PROT=(S:RWE,O:RWE,G:RE,W:E)"
$!
$ @VMI$KWD:MX_BASE_FIXUPS_'mx_arch'.COM
$ VMI$CALLBACK PROVIDE_IMAGE "" MX_BASE_IMAGES_'mx_arch'.TXT "" T
$ VMI$CALLBACK PROVIDE_FILE MX_TMP MX_ROUTER.COM 'MX_EXE'
$ VMI$CALLBACK PROVIDE_FILE MX_TMP MX_LOCAL.COM 'MX_EXE'
$ VMI$CALLBACK PROVIDE_FILE MX_TMP MX_FLQ_MGR.COM 'MX_EXE'
$ IF .NOT. mx_install_exe_only THEN installed_ARCH = ""
$!
$!  Remove obsolete files.
$!
$ IF f$search("''MX_EXE'FLQU.EXE").nes."" then -	!Get rid of obsolete
	VMI$CALLBACK DELETE_FILE 'MX_EXE'FLQU.EXE	!... FLQU utility
$ IF f$search("''MX_EXE'FLQ_SHR.EXE").nes."" then -	!Get rid of obsolete
	VMI$CALLBACK DELETE_FILE 'MX_EXE'FLQ_SHR.EXE	!... FLQ_SHR image
$ IF f$search("''MX_EXE'COMMON.OLB").nes."" then -	!Get rid of obsolete
	VMI$CALLBACK DELETE_FILE 'MX_EXE'COMMON.OLB	!... COMMON.OLB
$ IF f$search("''MX_EXE'MX_REBUILD_QUEUE.EXE").nes."" then -	!Get rid of obsolete
	VMI$CALLBACK DELETE_FILE 'MX_EXE'MX_REBUILD_QUEUE.EXE	!... MX_REBUILD_QUEUE.EXE
$!
$ CREATE VMI$KWD:MX_STARTUP.COM
$ OPEN/APPEND MX_STMP VMI$KWD:MX_STARTUP.COM
$ WRITE mx_stmp F$FAO ("$!! MX_STARTUP.COM, created during installation of !AS", mx_installing_version)
$ WRITE mx_stmp F$FAO ("$!!    at !%D by user !AS on node !AS", 0, -
    	    	    	F$EDIT (F$GETJPI ("", "USERNAME"), "TRIM"), F$GETSYI ("SCSNODE"))
$ WRITE MX_STMP "$ @''MX_EXE'MX___STARTUP 'P1 'P2 'P3 'P4 'P5 'P6 'P7 'P8"
$ WRITE mx_stmp "$ EXIT"
$ CLOSE MX_STMP
$ VMI$CALLBACK PROVIDE_FILE MX_OK MX_STARTUP.COM VMI$ROOT:[SYS$STARTUP]
$ DEFINE MX_SHR VMI$KWD:MX_SHR
$ DEFINE MX_FLQ_SHR VMI$KWD:MX_FLQ_SHR
$ DEFINE MX_MSG VMI$KWD:MX_MSG
$ DEFINE MX_SMTP_MSG VMI$KWD:MX_SMTP_MSG
$!
$ VMI$CALLBACK FIND_FILE MX_TMP 'MX_ROOT']MX_INSTALL_INFO.DAT "" S MX_FOUND
$ IF MX_FOUND .NES. "" THEN VMI$CALLBACK DELETE_FILE 'MX_ROOT']MX_INSTALL_INFO.DAT
$ IF .NOT. mx_cluster THEN GOTO Skip_Install_Info
$ CREATE VMI$KWD:MX_INSTALL_INFO.DAT
$ OPEN/APPEND MX_T VMI$KWD:MX_INSTALL_INFO.DAT
$ WRITE MX_T "ARCH:[''mx_arch']''installed_ARCH'"
$ WRITE MX_T "VERS:''mx_installing_version'"
$ CLOSE MX_T
$ VMI$CALLBACK PROVIDE_FILE MX_OK MX_INSTALL_INFO.DAT 'MX_ROOT']
$Skip_Install_Info:
$ IF mx_install_exe_only THEN GOTO Skip_Do_Base
$!
$ VMI$CALLBACK PROVIDE_FILE "" MX_FILE_LIST.TXT "" T
$ IF F$SEARCH ("''MX_ROOT']MCP_HELPLIB.HLB") .NES. "" THEN -
	VMI$CALLBACK DELETE_FILE 'MX_ROOT']MCP_HELPLIB.HLB
$!
$ IF mx_upgrading
$ THEN
$   IF mx_major_version .LT. 5 .OR. -
    	(mx_major_version .EQ. 5 .AND. mx_minor_version .LT. 2)
$   THEN
$   	IF F$TRNLNM ("MX_LOCAL_ALLOW_LONG_LINES") .NES. "" .OR. -
       	   F$TRNLNM ("MX_LOCAL_EXDISKQUOTA_FATAL") .NES. "" .OR. -
       	   F$TRNLNM ("MX_LOCAL_FORWARD_NO_RESENT") .NES. ""
$   	THEN
$   	    CREATE VMI$KWD:UPGRADE_CONFIG.MCP
$   	    OPEN/APPEND mx_u VMI$KWD:UPGRADE_CONFIG.MCP
$   	    IF F$TRNLNM ("MX_LOCAL_ALLOW_LONG_LINES") .NES. "" THEN -
    	    	WRITE mx_u "SET LOCAL/LONG_LINES"
$   	    IF F$TRNLNM ("MX_LOCAL_EXDISKQUOTA_FATAL") .NES. "" THEN -
    	    	WRITE mx_u "SET LOCAL/DISABLE_EXQUOTA=FATAL"
$   	    IF F$TRNLNM ("MX_LOCAL_FORWARD_NO_RESENT") .NES. "" THEN -
    	    	WRITE mx_u "SET LOCAL/OMIT_RESENT_HEADERS"
$   	    CLOSE mx_u
$   	ENDIF
$   ENDIF
$ ENDIF
$!
$Skip_Do_Base:
$!
$!  Begin the SMTP support installation.
$!
$ IF MX_DO_SMTP .EQS. "*" .OR. MX_DO_DNSMTP .EQS. "*" THEN -
    	VMI$CALLBACK RESTORE_SAVESET 'smtp_saveset'
$!
$ IF MX_DO_SMTP .NES. "*" THEN GOTO Skip_Do_SMTP
$ 
$ VMI$CALLBACK MESSAGE I INSTALL_SMTP "Installing SMTP support..."
$!
$ IF F$SEARCH("''MX_ROOT']SMTP.DIR").eqs."" then -
	VMI$CALLBACK CREATE_DIRECTORY USER 'MX_ROOT'.SMTP] -
    		"/OWNER=[1,4]/PROT=(S:RWE,O:RWE,G:RE,W)"
$ IF F$SEARCH("''MX_ROOT'.SMTP]LOCK.DIR").eqs."" then -
	VMI$CALLBACK CREATE_DIRECTORY USER 'MX_ROOT'.SMTP.LOCK] -
    		"/OWNER=[1,4]/PROT=(S:RWE,O:RWE,G:RE,W)"
$!
$ @VMI$KWD:MX_SMTP_FIXUPS_'mx_arch'.COM
$ VMI$CALLBACK PROVIDE_IMAGE "" MX_SMTP_IMAGES_'mx_arch'.TXT "" T
$ VMI$CALLBACK PROVIDE_FILE MX_TMP MX_SMTP.COM 'MX_EXE'
$ VMI$CALLBACK PROVIDE_FILE MX_TMP SMTP_SERVER.COM 'MX_EXE'
$ VMI$CALLBACK PROVIDE_FILE MX_TMP PROCESS_HOLDING_QUEUE.COM 'MX_EXE'
$!
$Skip_Do_SMTP:
$ IF mx_upgrading .AND. MX_DO_SMTP .NES. "*"
$ THEN
$   VMI$CALLBACK FIND_FILE MX_TMP 'MX_EXE'MX_SMTP.EXE "" S MX_FOUND
$   IF MX_FOUND .NES. "" THEN VMI$CALLBACK DELETE_FILE 'MX_EXE'MX_SMTP.EXE
$   VMI$CALLBACK FIND_FILE MX_TMP 'MX_EXE'MX_SMTP.COM "" S MX_FOUND
$   IF MX_FOUND .NES. "" THEN VMI$CALLBACK DELETE_FILE 'MX_EXE'MX_SMTP.COM
$   VMI$CALLBACK FIND_FILE MX_TMP 'MX_EXE'PROCESS_HOLDING_QUEUE.COM "" S MX_FOUND
$   IF MX_FOUND .NES. "" THEN VMI$CALLBACK DELETE_FILE 'MX_EXE'PROCESS_HOLDING_QUEUE.COM
$   VMI$CALLBACK FIND_FILE MX_TMP 'MX_EXE'SMTP_SERVER.EXE "" S MX_FOUND
$   IF MX_FOUND .NES. "" THEN VMI$CALLBACK DELETE_FILE 'MX_EXE'SMTP_SERVER.EXE
$   VMI$CALLBACK FIND_FILE MX_TMP 'MX_EXE'SMTP_SERVER.COM "" S MX_FOUND
$   IF MX_FOUND .NES. "" THEN VMI$CALLBACK DELETE_FILE 'MX_EXE'SMTP_SERVER.COM
$   VMI$CALLBACK FIND_FILE MX_TMP 'MX_EXE'DOMAIN_EXPANSION.EXE "" S MX_FOUND
$   IF MX_FOUND .NES. "" THEN VMI$CALLBACK DELETE_FILE 'MX_EXE'DOMAIN_EXPANSION.EXE
$ ENDIF
$!
$ IF MX_DO_DNSMTP .NES. "*" THEN GOTO Skip_Do_DNSMTP
$ 
$ VMI$CALLBACK MESSAGE I INSTALL_DNSMTP "Installing SMTP-over-DECnet support..."
$!
$ IF F$SEARCH("''MX_ROOT']DNSMTP.DIR").eqs."" then -
	VMI$CALLBACK CREATE_DIRECTORY USER 'MX_ROOT'.DNSMTP] -
    		"/OWNER=[1,4]/PROT=(S:RWE,O:RWE,G:RE,W)"
$!
$ @VMI$KWD:MX_DNSMTP_FIXUPS_'mx_arch'.COM
$ VMI$CALLBACK PROVIDE_IMAGE "" MX_DNSMTP_IMAGES_'mx_arch'.TXT "" T
$ VMI$CALLBACK PROVIDE_FILE MX_TMP MX_DNSMTP.COM 'MX_EXE'
$!
$   TYPE SYS$INPUT:

    Please refer to the Installation Guide for information on setting
    up a DECnet object for SMTP-over-DECnet.

$!
$Skip_Do_DNSMTP:
$ IF MX_UPGRADING .AND. MX_DO_DNSMTP .NES. "*"
$ THEN
$   VMI$CALLBACK FIND_FILE MX_TMP 'MX_EXE'MX_DNSMTP.EXE "" S MX_FOUND
$   IF MX_FOUND .NES. "" THEN VMI$CALLBACK DELETE_FILE 'MX_EXE'MX_DNSMTP.EXE
$   VMI$CALLBACK FIND_FILE MX_TMP 'MX_EXE'MX_DNSMTP.COM "" S MX_FOUND
$   IF MX_FOUND .NES. "" THEN VMI$CALLBACK DELETE_FILE 'MX_EXE'MX_DNSMTP.COM
$   VMI$CALLBACK FIND_FILE MX_TMP 'MX_EXE'DNSMTP_SERVER.EXE "" S MX_FOUND
$   IF MX_FOUND .NES. "" THEN VMI$CALLBACK DELETE_FILE 'MX_EXE'DNSMTP_SERVER.EXE
$ ENDIF
$!
$ IF MX_DO_SITE .EQS. "*".OR. MX_DO_MLF .EQS. "*" THEN -
    	VMI$CALLBACK RESTORE_SAVESET 'other_saveset'
$!
$ IF MX_DO_SITE .NES. "*" THEN GOTO Skip_Do_Site
$ VMI$CALLBACK MESSAGE I INSTALL_SITE "Installing SITE agent support..."
$!
$ IF F$SEARCH("''MX_ROOT']SITE.DIR").eqs."" then -
	VMI$CALLBACK CREATE_DIRECTORY USER 'MX_ROOT'.SITE] -
    		"/OWNER=[1,4]/PROT=(S:RWE,O:RWE,G:RE,W)"
$!
$ @VMI$KWD:MX_SITE_FIXUPS_'mx_arch'.COM
$ VMI$CALLBACK PROVIDE_IMAGE "" MX_SITE_IMAGES_'mx_arch'.TXT "" T
$ VMI$CALLBACK PROVIDE_FILE MX_TMP MX_SITE.COM 'MX_EXE'
$Skip_Do_SITE:
$ IF MX_UPGRADING .AND. MX_DO_SITE .NES. "*"
$ THEN
$   VMI$CALLBACK FIND_FILE MX_TMP 'MX_EXE'MX_SITE.EXE "" S MX_FOUND
$   IF MX_FOUND .NES. "" THEN VMI$CALLBACK DELETE_FILE 'MX_EXE'MX_SITE.EXE
$   VMI$CALLBACK FIND_FILE MX_TMP 'MX_EXE'MX_SITE.COM "" S MX_FOUND
$   IF MX_FOUND .NES. "" THEN VMI$CALLBACK DELETE_FILE 'MX_EXE'MX_SITE.COM
$   VMI$CALLBACK FIND_FILE MX_TMP 'MX_EXE'MX_SITE_IN.EXE "" S MX_FOUND
$   IF MX_FOUND .NES. "" THEN VMI$CALLBACK DELETE_FILE 'MX_EXE'MX_SITE_IN.EXE
$ ENDIF
$!
$ IF MX_DO_MLF .NES. "*" THEN GOTO Skip_Do_MLF
$ IF .NOT. mx_install_exe_only THEN VMI$CALLBACK RESTORE_SAVESET K
$ VMI$CALLBACK MESSAGE I INSTALL_MLF "Installing mailing list/file server support..."
$!
$ IF F$SEARCH("''MX_ROOT']MLF.DIR").eqs."" then -
	VMI$CALLBACK CREATE_DIRECTORY USER 'MX_ROOT'.MLF] -
    		"/OWNER=[1,4]/PROT=(S:RWE,O:RWE,G:RE,W)"
$ IF F$SEARCH("''MX_ROOT'.MLF]MAILING_LISTS.DIR").eqs."" then -
	VMI$CALLBACK CREATE_DIRECTORY USER 'MX_ROOT'.MLF.MAILING_LISTS] -
	    	"/OWNER=[1,4]/PROT=(S:RWE,O:RWE,G:RE,W)"
$!
$ @VMI$KWD:MX_MLF_FIXUPS_'mx_arch'.COM
$ VMI$CALLBACK PROVIDE_IMAGE "" MX_MLF_IMAGES_'mx_arch'.TXT "" T
$ VMI$CALLBACK PROVIDE_FILE MX_TMP MX_MLF.COM 'MX_EXE'
$ IF mx_install_exe_only THEN GOTO Skip_Do_MLF
$!
$ @VMI$KWD:MX_MLF_FILE_FIXUPS.COM
$ VMI$CALLBACK PROVIDE_FILE "" MX_MLIST_LIST.TXT "" T
$ MX_1 = MX_ROOT + ".LOCAL.MLIST]"
$ MX_2 = MX_ROOT + ".MLF.MAILING_LISTS]"
$ MX_FILES = "MLIST_ADD_MESSAGE.TXT,MLIST_REMOVE_MESSAGE.TXT,"+-
    	    	"MLIST_FORWARD_MESSAGE.TXT,MLIST_HELP.TXT,"+-
                "MLIST_CONFIRM_MESSAGE.TXT"
$ MX_I = -1
$MX_MLF_LOOP_2:
$ MX_I = MX_I + 1
$ MX_FILE = F$ELEMENT (MX_I,",",MX_FILES)
$ IF MX_FILE .EQS. "," THEN GOTO MX_END_MLF_LOOP_2
$ IF F$SEARCH (MX_2+MX_FILE) .NES. "" THEN GOTO MX_MLF_LOOP_2
$ IF F$SEARCH (MX_1+MX_FILE) .NES. ""
$ THEN
$   RENAME 'MX_1''MX_FILE' 'MX_2';
$   GOTO MX_MLF_LOOP_2
$ ENDIF
$ VMI$CALLBACK PROVIDE_FILE MX_TMP 'MX_FILE 'MX_2
$ GOTO MX_MLF_LOOP_2
$MX_END_MLF_LOOP_2:
$ IF F$SEARCH ("''MX_ROOT'.MLF]FILESERV_HELP.TXT") .EQS. "" THEN -
    	    VMI$CALLBACK PROVIDE_FILE MX_TMP FILESERV_HELP.TXT 'MX_ROOT'.MLF]
$ VMI$CALLBACK PROVIDE_FILE MX_TMP MLF_CONFIG.COM 'MX_ROOT']
$!
$Skip_Do_MLF:
$ IF MX_UPGRADING .AND. MX_DO_MLF .NES. "*"
$ THEN
$   VMI$CALLBACK FIND_FILE MX_TMP 'MX_EXE'MX_MLF.EXE "" S MX_FOUND
$   IF MX_FOUND .NES. "" THEN VMI$CALLBACK DELETE_FILE 'MX_EXE'MX_MLF.EXE
$   VMI$CALLBACK FIND_FILE MX_TMP 'MX_EXE'MX_MLF.COM "" S MX_FOUND
$   IF MX_FOUND .NES. "" THEN VMI$CALLBACK DELETE_FILE 'MX_EXE'MX_MLF.COM
$   VMI$CALLBACK FIND_FILE MX_TMP 'MX_ROOT']MLF_CONFIG.COM "" S MX_FOUND
$   IF MX_FOUND .NES. "" THEN VMI$CALLBACK DELETE_FILE 'MX_ROOT']MLF_CONFIG.COM
$ ENDIF
$!
$ IF MX_DO_DOC .EQS. "*"
$ THEN
$   VMI$CALLBACK RESTORE_SAVESET L
$   VMI$CALLBACK MESSAGE I INSTALL_DOC "Installing documentation files..."
$   IF F$SEARCH("''MX_ROOT']DOC.DIR").eqs."" then -
	VMI$CALLBACK CREATE_DIRECTORY USER 'MX_ROOT'.DOC] -
    		"/OWNER=[1,4]/PROT=(S:RWE,O:RWE,G:RE,W:RE)"
$   VMI$CALLBACK PROVIDE_FILE "" MX_DOC_LIST.TXT "" T
$   IF f$search("''MX_ROOT'.DOC]INSTALL_GUIDE.*").nes."" then -
	VMI$CALLBACK DELETE_FILE 'MX_ROOT'.DOC]INSTALL_GUIDE.*
$   IF f$search("''MX_ROOT'.DOC]MGMT_GUIDE.*").nes."" then -
	VMI$CALLBACK DELETE_FILE 'MX_ROOT'.DOC]MGMT_GUIDE.*
$   IF f$search("''MX_ROOT'.DOC]MLF_GUIDE.*").nes."" then -
	VMI$CALLBACK DELETE_FILE 'MX_ROOT'.DOC]MLF_GUIDE.*
$   IF f$search("''MX_ROOT'.DOC]USER_GUIDE.*").nes."" then -
	VMI$CALLBACK DELETE_FILE 'MX_ROOT'.DOC]USER_GUIDE.*
$   IF f$search("''MX_ROOT'.DOC]PROG_GUIDE.*").nes."" then -
	VMI$CALLBACK DELETE_FILE 'MX_ROOT'.DOC]PROG_GUIDE.*
$   IF F$SEARCH("''MX_ROOT'.DOC]MESSAGE_PATHS.GIF") .NES. "" THEN -
    	VMI$CALLBACK DELETE_FILE 'MX_ROOT'.DOC]MESSAGE_PATHS.GIF
$   IF F$SEARCH("''MX_ROOT'.DOC]LIBRARY.DECW$BOOKSHELF") .NES. "" THEN -
    	VMI$CALLBACK DELETE_FILE 'MX_ROOT'.DOC]LIBRARY.DECW$BOOKSHELF
$   IF F$SEARCH("''MX_ROOT'.DOC]MX.DECW$BOOKSHELF") .NES. "" THEN -
    	VMI$CALLBACK DELETE_FILE 'MX_ROOT'.DOC]MX.DECW$BOOKSHELF
$ ENDIF
$!
$!
$ IF MX_DO_EXAMPLES .EQS. "*"
$ THEN
$   VMI$CALLBACK RESTORE_SAVESET M
$   VMI$CALLBACK MESSAGE I INSTALL_EXAMPLES "Installing examples files and programs..."
$   IF F$SEARCH("''MX_ROOT']EXAMPLES.DIR").eqs."" then -
	VMI$CALLBACK CREATE_DIRECTORY USER 'MX_ROOT'.EXAMPLES] -
    		"/OWNER=[1,4]/PROT=(S:RWE,O:RWE,G:RE,W:RE)"
$   VMI$CALLBACK PROVIDE_FILE "" MX_EXAMPLES_LIST.TXT "" T
$ ENDIF
$!
$ IF .NOT. mx_install_exe_only
$ THEN
$   file = f$search("''MX_ROOT']MX_LOGICALS.DAT")
$   IF file .NES. ""
$   THEN
$   	COPY 'file' VMI$KWD:MX_LOGICALS.OLD
$   	GOSUB Inherit_Logicals
$   ELSE
$   	CREATE VMI$KWD:MX_LOGICALS.DAT
$   ENDIF
$   VMI$CALLBACK PROVIDE_FILE MX_OK MX_LOGICALS.DAT 'MX_ROOT'] K
$!
$!  The SPAMFILTER example module removed as of V5.1
$!
$   VMI$CALLBACK FIND_FILE MX_TMP 'MX_ROOT']EXAMPLES.DIR "" S MX_FOUND
$   IF MX_FOUND .NES. "" THEN VMI$CALLBACK FIND_FILE MX_TMP 'MX_ROOT'.EXAMPLES]SPAMFILTER.ZIP "" S MX_FOUND
$   IF MX_FOUND .NES. "" THEN VMI$CALLBACK DELETE_FILE 'MX_ROOT'.EXAMPLES]SPAMFILTER.ZIP
$ ENDIF
$!
$ mx_say ""
$ mx_say "    MX installation procedure complete."
$ mx_say ""
$ IF MX_DO_BASE .EQS. "*"
$ THEN
$   TYPE SYS$INPUT:
    Be sure to follow the post-installation instructions described in
    the MX Installation Guide.  This will minimally include editing
    the system startup procedure to include the following command:

               $ @SYS$STARTUP:MX_STARTUP

$ ENDIF
$ IF F$TRNLNM ("MX_FLQ_SHR","LNM$PROCESS") .NES. "" THEN DEASSIGN MX_FLQ_SHR
$ IF F$TRNLNM ("MX_SHR","LNM$PROCESS") .NES. "" THEN DEASSIGN MX_SHR
$ IF F$TRNLNM ("MX_MSG","LNM$PROCESS") .NES. "" THEN DEASSIGN MX_MSG
$ IF F$TRNLNM ("MX_SMTP_MSG","LNM$PROCESS") .NES. "" THEN DEASSIGN MX_SMTP_MSG
$ EXIT VMI$_SUCCESS
$!
$!
$Find_Agents_To_Update:
$!
$ mx_do_base = "*"
$ mx_i = 0
$ mx_tmp = mx_root - "]"
$Find_Agents_Loop:
$ mx_i = mx_i + 1
$ mx_opt = F$ELEMENT (mx_i, ":", mx_opts)
$ IF mx_opt .EQS. ":" THEN GOTO End_Find_Agents_Loop
$ IF mx_opt .EQS. "MX_DO_DOC"
$ THEN 'mx_opt' = "*"
$ ELSE IF mx_opt .EQS. "MX_DO_EXAMPLES" .AND. F$PARSE ("''mx_tmp'.EXAMPLES]") .NES. ""
$   	    THEN 'mx_opt' = "*"
$   	    ELSE
$   	    	mx_tmp1 = mx_opt - "_DO" + ".EXE"  ! MX_DO_blah -> MX_blah.EXE
$   	    	IF F$SEARCH ("''mx_tmp'.''mx_exe_dir']''mx_tmp1'") .NES. "" THEN 'mx_opt' = "*"
$   	    ENDIF
$ ENDIF
$ GOTO Find_Agents_Loop
$!
$End_Find_Agents_Loop:
$ RETURN
$!
$Get_Install_Info:
$ Installed_ARCH = ""
$ Installed_VERS = ""
$ VMI$CALLBACK FIND_FILE MX_TMP 'MX_ROOT']MX_INSTALL_INFO.DAT "" S MX_FOUND
$ IF MX_FOUND .EQS. "" THEN RETURN
$ OPEN/READ MX_T 'MX_ROOT']MX_INSTALL_INFO.DAT
$GII_Loop:
$ READ/END=GII_Close/ERROR=GII_Close MX_T mx_tmp
$ i = F$LOCATE (":", mx_tmp)
$ IF i .GT. F$LENGTH (mx_tmp) THEN GOTO GII_Loop
$ mx_tmp1 = F$EDIT (F$EXTRACT (0, i, mx_tmp), "TRIM,UPCASE")
$ IF mx_tmp1 .NES. "ARCH" .AND. mx_tmp1 .NES. "VERS" THEN GOTO GII_Loop
$ Installed_'mx_tmp1' = F$EDIT (F$EXTRACT (i+1, -1, mx_tmp), "TRIM")
$ GOTO GII_Loop
$GII_Close:
$ CLOSE MX_T
$ RETURN
$!
$Inherit_Logicals:
$ if f$type(mx_flq_dir).eqs."" then mx_flq_dir = f$trnlnm("MX_FLQ_DIR")
$ OPEN/READ MX_oldlog VMI$KWD:MX_LOGICALS.OLD
$ CREATE VMI$KWD:MX_LOGICALS.DAT
$ OPEN/APPEND MX_logi VMI$KWD:MX_LOGICALS.DAT
$Inherit_loop:
$ READ/END=End_Inherit mx_oldlog mx_tmp
$ mx_test = F$EDIT(F$ELEMENT(0, "\", mx_tmp), "TRIM,UPCASE")
$ IF mx_test .EQS. "MX_FLQ_DIR"
$ THEN
$   WRITE mx_logi "MX_FLQ_DIR\/SYSTEM/EXEC\", mx_flq_dir
$   GOTO Inherit_Loop
$ ENDIF
$ IF F$EXTRACT(0,3,mx_test) .EQS. "MX_" .AND. -
       F$EXTRACT(F$LENGTH(mx_test)-4, -1, mx_test) .EQS. "_DIR" THEN GOTO Inherit_Loop
$ IF mx_test .EQS. "MX_MCP_HELPLIB" .OR. mx_test .EQS. "MX_ALIAS_HELPLIB" THEN GOTO Inherit_Loop
$ WRITE mx_logi mx_tmp
$ GOTO Inherit_Loop
$End_Inherit:
$ CLOSE mx_logi
$ CLOSE mx_oldlog
$ RETURN
$!
$!  The post-installation commands
$!
$MX_POSTINSTALL:
$ IF P2 THEN SET VERIFY
$!
$! Define the MX logicals now so we can create the queue file.
$!
$ @SYS$STARTUP:MX_STARTUP.COM LOGICALS
$!
$!
$ install_type = F$TRNLNM("MX_RUN_MXCONFIG","LNM$PROCESS")
$ need_rejman_run = 0
$ IF install_type .EQS. "UPGRADE" THEN -
    need_rejman_run = F$SEARCH (F$PARSE("MX_REJECTION_DATBASE","MX_DIR:.MXCFG")) .NES. ""
$ IF install_type .NES. "NONE" THEN GOTO Do_Run_MXConfig
$ IF F$SEARCH (F$PARSE("MX_CONFIG","MX_DIR:.MXCFG")) .EQS. ""
$ THEN
$   install_type = "INSTALL"
$   GOTO Do_Run_MXConfig
$ ENDIF
$ GOTO Skip_Run_MXConfig
$Do_Run_MXConfig:
$ WRITE SYS$OUTPUT ""
$ IF install_type .EQS. "UPGRADE"
$ THEN
$   WRITE SYS$OUTPUT "    The MXCONFIG command procedure may now be run to assist in upgrading"
$   WRITE SYS$OUTPUT "    your MX configuration."
$ ELSE
$   WRITE SYS$OUTPUT "    The MXCONFIG command procedure may now be run to assist in creating"
$   WRITE SYS$OUTPUT "    an initial MX configuration."
$ ENDIF
$ WRITE SYS$OUTPUT ""
$ VMI$CALLBACK ASK mx_tmp "Would you like to run MXCONFIG now" "YES" B
$ IF .NOT. mx_tmp
$ THEN
$   IF install_type .EQS. "INSTALL"
$   THEN
$   	TYPE SYS$INPUT:

    You must execute the MXCONFIG procedure manually to configure MX before
    starting MX for the first time.

$   ENDIF
$   GOTO Skip_Run_MXConfig
$ ENDIF
$ SET NOON
$ @MX_DIR:MXCONFIG 'install_type
$ SET ON
$Skip_Run_MXConfig:
$ IF need_rejman_run
$ THEN
$   WRITE SYS$OUTPUT ""
$   WRITE SYS$OUTPUT "Upgrading your REJMAN rejection database from a prior version."
$   WRITE SYS$OUTPUT ""
$   DEFINE/USER SYS$INPUT _NLA0:
$   rejmanxxx := $MX_EXE:REJMAN
$   rejmanxxx SAVE
$ ENDIF
$ IF install_type .NES. "UPGRADE" THEN GOTO MX_Postinstall_Done
$ IF F$SEARCH ("VMI$KWD:UPGRADE_CONFIG.MCP") .EQS. "" THEN GOTO Skip_Upgrade_Config
$ TYPE SYS$INPUT:

    One or more obsolete logical names have been detected.  Your
    MX configuration will be upgraded with the following settings:

$ OPEN/READ mx_u VMI$KWD:UPGRADE_CONFIG.MCP
$UC_Loop:
$ READ/END=UC_Doit mx_u mx_tmp
$ say "   MCP> ", mx_tmp
$ GOTO UC_Loop
$UC_Doit:
$ say ""
$ CLOSE mx_u
$ OPEN/APPEND mx_u VMI$KWD:UPGRADE_CONFIG.MCP
$ WRITE mx_u "SAVE"
$ CLOSE mx_u
$ DEFINE/USER SYS$INPUT VMI$KWD:UPGRADE_CONFIG.MCP
$ RUN MX_EXE:MCP
$   TYPE SYS$INPUT:

    MX configuration updated.  Consult the Release Notes
    for information on the obsolete logical name definitions
    you may now remove from your system startup procedure.

$Skip_Upgrade_Config:
$!
$MX_Postinstall_Done:
$ EXIT VMI$_SUCCESS
