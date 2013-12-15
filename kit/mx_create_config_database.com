$ V = 'F$VERIFY(0)
$!
$! MXCONFIG.COM
$!
$!  Builds an initial MX configuration command file from some basic
$!  questions.
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
$! 23-JAN-1990	V1.0	Madison	    Initial coding.
$! 09-FEB-1990	V1.1	Madison	    Add UUCP support.
$! 19-MAR-1990	V1.2	Madison	    Improve things for non-networked systems.
$! 06-APR-1990	V1.2-1	Madison	    Add info about abbreviated host names.
$! 11-OCT-1990	V2.0	Madison	    Clean up a bit.  Don't stick SAVE in file.
$! 05-DEC-1990	V2.1	Madison	    Add SET JNET/NOPERCENT when needed.
$! 12-DEC-1990	V2.1-1	Madison	    Handle routed addresses a little better.
$! 15-FEB-1991	V2.2	Madison	    INTERBIT gateways prefer %-hacking.
$! 18-FEB-1991	V2.2-1	Madison	    Typo, logic error, sysuser change.
$! 24-OCT-1991	T2.4	Madison	    Use PATH/ROUTE instead of rewrite rules.
$! 11-NOV-1991	T2.4-1	Madison	    Use MX_STARTUP_INFO.
$! 14-NOV-1991  T2.4-2	Madison	    ROUTER now handles BITNET DOMAIN.NAMES file.
$! 20-NOV-1991	V3.0	Madison	    Some finishing touches.
$! 04-MAR-1992	X3.1	Madison	    Add non-TCP SMTP support.
$! 13-AUG-1998	V5.1	Madison	    Remove BITNET and UUCP gateways.
$! 25-NOV-2000	V5.2	Madison	    Remove Jnet support.
$! 03-Feb-2008  V6.0    Madison     Remove UU,X.25 support.
$!
$ SET := SET
$ SET SYMBOL/SCOPE=(NOLOCAL,NOGLOBAL)
$ say := WRITE SYS$OUTPUT
$ ask := READ SYS$COMMAND
$ mcpw := WRITE MCP
$ skip = "WRITE SYS$OUTPUT """""
$!
$ IF F$TRNLNM ("MX_DIR") .EQS. ""
$ THEN
$   TYPE SYS$INPUT:

    MXCONFIG uses logical name definitions for MX.  Please execute
    the following command before using MXCONFIG:

                      $ @SYS$STARTUP:MX_STARTUP LOGICALS

$   EXIT
$ ENDIF
$!
$ TYPE SYS$INPUT

    This procedure builds an MCP command file which will create an
    initial MX configuration database.

    First-time MX managers should use this command procedure as a starting
    point, then tailor the resulting MCP command file as needed.

    NOTE:  In the following questions, when asked for an ADDRESS, be
           sure to specify a full E-mail address, even if the address
           is local.
                       Example:  user@host.company.ORG

$!
$MCPFILE_ASK:
$ defans = "MX_DIR:CONFIG.MCP"
$ ask/prompt="* What do you want to call the command file? [''defans']: " -
    /END=MXCFG_EXIT MCPFILE
$ IF MCPFILE .EQS. "" THEN MCPFILE = defans
$ CREATE 'MCPFILE'
$ IF .NOT. $STATUS THEN GOTO MCPFILE_ASK
$ MCPFILE = F$SEARCH (MCPFILE)
$!
$ OPEN/APPEND MCP 'MCPFILE
$ mcpw "! ''MCPFILE'"
$ mcpw "! Created: ", F$TIME(), " by MX_CREATE_CONFIG_DATABASE "
$ mcpw "!"
$!
$! Build the tables for menu choices.
$!
$ MXC_OPT_NAMES = "?"
$ MXC_OPTS      = "?"
$ IF F$SEARCH("MX_ROOT:[*EXE]MX_SMTP.EXE") .NES. ""
$ THEN -
    MXC_OPT_NAMES = MXC_OPT_NAMES + ":SMTP over TCP/IP"
    MXC_OPTS      = MXC_OPTS      + ":MXC_DO_SMTP"
$ ENDIF
$ IF F$SEARCH("MX_ROOT:[*EXE]MX_DNSMTP.EXE") .NES. ""
$ THEN -
    MXC_OPT_NAMES = MXC_OPT_NAMES + ":SMTP over DECnet"
    MXC_OPTS      = MXC_OPTS      + ":MXC_DO_DNSMTP"
$ ENDIF
$ MXC_I = 0
$MXC_INIT_LOOP:
$ MXC_I = MXC_I + 1
$ MXC_OPT = F$ELEMENT (MXC_I,":",MXC_OPTS)
$ IF MXC_OPT .EQS. ":" THEN GOTO MXC_END_INIT_LOOP
$ 'MXC_OPT = " "
$ GOTO MXC_INIT_LOOP
$!
$MXC_END_INIT_LOOP:
$ OPEN/READ/ERROR=MXC_MENU STAR MX_DIR:MX_STARTUP_INFO.DAT
$MXC_STAR_LOOP:
$ READ/END=MXC_CLOSE_STAR/ERR=MXC_CLOSE_STAR STAR SREC
$ SPROC = F$EXTRACT (3,-1,F$ELEMENT (0,":",SREC))
$ IF F$TYPE(MXC_DO_'SPROC') .NES. "" THEN MXC_DO_'SPROC = "*"
$ GOTO MXC_STAR_LOOP
$MXC_CLOSE_STAR:
$ CLOSE STAR
$!
$MXC_MENU:
$ TYPE SYS$INPUT:

                           Delivery Path Selection

    Select the delivery paths you are using with MX from the menu
    below.  Selected items are marked with an asterisk ("*").  You
    can remove a delivery path from the list by selecting it again.
    You may enter more than one selection by separating your choices
    with commas.

$ MXC_M = 0
$MXC_SEL_LOOP:
$ MXC_M = MXC_M + 1
$ MXC_N = F$ELEMENT (MXC_M,":",MXC_OPT_NAMES)
$ IF MXC_N .EQS. ":" THEN GOTO MXC_SELECT_ASK
$ MXC_T = F$ELEMENT (MXC_M,":",MXC_OPTS)
$ say F$FAO ("    !2UL. [!AS] !AS", MXC_M, 'MXC_T, MXC_N)
$ GOTO MXC_SEL_LOOP
$MXC_SELECT_ASK:
$ say ""
$ say F$FAO ("    !2UL.     Exit", MXC_M)
$ say ""
$ say ""
$!
$ ask MXC_CHOICE_INPUT -
    	/PROMPT="*      Your choice [''MXC_M']: "
$ IF MXC_CHOICE_INPUT .EQS. "" THEN MXC_CHOICE_INPUT = MXC_M
$ MXC_I = -1
$MXC_PARSE_LOOP:
$ MXC_I = MXC_I + 1
$ MXC_CHOICE = F$ELEMENT (MXC_I, ",", MXC_CHOICE_INPUT)
$ IF MXC_CHOICE .EQS. "," THEN GOTO MXC_MENU
$ MXC_CHOICE = F$INTEGER (MXC_CHOICE)
$ IF MXC_CHOICE .EQ. MXC_M THEN GOTO MXC_CONFIRM
$ IF MXC_CHOICE .LT. 1 .OR. MXC_CHOICE .GT. MXC_M
$ THEN
$   SAY "%MXC-E-BADCHOICE, ",
    	"Choice ''MXC_CHOICE' invalid; choices range from 1 to ''MXC_M'."
$ ELSE
$   MXC_T = F$ELEMENT (MXC_CHOICE, ":", MXC_OPTS)
$   IF 'MXC_T .EQS. "*"
$   THEN
$   	'MXC_T = " "
$   ELSE
$   	'MXC_T = "*"
$   ENDIF
$ ENDIF
$ GOTO MXC_PARSE_LOOP
$!
$MXC_CONFIRM:
$ say ""
$ say "    You have selected the following delivery paths:"
$ say ""
$ MXC_CNT = 0
$ MXC_M = 0
$MXC_CONF_LOOP:
$ MXC_M = MXC_M + 1
$ MXC_N = F$ELEMENT (MXC_M,":",MXC_OPT_NAMES)
$ IF MXC_N .EQS. ":" THEN GOTO MXC_CONF_ASK
$ MXC_T = F$ELEMENT (MXC_M,":",MXC_OPTS)
$ IF 'MXC_T .EQS. "*"
$ THEN
$   MXC_PRI = MXC_M
$   say "        ",MXC_N
$   MXC_CNT = MXC_CNT + 1
$ ENDIF
$ GOTO MXC_CONF_LOOP
$!
$MXC_CONF_ASK:
$ IF MXC_CNT .EQ. 0 THEN say "        (None)"
$ say ""
$ say ""
$MXC_CONF_ASK_1:
$ ask/prompt="* Is this correct?  [Yes]: " MXC_OK
$ MXC_OK = F$EXTRACT (0,1,F$EDIT (MXC_OK, "UPCASE,TRIM,COMPRESS")+"Y")
$ IF MXC_OK .NES. "Y" .AND. MXC_OK .NES. "N"
$ THEN
$   say "Please answer YES or NO."
$   GOTO MXC_CONF_ASK_1
$ ENDIF
$ IF .NOT. MXC_OK THEN GOTO MXC_MENU
$!
$ USING_SMTP	= "''MXC_DO_SMTP'" .EQS. "*"
$ USING_DNSMTP	= "''MXC_DO_DNSMTP'" .EQS. "*"
$ DNSMTP_ROUTER = USING_DNSMTP .AND. USING_SMTP
$ USING_ANYTHING = USING_SMTP .OR. USING_DNSMTP 
$!
$ IF USING_ANYTHING THEN GOTO Done_NoNet
$ TYPE SYS$INPUT:

    Local-Only (no network connection) Information

    Enter one or more names you would like to use to identify the
    local system.  You must enter at least one system name.

    When done, just press RETURN to move on to the next question.
$!
$!
$NONET_ASKPRIMARY:
$ defnode = F$TRNLNM ("MX_NODE_NAME")
$ IF defnode .NES. "" THEN deflcl2 = " [" + defnode + "]"
$ skip
$ ask/prompt="* Enter the primary name for the local system''deflcl2': " -
    	nodename
$ IF nodename .EQS. "" .AND. defnode .NES. "" THEN nodename = defnode
$ IF nodename .EQS. ""
$ THEN
$   skip
$   say "You MUST enter a primary node name."
$   GOTO NONET_ASKPRIMARY
$ ENDIF
$ mcpw "DEFINE PATH ""''nodename'"" LOCAL"
$ a = "an"
$NONET_LCLNAME_LOOP:
$ skip
$ ask/prompt="* Enter ''a' alternate name for the local system: " nodename
$ IF nodename .EQS. "" THEN GOTO DONE_NONET
$ mcpw "DEFINE PATH ""''nodename'"" LOCAL"
$ a = "another"
$ GOTO NONET_LCLNAME_LOOP
$!
$DONE_NoNet:
$!
$ IF .NOT. USING_SMTP THEN GOTO Done_SMTP
$ TYPE SYS$INPUT:

    TCP/IP (SMTP) Information

    MX must be configured to recognize all possible names that could
    be used by other hosts on the network to identify the local system:

        Examples:   myhost.mycompany.COM
                    myhost                (abbreviation)

    It must also be able to recognize the bracketed, dotted-decimal
    IP address for the local system:

        Example:    [128.113.5.15]


    In a VAXcluster with multiple TCP/IP-connected hosts you should enter
    the node name(s) and bracketed address for each host connected to the
    TCP/IP network.

    When are finished entering node names, just press RETURN to move
    on to the next question.

$!
$ a = "a"
$SMTP_LCLNAME_LOOP:
$ skip
$ ask/prompt="* Enter ''a' local Internet node name: " nodename
$ IF nodename .EQS. "" THEN GOTO SMTP_GW_CHECK
$ mcpw "DEFINE PATH ""''nodename'"" LOCAL"
$ a = "another"
$ GOTO SMTP_LCLNAME_LOOP
$!
$SMTP_GW_CHECK:
$!
$DONE_SMTP:
$!
$ IF .NOT. USING_DNSMTP THEN GOTO SKIP_DNSMTP
$ xx = "(End Node)"
$ IF DNSMTP_ROUTER THEN xx = "(Routing Node)
$ SAY ""
$ SAY "    SMTP-over-DECnet Information ''xx'"
$ TYPE SYS$INPUT:

    Enter all of the SMTP-over-DECnet node names that could be used
    in addressing the local system.  Nodenames for SMTP-over-DECnet
    are arbitrarily selected by the network manager, but should in
    general correspond to a system's DECnet node name.  They should
    have a recognizable suffix appended, such as "nodename.DNET"
    or "nodename.DNET.rest-of-domain", such as MNYJRS.DNET or
    BIGBOOTE.DNET.YOYODYNE.COM, to make it easier for both mailers
    and users to determine the source of a message.

    When done, just press RETURN to move on to the next question.
$!
$ a = "a"
$DNSMTP_LCLNAME_LOOP:
$ skip
$ DID1 := FALSE
$ ask/prompt="* Enter ''a' local DECNET_SMTP node name: " nodename
$ IF nodename .EQS. "" THEN GOTO DNSMTP_ROUTER_CHECK
$ mcpw "DEFINE PATH ""''nodename'"" LOCAL"
$!
$ a = "another"
$ GOTO DNSMTP_LCLNAME_LOOP
$DNSMTP_ROUTER_CHECK:
$ TYPE SYS$INPUT:

    SMTP-over-DECnet Routing Information

    Enter the host names for all of your DECnet-connected nodes which
    will be using SMTP-over-DECnet.

    Instead of specifying each DECnet-connected host individually,
    you may use a wildcard, such as *.DNET.YOYODYNE.COM.  If you
    use a wildcard, the first segment of the host name (up to the
    first dot) MUST match the remote host's DECnet node name.

    When done, just press RETURN to move on to the next question.
$ a = "a"
$DNSMTP_REMOTE_LOOP:
$ skip
$ DID1 := FALSE
$ ask/prompt="* Enter ''a' remote DECNET_SMTP host name: " nodename
$ IF nodename .EQS. "" THEN GOTO DNSMTP_REMOTE_END
$ IF F$LOCATE (".",nodename) .LT. F$LENGTH (nodename)
$ THEN
$   dnnode = F$ELEMENT (0, ".", nodename)
$ ELSE
$   dnnode = nodename
$ ENDIF
$ IF dnnode .NES. "*"
$ THEN
$   ask/prompt="* Enter the host's DECnet node name [''dnnode']: " nnode
$   IF nnode .EQS. "" THEN nnode = dnnode
$   mcpw "DEFINE PATH ""''nodename'"" DECNET_SMTP/ROUTE=''nnode'"
$ ELSE
$   mcpw "DEFINE PATH ""''nodename'"" DECNET_SMTP
$ ENDIF
$ a = "another"
$ GOTO DNSMTP_REMOTE_LOOP
$DNSMTP_REMOTE_END:
$ IF DNSMTP_ROUTER THEN GOTO SKIP_DNSMTP
$ TYPE SYS$INPUT:

    As an end node with only SMTP-over-DECnet as your mail transport,
    you must specify a routing system that will handle all non-local,
    non-DECnet messages.

$!
$DNSMTP_ROUTER_ASK:
$ ask/prompt="* Enter the DECnet node name of your routing host: " nodename
$ IF nodename .EQS. "" THEN GOTO DNSMTP_ROUTER_ASK
$ mcpw "DEFINE PATH * DECNET_SMTP/ROUTE=''nodename'"
$!
$SKIP_DNSMTP:
$!
$ IF USING_SMTP
$ THEN
$   mcpw "! NOTE: The next path definition should always be LAST."
$   mcpw "DEFINE PATH * SMTP"
$ ENDIF
$!
$ mcpw "!"
$ mcpw "! Done with routing information."
$ mcpw "!"
$!
$ TYPE SYS$INPUT:

    Defining the "Postmaster" Alias

    If you have not set up a username on the system called POSTMASTER,
    you should create an alias in MX for username Postmaster to direct
    mail to the person performing postmaster duties.

    All Internet-connected systems MUST have a valid Postmaster address.

    If you have a valid POSTMASTER account on your system, just
    press RETURN.  Otherwise, enter a full (user@host) address
    to which all Postmaster-addressed messages should be sent.

$!
$ ask/prompt="* Enter an alias for Postmaster (user@host): " fwdadr
$ IF fwdadr .NES. ""
$ THEN
$   IF F$LOCATE ("@",fwdadr) .EQ. F$LENGTH (fwdadr) THEN -
   	fwdadr = fwdadr + "@" + F$TRNLNM ("MX_NODE_NAME")
$   mcpw "DEFINE ALIAS ""Postmaster"" ""''fwdadr'"""
$ ENDIF
$ CLOSE MCP
$!
$ TYPE SYS$INPUT:

    There are no more configuration questions.

$!
$DOIT_ASK:
$ ask-
    /prompt="* Would you like to run MCP now to build the configuration? [Y]: "-
    yes
$ yes = F$EXTRACT (0,1,F$EDIT (yes, "UPCASE,TRIM,COMPRESS")+"Y")
$ IF yes .NES. "Y" .AND. yes .NES. "N" THEN GOTO DOIT_ASK
$ IF yes
$ THEN
$   OPEN/WRITE BLAH SYS$SCRATCH:MXCFG_'F$GETJPI("","PID").TMP
$   WRITE BLAH "@''mcpfile'"
$   WRITE BLAH "SAVE MX_DIR:MX_CONFIG.MXCFG"
$   CLOSE BLAH
$   mcp := $mx_exe:mcp
$   DEFINE/USER SYS$INPUT SYS$SCRATCH:MXCFG_'F$GETJPI("","PID").TMP
$   mcp/nofile
$   DELETE SYS$SCRATCH:MXCFG_'F$GETJPI("","PID").TMP;*
$ ELSE
$   SAY F$FAO ("!/    You can build the configuration later with "+-
        "the following commands:")
$   SAY F$FAO ("!/    $ MCP :== $MX_EXE:MCP")
$   SAY "    $ MCP/NOFILE"
$   SAY "    MCP> @",MCPFILE
$   SAY "    MCP> SAVE MX_DIR:MX_CONFIG"
$   SAY F$FAO ("    MCP> EXIT!/")
$ ENDIF
$MXCFG_EXIT:
$   EXIT 1+0*F$VERIFY(V)
