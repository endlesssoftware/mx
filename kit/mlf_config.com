$ V = 'F$VERIFY(0)
$!
$! MLF_CONFIG.COM
$!
$!  Easy construction of mailing lists and file servers.
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
$! 11-NOV-1991	T1.0	Madison	    Initial coding (from MXCONFIG).
$! 20-NOV-1991	V1.0	Madison	    Finished off, including FILESERV stuff.
$! 08-APR-1992	V1.0-1	Madison	    Fix BEGIN_SEND/END_SEND defaults.
$! 16-MAR-1993	V1.1	Goatley     Add fsmlist to DEFINE FILE command.
$! 15-FEB-2002  V1.2    Madison     Add text-only, confirmation questions.
$!
$ SET := SET
$ SET SYMBOL/SCOPE=(NOLOCAL,NOGLOBAL)
$ say := WRITE SYS$OUTPUT
$ ask := READ SYS$COMMAND
$ mcpw := WRITE MCP
$ skip = "WRITE SYS$OUTPUT """""
$ mcp := $mx_exe:mcp/file=mx_dir:mx_config
$!
$ IF F$TRNLNM ("MX_DIR") .EQS. ""
$ THEN
$   TYPE SYS$INPUT:

    MLF_CONFIG uses logical name definitions for MX.  Please execute
    the following command before using MLF_CONFIG:

                      $ @SYS$STARTUP:MX_STARTUP LOGICALS

$   EXIT
$ ENDIF
$!
$ TYPE SYS$INPUT:

                             M L F _ C O N F I G

    This procedure can be used to create a command file with the necessary
    MCP commands to set up MX mailing lists and file servers.

$!
$MCPFILE_ASK:
$ defans = "MX_DIR:MLF_CONFIG.MCP"
$ ask/prompt="* What do you want to call the command file? [''defans']: " -
    /END=MXCFG_EXIT MCPFILE
$ IF MCPFILE .EQS. "" THEN MCPFILE = defans
$ CREATE 'MCPFILE'
$ IF .NOT. $STATUS THEN GOTO MCPFILE_ASK
$ MCPFILE = F$SEARCH (MCPFILE)
$!
$ OPEN/APPEND MCP 'MCPFILE
$ mcpw "! ''MCPFILE'"
$ mcpw "! Created: ", F$TIME(), " by MLF_CONFIG"
$ mcpw "!"
$ mcp show system_users/output=sys$scratch:mlf_config.tmp
$ open/read x sys$scratch:mlf_config.tmp
$ read x line
$ close x
$ delete sys$scratch:mlf_config.tmp;*
$ IF line .EQS. "" THEN GOTO Skip_SYSU
$!

    List Server Managers
    --------------------

    Before setting up any mailing lists, you should assign a primary
    list server manager and as many secondary managers as you think
    is appropriate.

    List server managers are granted access to all mailing lists on
    the system through the SYSTEM protection class.  In addition,
    they have implicit control access to lists (meaning they can
    add and remove other users from lists).

    The primary list server manager is also the contact for all
    bounced non-specific LISTSERV mail messages (those not related
    to a specific mailing list).

    Remember to specify each address in the form user@hostname,
    even for users on the local host.
$!
$Primary_SYSU:
$ skip
$ ask/prompt="* Enter the address of the PRIMARY list server manager: " user
$ IF user .EQS. ""
$ THEN
$   say "    You MUST specify a primary list server manager address."
$   GOTO Primary_SYSU
$ ENDIF
$ IF F$LOCATE ("@",user) .EQ. F$LENGTH (user) THEN -
    	user = user + "@" + F$TRNLNM ("MX_NODE_NAME")
$ sysu = """LSrvMngr@" + F$TRNLNM ("MX_NODE_NAME") + """,""" + user + """"
$ mcpw "DEFINE ALIAS ""LSrvMngr"" """ + user + """"
$!
$ say "    Just press RETURN to finish and move on to the next section."
$SYSU_LOOP:
$ skip
$ ask/prompt="* Enter the address of another list server manager: " user
$ IF user .EQS. "" THEN GOTO DONE_SYSU
$ sysu = sysu + ","
$ sysu = sysu + """''user'"""
$ GOTO SYSU_LOOP
$!
$DONE_SYSU:
$ IF F$LENGTH (sysu) .NE. 0
$ THEN
$   mcpw "DEFINE SYSTEM_USERS ",sysu
$   mcpw "!"
$ ENDIF
$!
$Skip_SYSU:
$!
$ TYPE SYS$INPUT:

    Mailing Lists
    -------------

    Information required for a mailing list definition includes:

        * Name of mailing list
        * E-mail address of the owner of the mailing list

    Optional information:

    	* Errors-To address
    	* Description of mailing list
        * Archive file specification
        * Protection information
        * Subscription confirmations
        * Text-only content limiting

    Refer to the MX Mailing List/File Server Guide for detailed information
    on creating and managing mailing lists.

    If you do not want to add any mailing lists, just press RETURN.
$!
$MLIST_LOOP:
$ skip
$ ask/prompt="* Enter a name for the mailing list: " mlname
$ IF mlname .EQS. "" THEN GOTO DONE_MLIST
$MLO_LOOP:
$ ask/prompt="* Address of mailing list owner: " owner
$ IF owner .EQS. ""
$ THEN
$   say F$FAO ("!/You must specify an owner address.!/")
$   GOTO MLO_LOOP
$ ENDIF
$ IF F$LOCATE ("@",owner) .EQ. F$LENGTH (owner) THEN -
    	owner = owner + "@" + F$TRNLNM ("MX_NODE_NAME")
$!
$ TYPE SYS$INPUT:

    Errors-To information
    ---------------------

    If you would like to have bounced mailing list messages
    go to someone other than the list owner, specify that
    person's address.  Otherwise, just press RETURN.

$ ask/prompt="* Errors-to address [''owner']: " errsto
$ IF errsto .EQS. "" THEN errsto = owner
$ IF F$LOCATE ("@",errsto) .EQ. F$LENGTH (errsto) THEN -
    	errsto = errsto + "@" + F$TRNLNM ("MX_NODE_NAME")
$ errsto = "/ERRORS_TO=""''errsto'"""
$!
$ TYPE SYS$INPUT:

    Description
    -----------

    Enter a brief description of the mailing list, if desired.
    This description will be added to the X-ListName header
    along with the list name on all messages sent via the list.

    If you do not wish to add a description to the mailing list,
    just press RETURN.

$ ask/prompt="* Description: " descrip
$ IF descrip .NES. ""
$ THEN
$   descrip = "/DESCRIPTION=""''descrip'"""
$ ELSE
$   descrip = "/DESCRIPTION="""""
$ ENDIF
$!
$ TYPE SYS$INPUT:

    List Archive
    ------------

    If you wish to have the mailing list archived, enter a file
    specification for the archive file(s).

    If you use just a device and directory specification, MX will
    use the mailing list name as the file name and the current month
    and year as the file type when archiving a message (monthly archives).

    If you also specify a file name, MX will use that name for the
    archive file and use the month and year as the file type.

    If you also specify a file type, MX will archive all messages
    into a single file.

    If you do not wish to have an archive for the mailing list,
    just press RETURN.

$!
$ ask/prompt="* Enter archive filespec: " arch
$ IF arch .NES. ""
$ THEN
$   IF F$PARSE (arch) .EQS. ""
$   THEN
$       TYPE SYS$INPUT:

    The directory you specified does not exist.  Don't forget to create
    the archive directory before using the mailing list.

$!
$   ENDIF
$   arch = "/ARCHIVE=''arch'"
$ ELSE
$   arch = "/NOARCHIVE"
$ ENDIF
$!
$ prot1 = "/PROTECTION=(S:RWED,O:RWED,G:RWED,W:RWE)"
$ prot2 = "/PROTECTION=(S:RWED,O:RWED,G:RWED,W:E)"
$ prot3 = "/PROTECTION=(S:RWED,O:RWED,G:W,W)"
$ TYPE SYS$INPUT:

    Mailing List Protection
    -----------------------

    There are three basic mailing list protection types.

    1.  Fully public list: anyone can subscribe to the list, and
        even non-subscribers can post messages to the list.

    2.  Semi-public list: anyone can subscribe to the list, but
        only subscribers can post messages to the list.

    3.  Private list: all subscription requests are forwarded
        to list owner, who must manually add or remove users
        from the list; only subscribers can post to the list.

    Refer to the MX Mailing List/File Server Guide for details on customizing
    your mailing list protection.

$!
$MLPROT_ASK:
$ ask/prompt="* Which type of protection would you like? [1]: " prottype
$ IF prottype .EQS. "" THEN prottype = "1"
$ prottype = F$INTEGER (prottype)
$ IF prottype .LT. 1 .OR. prottype .GT. 3
$ THEN
$   say F$FAO ("!/Please answer 1, 2, or 3.!/")
$   GOTO MLPROT_ASK
$ ENDIF
$ prot = prot'prottype
$!
$ TYPE SYS$INPUT:

    Subscription Confirmations
    --------------------------

    You can configure the mailing list to require
    confirmation for all new subscription requests.
    When enabled, the mailing list processor sends
    a confirmation message to the subscribing address;
    a reply must be received within 24 hours for the
    address to be added to the list.

$!
$MLCONF_ASK:
$ ask/prompt="* Require subscription confirmations? [NO]: " yes
$ IF yes .EQS. "" THEN yes = "N"
$ yes = F$EDIT (F$EXTRACT (0,1,yes),"UPCASE")
$ IF yes .NES. "Y" .AND. yes .NES. "N" 
$ THEN
$   say "Please answer YES or NO."
$   GOTO MLCONF_ASK
$ ENDIF
$ IF yes
$ THEN reqconf = "/REQUEST_CONFIRMATION"
$ ELSE reqconf = "/NOREQUEST_CONFIRMATION"
$ ENDIF
$!
$ TYPE SYS$INPUT:

    Text-Only Content
    -----------------

    You can configure the mailing list to accept
    only plain-text message content, to prevent
    users from sending potentially dangerous
    attachments or HTML as list postings.

$!
$MLTEXTO_ASK:
$ ask/prompt="* Limit list postings to text only? [NO]: " yes
$ IF yes .EQS. "" THEN yes = "N"
$ yes = F$EDIT (F$EXTRACT (0,1,yes),"UPCASE")
$ IF yes .NES. "Y" .AND. yes .NES. "N" 
$ THEN
$   say "Please answer YES or NO."
$   GOTO MLTEXTO_ASK
$ ENDIF
$ IF yes
$ THEN texto = "/TEXT_ONLY"
$ ELSE texto = "/NOTEXT_ONLY"
$ ENDIF
$!
$ mcpw "DEFINE LIST ""''mlname'""-"
$ mcpw "  /OWNER=""''owner'""-"
$ mcpw "  ",errsto,"-"
$ mcpw "  ",descrip,"-"
$ mcpw "  ",arch,"-"
$ mpcw "  ",reqconf,"-"
$ mcpw "  ",texto,"-"
$ mcpw "  ",prot'prottype'
$ mcpw "!"
$!
$ TYPE SYS$INPUT:

    Finished configuring this mailing list.  If you would like to create
    another mailing list, enter its name.  Otherwise, press RETURN to
    move on to the next section.

$ GOTO MLIST_LOOP
$!
$DONE_MLIST:
$!
$ TYPE SYS$INPUT:

    File Servers
    ------------

    Information required for creating a file server includes:

    	* Name of file server
        * E-mail address of the file server manager
    	* Device and directory of the root of file server

    Optional information:

        * Threshold size for delaying delivery of a file to off-hours
        * Start and end of the off-hours delivery period
    	* Mailing list "connected" to file server for access control
        * Per-user, per-host, and server-global daily limits

    Refer to the MX Mailing List/File Server Guide for detailed information
    on creating and managing file servers.

    If you do not want to add any file servers, just press RETURN.
$!
$FSRV_LOOP:
$   skip
$   ask/prompt="* Enter a name for the file server: " fsname
$ IF fsname .EQS. "" THEN GOTO DONE_FSRV
$FSM_LOOP:
$ ask/prompt="* Address of file server manager: " fsmgr
$ IF fsmgr .EQS. ""
$ THEN
$   say F$FAO ("!/You must specify an manager address.!/")
$   GOTO FSM_LOOP
$ ENDIF
$ IF F$LOCATE ("@",fsmgr) .EQ. F$LENGTH (fsmgr) THEN -
    	fsmgr = fsmgr + "@" + F$TRNLNM ("MX_NODE_NAME")
$!
$ TYPE SYS$INPUT:

    Top-level Directory for File Server
    -----------------------------------

    File servers have a fixed directory structure, the top of
    which is called the root.  All references to the packages
    provided by the file server will be made via this root.

    Please enter the device and directory that will serve as
    the top directory for the file server.

$!
$FSRV_DIRASK:
$ fsdirdef = "MX_ROOT:[MLF.FILE_SERVERS." + F$EDIT (fsname,"UPCASE") + "]"
$ ask/prompt="* Enter directory [''fsdirdef']: " fsdir
$ IF fsdir .EQS. "" THEN fsdir = fsdirdef
$ IF F$PARSE (fsdir) .EQS. ""
$ THEN
$   Skip
$   ask/prompt="* Directory does not exist.  Ok to create? [Y]: " yes
$   IF yes .EQS. "" THEN yes = "Y"
$   IF yes
$   THEN
$   	CREATE/DIRECTORY 'fsdir/PROTECTION=(S:RWE,O:RWE,G,W)
$   	IF .NOT. $STATUS
$   	THEN
$   	    say "Could not create directory.  Please try again."
$   	    GOTO FSRV_DIRASK
$   	ENDIF
$   ENDIF
$ ENDIF
$ fsdir = F$PARSE (fsdir,,,,"NO_CONCEAL,SYNTAX_ONLY") - "]["
$ fsroot = F$PARSE (fsdir,,,"DEVICE","SYNTAX_ONLY") +-
     (F$PARSE (fsdir,,,"DIRECTORY","SYNTAX_ONLY")-"]"+".]")
$!
$ TYPE SYS$INPUT:

    Off-Hours Delivery of Large Files
    ---------------------------------

    If you wish, you can have the file server delay the delivery of
    files exceeding a certain size until off-hours, to cut down on
    system and network load during the day.

    If you answer YES to the next question, you will be prompted
    for the threshold size (in bytes) that will determine whether
    a file is delayed, along with the start and end times of the
    off-hours delivery period.

$ ask/prompt=-
  "* Would you like to have large files sent only during off-hours? [N]: " yes
$ IF yes .EQS. "" THEN yes = "N"
$ IF yes
$ THEN
$   ask/prompt="* Enter threshold size, in bytes, for "large" [16384]: " size
$   size = F$INTEGER (size)
$   TYPE SYS$INPUT:

    For the next two questions, enter the time in the form "hh:mm",
    where "hh" is the hour on the 24-hour clock, and "mm" is the minute.

$   ask/prompt="* Enter start of off-hours period [17:00]: " start
$   IF start .EQS. "" THEN start = "17:00"
$   ask/prompt="* Enter end of off-hours period [08:00]: " end
$   IF end .EQS. "" THEN end = "08:00"
$   fsdelay = "/DELAY_THRESHOLD=''size'/BEGIN_SEND=''start'/END_SEND=''end'"
$ ELSE
$   fsdelay = "/NODELAY_THRESHOLD"
$ ENDIF
$!
$ TYPE SYS$INPUT:

    Mailing List
    ------------

    If this file server is intended to service only the subscribers
    of a particular local mailing list, specify the name of the list.
    For a public-access file server, just press RETURN.

$ ask/prompt="* Mailing list for file server: " fsmlist
$ IF fsmlist .EQS. ""
$ THEN fsmlist = "/NOMAILING_LIST"
$ ELSE fsmlist = "/MAILING_LIST=""''fsmlist'"""
$ ENDIF
$!
$ TYPE SYS$INPUT:

    Daily Request Limits
    --------------------

    You may elect to limit the amount of data sent to a particular
    user and/or a particular host per day.  You may also elect to
    place a global limit on the amount of data that the file server
    will send per day.


$ ask/prompt="* Do you want to have a per-user daily limit? [N]: " yes
$ IF yes .EQS. "" THEN yes := N
$ IF yes
$ THEN
$   ask/prompt="* Enter the per-user daily limit, in bytes [1000000]: " peruser
$   IF peruser .EQS. "" THEN peruser = "1000000"
$   peruser = "/USER_LIMIT=''peruser'"
$ ELSE
$   peruser = "/NOUSER_LIMIT"
$ ENDIF
$ ask/prompt="* Do you want to have a per-host daily limit? [N]: " yes
$ IF yes .EQS. "" THEN yes := N
$ IF yes
$ THEN
$   ask/prompt="* Enter the per-host daily limit, in bytes [2000000]: " perhost
$   IF perhost .EQS. "" THEN perhost = "2000000"
$   perhost = "/HOST_LIMIT=''perhost'"
$ ELSE
$   perhost = "/NOHOST_LIMIT"
$ ENDIF
$ ask/prompt="* Do you want to have a server daily limit? [N]: " yes
$ IF yes .EQS. "" THEN yes := N
$ IF yes
$ THEN
$   ask/prompt="* Enter the server's daily limit, in bytes [10000000]: " perserv
$   IF perserv .EQS. "" THEN perserv = "10000000"
$   perserv = "/SERVER_LIMIT=''perserv'"
$ ELSE
$   perserv = "/NOSERVER_LIMIT"
$ ENDIF
$!
$ mcpw "DEFINE FILE_SERVER ""''fsname'""-"
$ mcpw "  /MANAGER=""''fsmgr'""-"
$ mcpw "  /ROOT=''fsroot'-"
$ mcpw "  ",fsmlist,"-"
$ mcpw "  ",fsdelay,"-"
$ mcpw "  ",peruser,perhost,perserv
$ mcpw "!"
$ TYPE SYS$INPUT:

    Finished configuring this file server.  If you would like to create
    another file server, enter its name.  Otherwise, press RETURN to
    finish.

$ GOTO FSRV_LOOP
$!
$DONE_FSRV:
$!
$ CLOSE MCP
$!
$ TYPE SYS$INPUT:

    There are no more configuration questions.  You should now review the MCP
    commands generated by this procedure and execute them with the following
    commands:

$   SAY F$FAO ("!/    $ MCP :== $MX_EXE:MCP")
$   SAY "    $ MCP"
$   SAY "    MCP> @",MCPFILE
$   SAY "    MCP> SAVE"
$   SAY "    MCP> EXIT"
$ EXIT 1
