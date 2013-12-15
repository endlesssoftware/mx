$ v = 'F$VERIFY(0)
$!
$!  MXCONFIG.COM
$!
$!  Bootstrap configuration script for MX.
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
$!  18-FEB-1997	V1.0	Madison	    Initial coding.
$!  03-MAY-1997	V1.1	Madison	    Remove MX_VMSMAIL_LOCALHOST.
$!  08-MAY-1997	V1.1-1	Madison	    Fix timezone check.
$!  01-JUN-1997	V1.1-2	Madison	    Kill startup info for non-existent agents.
$!  26-APR-1998	V1.1-3	Madison	    Explain system timezone use.
$!  27-JUN-1998	V1.2	Madison	    Add max message size logical names.
$!  13-AUG-1998	V1.2-1	Madison	    Protect against blanks as node name.
$!  28-NOV-1999	V1.2-2	Madison	    Increase SMTP thread limit to 36.
$!  25-NOV-2000	V1.3	Madison	    Remove Jnet support.
$!  25-DEC-2000	V1.4	Madison	    Move queue directory creation here.
$!  15-MAR-2001 V1.4-1  Madison     Fix queue directory creation loop.
$!  03-Feb-2008 V2.0    Madison     Remove X.25,UUCP,LSV support.
$!  11-Jun-2010 V2.1    Tim Sneddon Re-apply UUCP.
$!
$ SET := SET
$ SET SYMBOL/SCOPE=(NOLOCAL,NOGLOBAL)
$ ask := READ SYS$COMMAND/END=Step_5
$ say := WRITE SYS$OUTPUT
$ mcp := $MX_EXE:MCP
$ did_config_db = 0
$!
$ GOSUB Read_Current_Configuration
$!
$ ON CONTROL_Y THEN GOTO Step_5
$!
$ lognam_change = 0
$ changed_lognams = ""
$ process_change = 0
$!
$ installing = p1 .EQS. "INSTALL" .OR. p1 .EQS. "UPGRADE"
$ IF installing THEN GOTO Step_1
$ GOTO Main_Menu
$Depart:
$ EXIT 1+0*F$VERIFY(v)
$!
$Main_Menu:
$ TYPE SYS$INPUT:

                  Message Exchange Configuration Procedure

Main Menu

    1. Configure MX message queue.
    2. Configure MX host and timezone logical names.
    3. Configure MX agent processes.
    4. Create an MX configuration database.
    5. Exit from this procedure.

$Main_ask:
$ ask/prompt="Enter choice: " choice
$ choice = F$INTEGER(choice)
$ IF choice .LT. 1 .OR choice .GT. 5
$ THEN
$   say "Please enter a number from 1 to 5."
$   GOTO Main_ask
$ ENDIF
$ GOTO Step_'choice'
$!
$Step_1:   ! configuring MX message queue
$!
$ defans = "''mx_flq_dir'"
$ IF defans .EQS. "" THEN defans = "MX_ROOT:[QUEUE]"
$ defans = F$EDIT (defans, "TRIM,UPCASE")
$!
$ TYPE SYS$INPUT:

    Message Queue Directory
    -----------------------

    MX uses a directory tree for storing queued mail messages.  This directory
    tree may be placed with the other MX directories, or may be placed on a
    different disk.  The disk on which the queue directory resides must
    have quotas disabled or must have sufficient system quota to provide
    for a backlog of undelivered messages.

$!
$flq_dir_ask:
$ ask/prompt="Message queue root directory [''defans']: " mx_tmp
$ IF mx_tmp .EQS. "" THEN mx_tmp = defans
$ IF F$PARSE (mx_tmp,"$$NOSUCHDEV$$:[$$NOSUCHDIR$$]",,"DEVICE","SYNTAX_ONLY") .EQS. "$$NOSUCHDEV$$:" .OR. -
     F$PARSE (mx_tmp,"$$NOSUCHDEV$$:[$$NOSUCHDIR$$]",,"DIRECTORY","SYNTAX_ONLY") .EQS. "[$$NOSUCHDIR$$]" .OR. -
     F$PARSE (mx_tmp,,,,"SYNTAX_ONLY") .EQS. "" .OR. -
     F$LOCATE (">[",mx_tmp) .LT. F$LENGTH (mx_tmp) .OR. F$LOCATE ("]<",mx_tmp) .LT. F$LENGTH (mx_tmp)
$ THEN
$   TYPE SYS$INPUT:

    Please enter a device and directory specification.

$   GOTO flq_dir_ask
$ ENDIF
$ mx_tmp = F$PARSE (mx_tmp,,,"DEVICE","SYNTAX_ONLY") + F$PARSE (mx_tmp,,,"DIRECTORY","SYNTAX_ONLY") - "]["
$ IF "''mx_flq_dir'" .EQS. ""
$ THEN
$   mx_flq_dir = mx_tmp
$   lognam_change = 1
$   IF F$LOCATE (",MX_FLQ_DIR",changed_lognams) .EQ. F$LENGTH (changed_lognams) THEN -
    	changed_lognams = changed_lognams + ",MX_FLQ_DIR"
$   GOTO Check_FLQ_Tree_Exists
$ ENDIF
$ IF .NOT. (F$SEARCH ("''mx_flq_dir'MX_SYSTEM_QUEUE.FLQ_CTL") .NES. "" .AND. -
    	    F$PARSE (mx_flq_dir,,,,"NO_CONCEAL") .NES. F$PARSE(mx_tmp,,,,"NO_CONCEAL"))
$ THEN
$   mx_flq_dir = mx_tmp
$   lognam_change = 1
$   IF F$LOCATE (",MX_FLQ_DIR",changed_lognams) .EQ. F$LENGTH (changed_lognams) THEN -
    	changed_lognams = changed_lognams + ",MX_FLQ_DIR"
$   GOTO Check_FLQ_Tree_Exists
$ ENDIF
$!
$ say ""
$ say "A message queue exists at ''mx_flq_dir'."
$ say ""
$Ask_Queue_Move:
$ ask/prompt="Do you want to move the message queue to ''mx_tmp'? [NO]: " mx_tmp1
$ IF mx_tmp1 .EQS. "" THEN mx_tmp1 = "NO"
$ IF F$LOCATE (mx_tmp1, "YES") .EQ. 0
$ THEN mx_tmp1 = "YES"
$ ELSE IF F$LOCATE (mx_tmp1, "NO") .EQ. 0
$      THEN
$   	    mx_tmp1 = "NO"
$      ELSE
$   	    say "Please answer YES or NO."
$   	    GOTO Ask_Queue_Move
$      ENDIF
$ ENDIF
$ IF .NOT. mx_tmp1
$ THEN
$   say ""
$   say "Message queue will remain at ''mx_flq_dir'."
$   say ""
$   GOTO Check_FLQ_Tree_Exists
$ ENDIF
$ TYPE SYS$INPUT:

    ** WARNING **

    MX must be COMPLETELY shut down before attempting to move the message queue.
    If any MX delivery agents or VMS MAIL users are attempting to access the
    message queue, the move will fail.
    
$Ask_Queue_Move1:
$ ask/prompt="Are you sure want to move the message queue to ''mx_tmp'? [NO]: " mx_tmp1
$ IF mx_tmp1 .EQS. "" THEN mx_tmp1 = "NO"
$ IF F$LOCATE (mx_tmp1, "YES") .EQ. 0
$ THEN mx_tmp1 = "YES"
$ ELSE IF F$LOCATE (mx_tmp1, "NO") .EQ. 0
$      THEN
$   	    mx_tmp1 = "NO"
$      ELSE
$   	    say "Please answer YES or NO."
$   	    GOTO Ask_Queue_Move1
$      ENDIF
$ ENDIF
$ IF .NOT. mx_tmp1
$ THEN
$   say ""
$   say "Message queue will remain at ''mx_flq_dir'."
$   say ""
$   GOTO Check_FLQ_Tree_Exists
$ ENDIF
$!
$ IF F$PARSE ("''mx_tmp'") .EQS. "" THEN CREATE/DIRECTORY/LOG/OWNER=[1,4]/PROTECTION=(S:RWE,O:RWE,G:RE,W) 'mx_tmp'
$ say ""
$ say "BACKUP will be used to move the message queue"
$ say "  From: ''mx_flq_dir'"
$ say "  To:   ''mx_tmp'"
$ say "Logging will be enabled during the move."
$ say ""
$ bkup_src = mx_flq_dir - "]" + "...]*.*;"
$ bkup_dst = mx_tmp - "]" + "...]"
$ SET NOON
$ BACKUP 'bkup_src' 'bkup_dst'/OWNER=PARENT/LOG
$ SET ON
$ IF .NOT. $STATUS
$ THEN
$   say ""
$   say "Errors occurred during move.  Queue will remain at ''mx_flq_dir'."
$   say "You must manually delete any files at ''mx_tmp' before trying the move again."
$   say ""
$   GOTO Check_FLQ_Tree_Exists
$ ENDIF
$!
$ say ""
$ say "Message queue tree successfully moved to ''mx_tmp'."
$ say "Old message queue tree at ''mx_flq_dir' has been retained."
$ say "You many delete the old tree after MX has successfully run using the new queue."
$ say ""
$ mx_flq_dir = mx_tmp
$ lognam_change = 1
$ IF F$LOCATE (",MX_FLQ_DIR",changed_lognams) .EQ. F$LENGTH (changed_lognams) THEN -
    changed_lognams = changed_lognams + ",MX_FLQ_DIR"
$!
$Check_FLQ_Tree_Exists:
$ IF F$PARSE ("''mx_flq_dir'") .EQS. "" THEN CREATE/DIRECTORY/LOG 'mx_flq_dir'/OWNER=[1,4]/PROTECTION=(S:RWE,O:RWE,G:RE,W)
$ IF F$SEARCH ("''mx_flq_dir'MX_SYSTEM_QUEUE.FLQ_CTL") .NES. "" THEN GOTO Check_FLQ_Subdirs
$ TYPE SYS$INPUT:

    Selecting the Size of the MX Message Queue
    ------------------------------------------

    The MX queueing subsystem uses a fixed-size sequential file for
    queue control.  The size of the file determines the number of
    messages that can be in the queue at any one time.  The size of
    the file can be extended at a later date using the MCP command
    QUEUE EXTEND.

    For each message, one block is required.  To allow up to 5,000
    messages to be in the queue at any one time, the queue file must
    be slightly larger than 5,000 blocks. The required file size
    depends heavily on your site's e-mail traffic.  The minimum
    required size is 100 entries.

    For sites with a lot of mail traffic, a size of 5,000--10,000
    blocks is recommended.  If disk space is not a problem, you can
    specify as many as 131,072 (128K) messages, which is the maximum
    number MX is designed to handle.

$ defans = "5000"
$flq_size_ask:
$ ask/prompt="Maximum number of entries to allow in the queue? [''defans']: " flq_size
$ IF flq_size .EQS. "" THEN flq_size = defans
$ IF F$INTEGER (flq_size) .LT. 100 .OR. F$INTEGER (flq_size) .GT. 131072
$ THEN
$   say ""
$   say "Please enter an integer from 100 to 131072."
$   say ""
$   GOTO flq_size_ask
$ ENDIF
$ SET NOON
$ mcp/NOFILE QUEUE CREATE/MAXIMUM='flq_size' 'mx_flq_dir'MX_SYSTEM_QUEUE.FLQ_CTL
$ SET ON
$ IF F$SEARCH ("''mx_flq_dir'MX_SYSTEM_QUEUE.FLQ_CTL") .EQS. ""
$ THEN
$   TYPE SYS$INPUT:

    An error occurred creating the queue control file.  Please correct the
    problem and use the MCP QUEUE CREATE command to create the queue control file.

$ ENDIF
$Check_FLQ_Subdirs:
$ SET ACL/DELETE=ALL 'mx_flq_dir'MX_SYSTEM_QUEUE.FLQ_CTL
$ SET PROTECTION=(S:RWE,O:RWE,G:RE,W) 'mx_flq_dir'MX_SYSTEM_QUEUE.FLQ_CTL
$ mx_tmp = mx_flq_dir - "]"
$ mx_i = 0
$FLQ_Subdir_Loop:
$ IF F$PARSE ("''mx_tmp'.''mx_i']") .EQS. "" THEN CREATE/DIRECTORY/LOG 'mx_tmp'.'mx_i']
$ SET ACL/DELETE=ALL 'mx_tmp']'mx_i'.DIR;1
$ SET ACL/ACL=(DEFAULT_PROTECTION,S:RWED,O:RWED,G,W) 'mx_tmp']'mx_i'.DIR;1
$ mx_i = mx_i + 1
$ IF mx_i .LE. 9 THEN GOTO FLQ_Subdir_Loop
$!
$Config_FLQ_Node:
$ defans = "''mx_flq_node_name'"
$ defans1 = defans
$ IF defans1 .EQS. "" THEN defans1 = F$TRNLNM("MX_FLQ_NODE_NAME","LNM$SYSTEM",,"EXECUTIVE")
$ IF defans1 .EQS. "" THEN defans1 = F$TRNLNM ("SYS$CLUSTER_NODE") - "::"
$ IF defans1 .EQS. "" THEN defans1 = F$GETSYI ("NODENAME")
$ IF defans1 .EQS. "" THEN defans1 = "LOCAL"
$ defans1 = F$EDIT (defans1,"TRIM,UPCASE")
$!
$ TYPE SYS$INPUT:

    MX Message Queue "Cluster" Name
    -------------------------------

    This is a 1-to-6 character name that is used to coordinate access
    to the message queue.  If this system is part of a VMScluster, all
    systems in the cluster that share this MX installation will use
    the same message queue cluster name.

    For further information on MX cluster support, see the Manager's Guide.

    This name is typically set to the DECnet cluster alias or the
    DECnet node name of the local system.

$!
$flq_node_ask:
$ ask/prompt="Message queue cluster name [''defans1']: " mx_flq_node_name
$ mx_flq_node_name = F$EDIT(mx_flq_node_name,"TRIM,UPCASE")
$ IF mx_flq_node_name .EQS. "" THEN mx_flq_node_name = defans1
$ IF F$LENGTH (mx_flq_node_name) .GT. 6
$ THEN
$   say "Please enter no more than six characters."
$   GOTO flq_node_ask
$ ENDIF
$ IF F$LENGTH (mx_flq_node_name) .EQ. 0
$ THEN
$   say "Please enter a non-null node name."
$   GOTO flq_node_ask
$ ENDIF
$ IF mx_flq_node_name .NES. defans
$ THEN
$   lognam_change = 1
$   IF F$LOCATE (",MX_FLQ_NODE_NAME",changed_lognams) .EQ. F$LENGTH (changed_lognams) THEN -
    	changed_lognams = changed_lognams + ",MX_FLQ_NODE_NAME"
$ ENDIF
$!
$ origval = "''mx_flq_autopurge_fin'"
$ defans  = origval
$ IF defans .EQS. "" THEN defans = F$TRNLNM("MX_FLQ_AUTOPURGE_FIN","LNM$SYSTEM",,"EXECUTIVE")
$ IF defans .EQS. "" THEN defans = "NO"
$ IF defans THEN defans = "YES"
$ IF .NOT. defans THEN defans = "NO"
$!
$ TYPE SYS$INPUT:

    Immediate Deletion of Finished Messages
    ---------------------------------------

    When an MX queue entry has been fully processed,  it is marked as
    being "finished" and  is  left in the queue for a period of time.
    The MX Router or MX FLQ Manager scans the file every  15 minutes,
    by default, and purges the finished entries.  This delay can
    improve responsiveness of the delivery agents and reduce contention
    for the message queue.

    However, sites processing a high volume of messages may need to have
    the finished queue entries deleted immediately on completion, in
    order to reclaim message queue and file system resources more
    quickly.

$autopurge_ask:
$ ask/prompt="Should finished messages be deleted immediately? [''defans']: " -
    	mx_flq_autopurge_fin
$ IF mx_flq_autopurge_fin .EQS. "" THEN mx_flq_autopurge_fin = defans
$ mx_flq_autopurge_fin = F$EDIT (mx_flq_autopurge_fin,"TRIM,UPCASE")
$ IF F$LOCATE (mx_flq_autopurge_fin, "YES") .EQ. 0
$ THEN
$   mx_flq_autopurge_fin = "YES"
$ ELSE
$   IF F$LOCATE (mx_flq_autopurge_fin, "NO") .EQ. 0
$   THEN
$   	mx_flq_autopurge_fin = "NO"
$   ELSE
$   	say "Please answer YES or NO."
$   	GOTO autopurge_ask
$   ENDIF
$ ENDIF
$ IF origval .EQS. "" .OR. ((mx_flq_autopurge_fin .AND. .NOT. defans) .OR. -
     (defans .AND. .NOT. mx_flq_autopurge_fin))
$ THEN
$   lognam_change = 1
$   IF F$LOCATE(",MX_FLQ_AUTOPURGE_FIN", changed_lognams) .EQ. F$LENGTH (changed_lognams) THEN -
    	changed_lognams = changed_lognams + ",MX_FLQ_AUTOPURGE_FIN"
$ ENDIF
$ IF mx_flq_autopurge_fin
$ THEN mx_flq_autopurge_fin = "TRUE"
$ ELSE mx_flq_autopurge_fin = ""
$ ENDIF
$!
$ origval = "''mx_flq_max_entry_size'"
$ defans = origval
$ IF defans .EQS. "" THEN defans = F$TRNLNM("MX_FLQ_MAX_ENTRY_SIZE","LNM$SYSTEM","EXECUTIVE")
$ IF defans .EQS. "" THEN defans = "0"
$!
$ TYPE SYS$INPUT:

    Maximum Message Size
    --------------------

    You may set a fixed limit on the size of messages that MX will
    accept.  Any message that is larger than this fixed limit will
    be rejected by the message entry agents.  The maximum size is
    specified in KBytes.

    By default, the maximum size is zero, meaning that there is no
    fixed limit.

$max_size_ask:
$ ask/prompt="Maximum message size (in KBytes)? [''defans']: " mx_flq_max_entry_size
$ IF mx_flq_max_entry_size .EQS. "" THEN mx_flq_max_entry_size = defans
$ mx_flq_max_entry_size = F$FAO ("!UL", F$INTEGER(F$EDIT (mx_flq_max_entry_size,"TRIM")))
$ IF origval .EQS. "" .OR. mx_flq_max_entry_size .NES. defans
$ THEN
$   lognam_change = 1
$   IF F$LOCATE(",MX_FLQ_MAX_ENTRY_SIZE", changed_lognams) .EQ. F$LENGTH (changed_lognams) THEN -
    	changed_lognams = changed_lognams + ",MX_FLQ_MAX_ENTRY_SIZE"
$ ENDIF
$!
$ origval = "''mx_flq_disk_free_reserved'"
$ defans = origval
$ IF defans .EQS. "" THEN defans = F$TRNLNM("MX_FLQ_DISK_FREE_RESERVED","LNM$SYSTEM","EXECUTIVE")
$ IF defans .EQS. "" THEN defans = "10"
$!
$ TYPE SYS$INPUT:

    Reserved Free Space on Message Queue Device
    -------------------------------------------

    MX limits the size of messages it accepts based on the amount
    of free space available on the disk device where the message
    queue resides.

    You may reserve a percentage of the total space on the disk.
    MX will ensure that it accepts no message that will cause the
    amount of remaining free space to drop below the reserved
    amount.

    By default, the reserved free space setting is 10%.  The free
    space percentage may range from 1 to 90.

$free_reserved_ask:
$ ask/prompt="Percentage of disk space to reserve (1-90)? [''defans']: " mx_flq_disk_free_reserved
$ IF mx_flq_disk_free_reserved .EQS. "" THEN mx_flq_disk_free_reserved = defans
$ mx_flq_disk_free_reserved = F$FAO ("!UL", F$INTEGER(F$EDIT (mx_flq_disk_free_reserved,"TRIM")))
$ IF F$INTEGER (mx_flq_disk_free_reserved) .LT. 1 .OR. -
     F$INTEGER (mx_flq_disk_free_reserved) .GT. 90
$ THEN
$   say "Please specify a number from 1 to 90."
$   GOTO free_reserved_ask
$ ENDIF
$ IF origval .EQS. "" .OR. mx_flq_disk_free_reserved .NES. defans
$ THEN
$   lognam_change = 1
$   IF F$LOCATE(",MX_FLQ_DISK_FREE_RESERVED", changed_lognams) .EQ. F$LENGTH (changed_lognams) THEN -
    	changed_lognams = changed_lognams + ",MX_FLQ_DISK_FREE_RESERVED"
$ ENDIF
$!
$ IF .NOT. installing THEN GOTO Main_Menu
$!
$Step_2:
$!
$ defans = "''mx_node_name'"
$ defans1 = defans
$ IF defans1 .EQS. "" THEN defans1 = F$TRNLNM("MX_NODE_NAME","LNM$SYSTEM",,"EXECUTIVE")
$ IF defans1 .EQS. "" THEN defans1 = F$TRNLNM("UCX$INET_HOST")
$!
$ TYPE SYS$INPUT:

    MX Network Host Name
    --------------------

    This is a 1-to-255 character name that is your "official" host
    name for E-mail purposes.

    For Internet hosts, this should be your Internet domain name.
    (Example: myhost.mycompany.com)

    For hosts not connected to the Internet, consult your network
    consult your network manager for an appropriate host name.

$ IF defans1 .EQS. ""
$ THEN defans2 = ""
$ ELSE defans2 = " [''defans1']"
$ ENDIF
$nodename_ask:
$ ask/prompt="Enter the MX network host name''defans2': " mx_node_name
$ IF mx_node_name .EQS. "" THEN mx_node_name = defans1
$ IF mx_node_name .EQS. ""
$ THEN
$   say "Please enter a host name."
$   GOTO nodename_ask
$ ENDIF
$ IF mx_node_name .NES. defans
$ THEN
$   lognam_change = 1
$   IF F$LOCATE(",MX_NODE_NAME", changed_lognams) .EQ. F$LENGTH (changed_lognams) THEN -
    	changed_lognams = changed_lognams + ",MX_NODE_NAME"
$ ENDIF
$!
$!  If SYS$TIMEZONE_DIFFERENTIAL is defined, MX will use
$!  it instead of anything else, so we needn't bother with
$!  the timezone question.
$!
$ IF F$TRNLNM("SYS$TIMEZONE_DIFFERENTIAL") .NES. ""
$ THEN
$   mx_timezone = ""
$   IF F$TRNLNM("MX_TIMEZONE") .NES. "" THEN GOTO TZ_Ok  ! to clear it
$   TYPE SYS$INPUT:

    MX timezone information will be taken from the system timezone setting
    in the logical name SYS$TIMEZONE_DIFFERENTIAL.

$   GOTO TZ_Skip
$ ENDIF
$ IF F$SEARCH("SYS$MANAGER:UTC$CONFIGURE_TDF.COM") .NES. "" THEN -
    GOTO Explain_TDF
$!
$ TYPE SYS$INPUT:

    Timezone Setting
    ----------------

    Outgoing messages must include a timezone specification in
    their date/time stamps.

    Please enter a timezone specification, either "GMT" or a
    numeric timezone specification of the form [+|-]hhmm,
    where "hh" is the number of hours and "mm" is the number
    of minutes your timezone is ahead (+) or behind (-) GMT.

$ origval = "''mx_timezone'"
$ defans  = origval
$ IF defans .EQS. "" THEN defans = F$TRNLNM("MX_TIMEZONE","LNM$SYSTEM",,"EXECUTIVE")
$ GOTO TZ_ask
$TZ_reprompt:
$   say "Please enter GMT or a timezone specification of the form [+|-]hhmm."
$TZ_ask:
$ IF defans .EQS. ""
$ THEN  promptdef = ""
$ ELSE  promptdef = " [''defans']"
$ ENDIF
$ ask/prompt="Enter timezone string''promptdef': " mx_timezone
$ mx_timezone = F$EDIT(mx_timezone, "TRIM,COMPRESS,UPCASE")
$ IF mx_timezone .EQS. "" THEN mx_timezone = defans
$ IF mx_timezone .NES. "GMT" .AND. F$LENGTH(mx_timezone) .NE. 5 THEN GOTO tz_reprompt
$ IF mx_timezone .EQS. "GMT" THEN GOTO TZ_ok
$ tmp = F$EXTRACT(0,1,mx_timezone)
$ IF tmp .NES. "-" .AND. tmp .NES. "+" THEN GOTO TZ_reprompt
$ i = 0
$TZ_check_loop:
$ i = i + 1
$ tmp = F$EXTRACT(i,1,mx_timezone)
$ IF tmp .LTS. "0" .OR. tmp .GTS. "9" THEN GOTO TZ_reprompt
$ IF i .LT. 4 THEN GOTO TZ_check_loop
$ IF origval .NES. "" .AND. mx_timezone .EQS. origval THEN GOTO TZ_Skip
$TZ_ok:
$ lognam_change = 1
$ IF F$LOCATE(",MX_TIMEZONE", changed_lognams) .EQ. F$LENGTH (changed_lognams) THEN -
    	changed_lognams = changed_lognams + ",MX_TIMEZONE"
$!
$TZ_Skip:
$ IF .NOT. installing THEN GOTO Main_Menu
$!
$Step_3:
$!
$ process_change = 1
$ cluster = F$GETSYI("CLUSTER_MEMBER")
$!
$ TYPE SYS$INPUT:

    MX Message Queue Manager Agent
    ------------------------------

    If this MX installation will be handling a large volume of messages,
    it is recommended that you run a separate Queue Manager agent to
    maintain the message queue.  For small installations, the separate
    Queue Manager is not required, since the Router Agent can also perform
    queue management.

$ cur_process = "FLQ_MGR"
$ cur_descrip = "Queue Manager"
$ mult_procs  = 0
$ GOSUB Ask_Agent_Questions
$!      
$ TYPE SYS$INPUT:

    MX Processing Agents
    --------------------

    You will now be asked to specify startup information for the other
    installed MX processing agents.

$ cur_process = "ROUTER"
$ cur_descrip = "Message Router"
$ mult_procs  = 1
$ GOSUB Ask_Agent_Questions
$!
$ cur_process = "LOCAL"
$ cur_descrip = "Local Delivery Agent"
$ mult_procs  = 1
$ GOSUB Ask_Agent_Questions
$!
$ IF F$SEARCH("MX_ROOT:[*EXE]MX_SMTP.EXE") .NES. ""
$ THEN
$   cur_process = "SMTP"
$   cur_descrip = "SMTP Delivery Agent"
$   mult_procs  = 1
$   GOSUB Ask_Agent_Questions
$!
$   cur_process = "SMTP_SERVER"
$   cur_descrip = "SMTP Server"
$   mult_procs  = 0
$   GOSUB Ask_Agent_Questions
$   GOSUB Ask_SMTP_Server_Connections
$ ELSE
$   IF F$TYPE(mx_startup_nodes_smtp) .NES. "" THEN DELETE/SYMBOL mx_startup_nodes_smtp
$ ENDIF
$!
$ IF F$SEARCH("MX_ROOT:[*EXE]MX_DNSMTP.EXE") .NES. ""
$ THEN
$   cur_process = "DNSMTP"
$   cur_descrip = "SMTP-over-DECnet Delivery Agent"
$   mult_procs  = 1
$   GOSUB Ask_Agent_Questions
$ ELSE
$   IF F$TYPE(mx_startup_nodes_dnsmtp) .NES. "" THEN DELETE/SYMBOL mx_startup_nodes_dnsmtp
$ ENDIF
$!
$ IF F$SEARCH("MX_ROOT:[*EXE]MX_SITE.EXE") .NES. ""
$ THEN
$   cur_process = "SITE"
$   cur_descrip = "SITE Delivery Agent"
$   mult_procs  = 1
$   GOSUB Ask_Agent_Questions
$ ELSE
$   IF F$TYPE(mx_startup_nodes_site) .NES. "" THEN DELETE/SYMBOL mx_startup_nodes_site
$ ENDIF
$!
$ IF F$SEARCH("MX_ROOT:[*EXE]MX_MLF.EXE") .NES. ""
$ THEN
$   cur_process = "MLF"
$   cur_descrip = "Mailing List/File Server Agent"
$   mult_procs  = 0
$   GOSUB Ask_Agent_Questions
$ ELSE
$   IF F$TYPE(mx_startup_nodes_mlf) .NES. "" THEN DELETE/SYMBOL mx_startup_nodes_mlf
$ ENDIF
$!
$ IF .NOT. installing THEN GOTO Main_Menu
$!
$Step_4:
$!
$ IF p1 .EQS. "INSTALL" THEN @MX_DIR:MX_CREATE_CONFIG_DATABASE
$!
$ IF .NOT. installing THEN GOTO Main_Menu
$!
$Step_5:
$!
$ defans = "YES"
$ask_save:
$ IF .NOT. lognam_change .AND. .NOT. process_change THEN GOTO Skip_Save
$ ask/prompt="Save agent and logical name configuration changes? [''defans']: " yesno
$ IF yesno .EQS. "" THEN yesno = defans
$ yesno = F$EDIT(yesno, "TRIM,COMPRESS,UPCASE")
$ IF F$LOCATE(yesno,"YES") .NE. 0 .AND. F$LOCATE(yesno, "NO") .NE. 0
$ THEN
$   say "Please answer YES or NO."
$   GOTO ask_save
$ ENDIF
$ IF yesno
$ THEN
$   GOSUB Save_Current_Configuration
$   GOTO Depart
$ ENDIF
$Skip_Save:
$ IF installing
$ THEN
$   TYPE SYS$INPUT:

    You should execute this configuration procedure after the
    installation process completes by executing the command

        $ @MX_DIR:MXCONFIG

    If this is a new installation of MX, you MUST run this
    configuration procedure for MX to start properly.

$ ENDIF
$ GOTO Depart
$!
$Ask_Agent_Questions:
$ IF F$TYPE(mx_startup_nodes_'cur_process') .EQS. ""
$ THEN node_list = "*"
$ ELSE node_list = mx_startup_nodes_'cur_process'
$ ENDIF
$!
$ defans = "YES"
$ IF node_list .EQS. "" THEN defans = "NO"
$!
$ask_agent:
$ ask/prompt="Do you want to run the MX ''cur_descrip'? [''defans']: " yesno
$ IF yesno .EQS. "" THEN yesno = defans
$ yesno = F$EDIT(yesno, "TRIM,COMPRESS,UPCASE")
$ IF F$LOCATE(yesno,"YES") .NE. 0 .AND. F$LOCATE(yesno, "NO") .NE. 0
$ THEN
$   say "Please answer YES or NO."
$   GOTO ask_agent
$ ENDIF
$ IF .NOT. yesno
$ THEN
$   mx_startup_nodes_'cur_process' = ""
$   RETURN
$ ENDIF
$ IF .NOT. cluster
$ THEN
$   mx_startup_nodes_'cur_process' = "*"
$   GOTO ask_mult_procs
$ ENDIF
$ defans = node_list
$ask_agent_nodes:
$ ask/prompt="Enter names of VMScluster nodes to run MX ''cur_descrip' [''defans']: " node_list
$ node_list = F$EDIT(node_list,"TRIM,COLLAPSE,UPCASE")
$ IF node_list .EQS. "" THEN node_list = defans
$ IF node_list .EQS. "*"
$ THEN
$   mx_startup_nodes_'cur_process' = "*"
$   GOTO ask_mult_procs
$ ENDIF
$ i = -1
$ tmp = ""
$ask_agent_node_loop:
$ i = i + 1
$ node = F$ELEMENT(i, ",", node_list)
$ IF node .EQS. "," THEN GOTO end_aan_loop
$ node = F$EDIT(F$EXTRACT(0,6,node),"COLLAPSE,UPCASE")
$ IF .NOT. F$GETSYI("CLUSTER_MEMBER", node) THEN -
    say "%MXCONFIG-I-NOTMEMBER, node ''node' is not currently a member of this VMScluster"
$ IF tmp .EQS. ""
$ THEN tmp = node
$ ELSE tmp = tmp + "," + node
$ ENDIF
$ GOTO ask_agent_node_loop
$end_aan_loop:
$ mx_startup_nodes_'cur_process' = tmp
$ask_mult_procs:
$ node_list = mx_startup_nodes_'cur_process'
$ i = -1
$mult_proc_loop:
$ i = i + 1
$ node = F$ELEMENT(i,",",node_list)
$ IF node .EQS. "," THEN GOTO end_mult_proc
$ IF .NOT. mult_procs
$ THEN
$   IF node .EQS. "*"
$   THEN mx_stup_'cur_process'_asterisk_count = 1
$   ELSE mx_stup_'cur_process'_'node'_count   = 1
$   ENDIF
$   GOTO mult_proc_loop
$ ENDIF
$!
$ IF node .EQS. "*"
$ THEN
$   IF F$TYPE(mx_stup_'cur_process'_asterisk_count) .EQS. ""
$   THEN defans = "1"
$   ELSE defans = mx_stup_'cur_process'_asterisk_count
$   ENDIF
$ ELSE
$   IF F$TYPE(mx_stup_'cur_process'_'node'_count) .EQS. ""
$   THEN defans = "1"
$   ELSE defans = mx_stup_'cur_process'_'node'_count
$   ENDIF
$ ENDIF
$!
$ IF node .EQS. "*"
$ THEN prompt = "How many MX ''cur_descrip' processes should be started? [''defans']: "
$ ELSE prompt = "How many MX ''cur_descrip' processes should be started on node ''node'? [''defans']: "
$ ENDIF
$ ask/prompt="''prompt'" count
$ IF count .EQS. "" THEN count = defans
$ IF F$INTEGER(count) .GT. 99 THEN count = "99"
$ IF node .EQS. "*"
$ THEN mx_stup_'cur_process'_asterisk_count = F$INTEGER(count)
$ ELSE mx_stup_'cur_process'_'node'_count   = F$INTEGER(count)
$ ENDIF
$ GOTO mult_proc_loop
$!
$end_mult_proc:
$ RETURN
$!
$Ask_SMTP_Server_Connections:
$ TYPE SYS$INPUT:

    SMTP Server Connections
    -----------------------

    The MX SMTP Server can be configured to handle from 1 to 36
    simultaneous incoming connections.  In VMScluster environments,
    this configuration setting affects all nodes running the
    SMTP server.

$ defans = "''mx_smtp_server_threads'"
$ defans1 = defans
$ IF defans1 .EQS. "" THEN defans1 = F$TRNLNM("MX_SMTP_SERVER_THREADS","LNM$SYSTEM",,"EXECUTIVE")
$ IF defans1 .EQS. "" THEN defans1 = "4"
$!
$ask_smtpsrv_count:
$ ask/prompt="Enter maximum number of SMTP server connections to allow [''defans1']: " count
$ count = F$EDIT(count,"TRIM,COMPRESS")
$ IF count .EQS. "" THEN count = defans1
$ mx_smtp_server_threads = F$INTEGER(count)
$ IF mx_smtp_server_threads .LT. 1 .OR. mx_smtp_server_threads .GT. 36
$ THEN
$   say "Please enter a number from 1 to 36."
$   GOTO ask_smtpsrv_count
$ ENDIF
$ mx_smtp_server_threads = F$STRING(mx_smtp_server_threads)
$ IF mx_smtp_server_threads .NES. defans
$ THEN
$   lognam_change = 1
$   IF F$LOCATE (",MX_SMTP_SERVER_THREADS",changed_lognams) .EQ. F$LENGTH (changed_lognams) THEN -
    	changed_lognams = changed_lognams + ",MX_SMTP_SERVER_THREADS"
$ ENDIF
$ RETURN
$!
$Read_Current_Configuration:
$!
$ IF F$SEARCH("MX_DIR:MX_LOGICALS.DAT") .EQS. "" THEN GOTO Read_Startup_Info
$ OPEN/READ in MX_DIR:MX_LOGICALS.DAT
$read_lognams:
$ READ/END=readlog_end in line
$ lognam = F$ELEMENT (0,"\",line)
$ IF lognam .EQS. "\" THEN GOTO read_lognams
$ 'lognam'_flags = F$ELEMENT (1, "\", line)
$ IF 'lognam'_flags .EQS. "" THEN 'lognam'_flags = "/SYSTEM/EXEC"
$ 'lognam' = F$ELEMENT (2, "\", line)
$ IF 'lognam' .EQS. "\" THEN 'lognam' = ""
$ GOTO read_lognams
$readlog_end:
$ CLOSE in
$!
$Read_Startup_Info:
$ IF F$SEARCH("MX_DIR:MX_STARTUP_INFO.DAT") .EQS. "" THEN RETURN
$!
$ OPEN/READ stup MX_DIR:MX_STARTUP_INFO.DAT
$rcc_read_loop:
$ READ/END=rcc_end stup stupline
$ stupline = F$EDIT(F$EXTRACT(3,-1,stupline),"TRIM,COMPRESS,UPCASE")
$ process = F$ELEMENT(0,":",stupline)
$ nlist = F$ELEMENT(1,":",stupline)
$ IF nlist .EQS. ":" THEN nlist = ""
$ i = -1
$ tmp = ""
$rcc_parse_loop:
$ i = i + 1
$ nspec = F$ELEMENT(i,",",nlist)
$ IF nspec .EQS. "," THEN GOTO end_rcc_parse
$ node = F$ELEMENT(0,"=",nspec)
$ count = F$ELEMENT(1,"=",nspec)
$ IF count .EQS. "=" THEN count = 1
$ IF node .EQS. "*"
$ THEN
$   tmp = "*"
$   mx_stup_'process'_asterisk_count = count
$   GOTO end_rcc_parse
$ ENDIF
$ mx_stup_'process'_'node'_count = count
$ IF tmp .EQS. ""
$ THEN tmp = node
$ ELSE tmp = tmp + "," + node
$ ENDIF
$ GOTO rcc_parse_loop
$end_rcc_parse:
$ mx_startup_nodes_'process' = tmp
$ GOTO rcc_read_loop
$rcc_end:
$ CLOSE stup
$ RETURN
$!
$Save_Current_Configuration:
$!
$ SET NOCONTROL=Y
$ IF .NOT. lognam_change THEN GOTO Skip_Lognam_Save
$ OPEN/READ in MX_DIR:MX_LOGICALS.DAT
$ CREATE MX_DIR:MX_LOGICALS.DAT
$ OPEN/APPEND out MX_DIR:MX_LOGICALS.DAT
$saveloop:
$ READ/END=end_save in inline
$ lognam = F$ELEMENT(0,"\",inline)
$ IF F$EDIT (lognam, "UPCASE") .EQS. "MX_VMSMAIL_LOCALHOST" THEN GOTO saveloop
$ IF F$TYPE('lognam') .EQS. ""
$ THEN
$   WRITE out inline
$ ELSE
$   IF F$TYPE('lognam'_flags) .EQS. "" THEN 'lognam'_flags = F$ELEMENT (1,"\",inline)
$   IF 'lognam' .NES. "" THEN WRITE out lognam,"\",'lognam'_flags,"\",'lognam'
$   i = F$LOCATE (",''lognam'", changed_lognams)
$   IF i .LT. F$LENGTH (changed_lognams)
$   THEN
$   	changed_lognams = F$EXTRACT (0, i, changed_lognams) +-
    	    	    	  F$EXTRACT (i+F$LENGTH(lognam)+1, -1, changed_lognams)
$   ENDIF
$ ENDIF
$ GOTO saveloop
$end_save:
$ i = 0
$SaveLoop2:
$ i = i + 1
$ lognam = F$ELEMENT (i, ",", changed_lognams)
$ IF lognam .EQS. "," THEN GOTO End_Save2
$ IF 'lognam' .NES. "" THEN WRITE out lognam, "\","/SYSTEM/EXEC","\",'lognam'
$ GOTO SaveLoop2
$End_Save2:
$ CLOSE out
$ CLOSE in
$!
$Skip_Lognam_Save:
$!
$ IF .NOT. process_change THEN GOTO Skip_Process_Save
$ CREATE MX_DIR:MX_STARTUP_INFO.DAT
$ OPEN/APPEND stup MX_DIR:MX_STARTUP_INFO.DAT
$ IF F$TYPE(mx_startup_nodes_netlib) .EQS. "" .AND. -
    (F$TYPE(mx_startup_nodes_smtp) .NES. "" .OR. F$TYPE(mx_startup_nodes_smtp_server) .NES. "")
$ THEN
$   IF "''mx_startup_nodes_smtp'" .EQS. "*" .OR. "''mx_startup_nodes_smtp_server'" .EQS. "*"
$   THEN
$   	mx_startup_nodes_netlib = "*"
$   ELSE
$   	mx_startup_nodes_netlib = "''mx_startup_nodes_smtp'"
$   	IF "''mx_startup_nodes_smtp'" .NES. "''mx_startup_nodes_smtp_server'"
$   	THEN
$   	    IF mx_startup_nodes_netlib .NES. "" .AND. "''mx_startup_nodes_smtp_server'" .NES. "" THEN -
    	    	mx_startup_nodes_netlib = mx_startup_nodes_netlib + ","
$   	    mx_startup_nodes_netlib = mx_startup_nodes_netlib + "''mx_startup_nodes_smtp_server'"
$   	ENDIF
$   ENDIF
$ ENDIF
$ IF F$TYPE(mx_startup_nodes_netlib) .NES. ""
$ THEN
$   WRITE stup "001NETLIB:",mx_startup_nodes_netlib
$ ENDIF
$ IF F$TYPE(mx_startup_nodes_flq_mgr) .NES. ""
$ THEN
$   WRITE stup "002FLQ_MGR:",mx_startup_nodes_flq_mgr
$ ENDIF
$ IF F$TYPE(mx_startup_nodes_router) .NES. ""
$ THEN
$   cur_process = "ROUTER"
$   GOSUB buildstupline
$   WRITE stup "002ROUTER:",stupline
$ ENDIF
$ IF F$TYPE(mx_startup_nodes_local) .NES. ""
$ THEN
$   cur_process = "LOCAL"
$   GOSUB buildstupline
$   WRITE stup "003LOCAL:",stupline
$ ENDIF
$ IF F$TYPE(mx_startup_nodes_smtp) .NES. ""
$ THEN
$   cur_process = "SMTP"
$   GOSUB buildstupline
$   WRITE stup "004SMTP:",stupline
$ ENDIF
$ IF F$TYPE(mx_startup_nodes_smtp_server) .NES. ""
$ THEN
$   WRITE stup "004SMTP_SERVER:",mx_startup_nodes_smtp_server
$ ENDIF
$ IF F$TYPE(mx_startup_nodes_dnsmtp) .NES. ""
$ THEN
$   cur_process = "DNSMTP"
$   GOSUB buildstupline
$   WRITE stup "004DNSMTP:",stupline
$ ENDIF
$ IF F$TYPE(mx_startup_nodes_site) .NES. ""
$ THEN
$   cur_process = "SITE"
$   GOSUB buildstupline
$   WRITE stup "004SITE:",stupline
$ ENDIF
$ IF F$TYPE(mx_startup_nodes_mlf) .NES. ""
$ THEN
$   WRITE stup "005MLF:",mx_startup_nodes_mlf
$ ENDIF
$ CLOSE stup
$Skip_Process_Save:
$ SET CONTROL=Y
$ RETURN
$buildstupline:
$ stupline = ""
$ IF mx_startup_nodes_'cur_process' .EQS. ""
$ THEN
$   stupline = ""
$   RETURN
$ ENDIF
$ j = -1
$bslloop:
$ j = j + 1
$ node = F$ELEMENT(j,",",mx_startup_nodes_'cur_process')
$ IF node .EQS. "," THEN RETURN
$ IF node .EQS. "*"
$ THEN
$   IF F$INTEGER(mx_stup_'cur_process'_asterisk_count) .LE. 1
$   THEN stupline = "*=1"
$   ELSE stupline = F$FAO("*=!UL",F$INTEGER(mx_stup_'cur_process'_asterisk_count))
$   ENDIF
$   RETURN
$ ENDIF
$ count = F$INTEGER(mx_stup_'cur_process'_'node'_count)
$ IF count .EQ. 0 THEN count = 1
$ IF stupline .EQS. ""
$ THEN stupline = "''node'=''count'"
$ ELSE stupline = stupline + "," + "''node'=''count'"
$ ENDIF
$ GOTO bslloop
