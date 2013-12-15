.TITLE	    MCP_MSG
.FACILITY   MCP,1144/PREFIX=MCP__
.IDENT	    "V1.12"
!+
! DESCRIPTION:
!
!   This module contains message codes used by the MCP utility.
!
! AUTHOR:
!
!   M. Madison
!
! Copyright (c) 2008, Matthew Madison.
! 
! All rights reserved.
! 
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions
! are met:
! 
!     * Redistributions of source code must retain the above
!       copyright notice, this list of conditions and the following
!       disclaimer.
!     * Redistributions in binary form must reproduce the above
!       copyright notice, this list of conditions and the following
!       disclaimer in the documentation and/or other materials provided
!       with the distribution.
!     * Neither the name of the copyright owner nor the names of any
!       other contributors may be used to endorse or promote products
!       derived from this software without specific prior written
!       permission.
! 
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
! A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
! OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
! SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
! LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
! DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
! THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
! UPDATE HISTORY:
!
!   13-DEC-1989	V1.0	Madison	    Initial coding.
!   12-JAN-1990	V1.1-1	Madison	    FILESERV support added.
!   11-OCT-1990	V1.1-2	Madison	    Add SHUTDOWN messages.
!   15-OCT-1990	V1.1-3	Madison	    Add MCP__FORCED.
!   16-OCT-1991	V1.2	Madison	    Add Q stuff.
!   04-MAR-1992	V1.2-1	Madison	    NOAGENTS.
!   21-DEC-1993 V1.3	Goatley     QNOSUPRT, QSYNCH*.
!    7-JAN-1994 V1.3-1	Goatley     INVREWRULE.
!   17-JAN-1994 V1.3-2	Goatley     Messages for QUEUE COMPRESS.
!   26-JAN-1994 V1.3-3	Goatley     INVALQ, INVDEQ.
!   16-FEB-1994 V1.4	Goatley	    QNEWQ*.
!   15-MAR-1994 V1.4-1	Goatley     QSTATUS.
!    5-MAY-1994 V1.5	Altmayer    QNPURGED.
!    3-JUN-1994 V1.5-1	Goatley     Add highest percentage to QSTATUS.
!   15-JUN-1994 V1.5-2	Goatley     INV821ADDR.
!   17-NOV-1995 V1.6	Goatley	    UNKNODE, NONODES.
!    4-DEC-1995 V1.6-1	Goatley     INVLSTNAM.
!   02-APR-1997	V1.7	Madison	    INVRCPMAX.
!   29-AUG-1997 V1.7-1	Goatley	    Changed QHOLD to "user hold".
!    8-SEP-1997 V1.7-2	Goatley	    ADTOOLONG.
!   27-AUG-1998	V1.8	Madison	    INVHOLDQ.
!   27-NOV-2000	V1.9	Madison	    INVMAXMSGSZ; remove NODIFIED.
!   03-DEC-2000	V1.9-1	Madison	    UAINVUNAME, UAINVPWD.
!   13-JAN-2001	V1.10	Madison	    QSELECTED, QSELERR.
!   19-FEB-2002 V1.11   Madison     REGEX, DUMP messages.
!   31-DEC-2004 V1.12   Madison     QSTATUS change.
!-
.SEVERITY   	ERROR
INVADDR	    	<invalid address specification: !AS>/FAO=1
INSFPRV	    	<you need !AS privilege for this command>/FAO=1
QUPDTERR    	<error updating queue entry number !UL>/FAO=1
QREADERR    	<error reading queue entry number !UL>/FAO=1
QOPENERR    	<error occurred opening system message queue>
QCOMPERR    	<error attempting COMPRESS on system message queue>
QNOSUPRT	<!AS is not supported in this version of MX>/fao=1
QSYNCHERR	<error synchronizing system message queue bitmap>
INVREWRULE	<invalid rewrite rule format (missing required angle bracket(s))!/  \!AD\>/FAO=2
INVFILNAM	<error in !AS file name syntax>/FAO=1
QCVTFAIL	<error creating new system message queue>
QINVSIZ		<invalid maximum number of entries; must be less than !UL>/FAO=1
QEXTFAIL	<error extending existing system message queue>
QUSECMP		<use QUEUE COMPRESS to make the message queue smaller>
INV821ADDR	<rewrite rule result is not RFC821-compliant!/  \!AD\>/FAO=2
NONODES		<no valid nodes specified>
INVLSTNAM	<illegal character(s) specified in list name>
ADTOOLONG	<alias address is too long (!UB characters maximum)>/fao=1
INVHOLDQ    	<invalid holding queue number !UL (range is from 1 to 8)>/FAO=1

.SEVERITY   	WARNING
NOMATCH	    	<no record matched specified !AS>/FAO=1
ALREADY	    	<matching !AS record already exists>/FAO=1
PRSPECERR   	<protection code specification error>
NOOWNER	    	<you must specify an owner for the list>
CANTIND	    	<failed to redirect input stream>
NORDCFG	    	<could not read configuration file !AS>/FAO=1
NOWRTCFG    	<could not write configuration file !AS>/FAO=1
NOOPNOUT    	<could not open output file !AS>/FAO=1
INVROOT	    	<invalid root specification !AS>/FAO=1
SHUTRESF    	<command !AS failed>/FAO=1
NOAGENTS    	<no such agent processes are running>
QEXTINV		<specified maximum !UL is less than existing !UL entries>/FAO=2
QMAXREQ		<the /MAXIMUM_ENTRIES qualifier is required>
UNKNODE		<invalid node name !AS specified>/FAO=1
INVRCPMAX   	<invalid count for /RECIPIENT_MAXIMUM - specify decimal number or DEFAULT>

.SEVERITY   	INFORMATION
READCFG	    	<read configuration from file !AS>/FAO=1
WROTECFG    	<wrote configuration to file !AS>/FAO=1
NOTIFIED    	<agent !AS has been notified!AS!AC>/FAO=3
FORCED	    	<forced exit of standby !AS process, pid !XL>/FAO=2
QSYNBGN		<synchronizing system message queue bitmap>
QBPASS1		<beginning pass 1 on old queue file....>
QBPASS2		<beginning pass 2 to renumber the queue entries....>
QPRCSNG		<processing entry !UL, new entry !UL>/FAO=2
QDIRREN		<renaming %.DIR files to OLD_%.DIR....>
QCREDIR		<creating new %.DIR files....>
QRENMSG		<renaming message files from [.OLD_%] to [.%]....>
QDELDIR		<deleting old %.DIR files....>
QDELFLQ		<deleting old MX_SYSTEM_QUEUE.FLQ_CTL file>
QNEWQFL		<creating new system message queue file for !UL entries....>
QNEWQDN		<new system message queue created for !UL entries>
QEXTQDN		<system message queue extended to allow !UL entries>
QSTATUS		<total entries: !UL/!UL (!UL%)>/FAO=3
QENTRIES	<total matching entries: !UL, distinct messages: !UL>/FAO=2
QNPURGED	<!UL entries purged>/FAO=1
NOLONGUSERUESD	<this message code is no longer used>

.SEVERITY   	SUCCESS
QCANCEL	    	<queue entry number !UL cancelled>/FAO=1
QREADY    	<queue entry number !UL readied>/FAO=1
QPURGE	    	<queue entry number !UL purged>/FAO=1
QRECLAIM    	<reclaimed a total of !UL buckets (!UL data, !UL index)>/FAO=3
QSYNCHED	<system message queue bitmap synchronized>
QCOMPNON	<no entries to copy; compression complete>
QHOLD	    	<queue entry number !UL marked as user hold>/FAO=1

INVMAXMSGSZ 	<invalid value for maximum message size; specify decimal number>/ERROR
UACREFAIL   	<creation of user authentication database failed>/ERROR
UAADDFAIL   	<could not add user to authentication database>/ERROR
UAMODFAIL   	<could not modify entry in authentication database>/ERROR
UAREMFAIL   	<could not remove entry from authentication database>/ERROR
UAINVUNAME  	<invalid username: too long or contains invalid characters>/ERROR
UAINVPWD    	<invalid password: too long>/ERROR
QSELECTED   	<!UL entr!AS selected>/FAO=2/INFORMATIONAL
QSELERR	    	<error in SELECT processing>/ERROR
INVREGEX        <invalid regular expression: !AD>/FAO=2/ERROR
INVDUMPSPEC     <invalid file specification !AS for DUMP; type and version not allowed>/FAO=1/ERROR
DUMPSTATUS      <queue entry !UL not READY or IN-PROGRESS>/FAO=1/ERROR
ENVREADERR      <error reading envelope information for queue entry !UL>/FAO=1/ERROR
HDRREADERR      <error reading header information for queue entry !UL>/FAO=1/ERROR
TXTOPNERR       <error opening message text for queue entry !UL>/FAO=1/ERROR
DMPOPNERR       <error opening !AS file>/FAO=1/ERROR
DMPWRTERR       <error writing !AS file>/FAO=1/ERROR

.SEVERITY       INFORMATION
QSYNCHQUEUE     <entry !UL status changed to !ASvalid in queue bitmap>/FAO=2

.END
