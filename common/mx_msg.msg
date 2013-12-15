.TITLE		MX_MSG
.FACILITY	MX,1063/PREFIX=MX__
.IDENT		"V2.2"
!++
!
! DESCRIPTION:
!
!   These are the message codes used by the MX facility.
!
! AUTHOR:	M. Madison
!
! Copyright (c) 2008, Matthew Madison.
! Copyright (c) 2012, Endless Software Solutions.
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
! CREATION DATE: 11-DEC-1989
!
! MODIFICATION HISTORY:
!
!   11-DEC-1989	V1.0	Madison	    Initial coding.
!   27-JUN-1990	V1.1-1	Madison	    Add CFGERR.
!   14-DEC-1990	V1.2	Madison	    Add QRECLERR
!   08-FEB-1991	V1.2-1	Madison	    Add QERROR.
!   25-OCT-1991	V1.3	Madison	    Add UNKSET.
!   15-NOV-1991	V1.3-1	Madison	    Add MEMALLOC.
!   29-Oct-1992 V1.4	Munroe	    Add SETACCESS.
!   19-JAN-1993 V1.5	Goatley     Add FLQLCK.
!   26-JAN-1993 V1.5-1	Goatley     Add FLQERR for the rest.
!   10-APR-1993 V1.6	Goatley     Add MAIDLVR, BASE64, RFC822.
!   16-DEC-1993 V1.7	Goatley	    Add NOMAILNOW, NOFAKE, NOACCESS.
!    7-JAN-1994 V1.7-1	Goatley	    Add MM_RETRY.
!   28-MAR-1994 V1.7-2	Goatley     Add MX__LISTOWNER.
!    3-JUN-1994 V1.8	Goatley	    Add MX__NODNSRESPONSE.
!   15-JUN-1994 V1.8-1	Goatley     Add MX__BADRFC821ADDR.
!   11-SEP-1995 V1.9	Goatley     Add MX__MAILLISTD, MX__NODIGEST.
!   07-MAY-1997	V1.10	Madison	    Add MX__HOSTLOCK, MX__ADDRLOCK
!   29-AUG-1997	V1.11	Madison	    Add MX__PROCERR
!   16-JUL-1998	V2.0	Madison	    New codes to support DSN and size checks.
!   26-NOV-2000	V2.1	Madison	    New userauth codes.
!   12-MAR-2012	V2.2	Sneddon	    Add TLSINIERR.
!--

.SEVERITY   	FATAL
RWINITERR   	<error reading routing information file>
AGENTALRDY    	<agent !AS already running>/FAO=1
NONET	    	<TCP/IP network unavailable>
NOHOST	    	<no such host>
RETRYEXCD   	<retry count exceeded>
MEMALLOC    	<error allocating memory from zone !AS>/FAO=1

.SEVERITY   	ERROR
NONETADR    	<no network address for host>
PROTOERR    	<protocol error?>
NOCOMPLETE  	<message transmission could not be completed>
DISMAIL	    	<user has mail privileges disabled>
FWDLOOP	    	<forwarding loop detected>
INVADDR	    	<invalid mail address>
CFGERR	    	<version mismatch in MX configuration>
QERROR	    	<error accessing system message queue>
MSGTXTERR   	<error entering message text>

.SEVERITY   	WARNING
NOMATCH	    	<no strings matched>
NOTSUB	    	<address is not on list of subscribers>
QRECLERR	<error occurred during CONV$RECLAIM on message queue>
UNKSET	    	<unknown SET command parameter>
SETACCESS	<SET is not valid for access control subscribers>
HOSTLOCK    	<target host is temporarily locked against connection attempts>
ADDRLOCK    	<target address is temporarily locked against connection attempts>

.SEVERITY   	INFO
ALRSUB	    	<user is already subscribed to list>
FLQLCK		<MX system queue file locked for maintenance; try again>
FLQERR		<error accessing MX system queue file>

.SEVERITY   	SUCCESS
FWDUSER	    	<user forwarding mail to address !AD>/FAO=2
LCLUSER	    	<local user exists>
MAILLIST    	<name represents mailing list>
MLCTRL	    	<name represents mailing list control address>
FILEREQ	    	<message is a FILESERV request>

.SEVERITY	INFO
MAIDLVR		<message (entry number !UL) successfully delivered to MX>/fao=1
BASE64		<encoding MX foreign message using BASE64>
RFC822		<using RFC822 headers from input file>

.SEVERITY	ERROR
NOMAILNOW	<your system manager has temporarily disabled MX>
NOFAKE		<you are not authorized to resend SMTP files using MX>
NOACCESS	<you are not authorized to send mail using MX>
MM_RETRY	<error locking MM init file>

.SEVERITY	SUCCESS
LISTOWNER	<name represents mailing list owner>

.SEVERITY	FATAL
NODNSRESPONSE	<no response from DNS server>
BADRFC821ADDR	<bad RFC821 address passed from MX Router---check rewrite rules>

.SEVERITY	SUCCESS
MAILLISTD    	<name represents mailing list digest>

.SEVERITY	WARNING
NODIGEST	<list does not support digests>
PROCERR	    	<internal list processing error>

DSN_S__OTHER	<2.0.0 (Unspecified success status)>/SUCCESS
DSN_W__OTHER	<4.0.0 (Unspecified transient failure)>/WARNING
DSN_F__OTHER	<5.0.0 (Unspecified permanent failure)>/FATAL

DSN_SA_OTHER 	<2.1.0 (Other address status)>/SUCCESS
DSN_WA_OTHER 	<4.1.0 (Other address status)>/WARNING
DSN_FA_OTHER 	<5.1.0 (Other address status)>/FATAL
DSN_FA_BADMBX	<5.1.1 (Bad destination mailbox address)>/FATAL
DSN_FA_BADSYS	<5.1.2 (Bad destination system address)>/FATAL
DSN_FA_DSTSYN	<5.1.3 (Bad destination mailbox address syntax)>/FATAL
DSN_WA_DSTAMB	<4.1.4 (Destination mailbox address ambiguous)>/WARNING
DSN_SA_DSTVAL	<2.1.5 (Destination mailbox address valid)>/SUCCESS
DSN_WA_MBXMOV	<5.1.6 (Mailbox has moved; no fowarding address)>/FATAL
DSN_FA_SNDSYN	<5.1.7 (Bad sender's mailbox address syntax)>/FATAL
DSN_FA_SNDSYS	<5.1.8 (Bad sender's system address)>/FATAL

DSN_SM_OTHER	<2.2.0 (Other or undefined mailbox status)>/SUCCESS
DSN_WM_OTHER	<4.2.0 (Other or undefined mailbox status)>/WARNING
DSN_FM_OTHER	<5.2.0 (Other or undefined mailbox status)>/FATAL
DSN_WM_DISABL	<4.2.1 (Mailbox disabled, not accepting messages)>/WARNING
DSN_FM_DISABL	<5.2.1 (Mailbox disabled, not accepting messages)>/FATAL
DSN_WM_MBXFUL   <4.2.2 (Mailbox full)>/WARNING
DSN_FM_ADMLIM	<5.2.3 (Message length exceeds administrative limit)>/FATAL
DSN_WM_EXPANS	<4.2.4 (Mailing list expansion problem)>/WARNING
DSN_FM_EXPANS	<5.2.4 (Mailing list expansion problem)>/FATAL

DSN_SS_OTHER	<2.3.0 (Other or undefined mail system status)>/SUCCESS
DSN_WS_OTHER	<4.3.0 (Other or undefined mail system status)>/WARNING
DSN_FS_OTHER	<5.3.0 (Other or undefined mail system status)>/FATAL
DSN_WS_SYSFUL	<4.3.1 (Mail system full)>/WARNING
DSN_WS_NOTACC	<4.3.2 (System not accepting network messages)>/WARNING
DSN_FS_NOTACC	<5.3.2 (System not accepting network messages)>/FATAL
DSN_FS_CAPABL	<5.3.3 (System not capable of selected features)>/FATAL
DSN_FS_TOOBIG	<5.3.4 (Message too big for system)>/FATAL
DSN_FS_CONFIG	<5.3.5 (System is incorrectly configured)>/FATAL

DSN_SR_OTHER	<2.4.0 (Other or undefined network or routing status)>/SUCCESS
DSN_WR_OTHER	<4.4.0 (Other or undefined network or routing status)>/WARNING
DSN_FR_OTHER	<5.4.0 (Other or undefined network or routing status)>/FATAL
DSN_WR_NOANSR	<4.4.1 (No answer from host)>/WARNING
DSN_WR_BADCON	<4.4.2 (Bad connection)>/WARNING
DSN_WR_DSFAIL	<4.4.3 (Directory server failure)>/WARNING
DSN_WR_UNARTE	<4.4.4 (Unable to route message)>/WARNING
DSN_FR_UNARTE	<5.4.4 (Unable to route message)>/FATAL
DSN_WR_CONGES	<4.4.5 (Network or mail system congestion)>/WARNING
DSN_WR_RTLOOP	<4.4.6 (Routing loop detected)>/WARNING
DSN_WR_EXPIRE	<4.4.7 (Delivery time expired)>/WARNING

DSN_SP_OTHER	<2.5.0 (Other or undefined protocol status)>/SUCCESS
DSN_WP_OTHER	<4.5.0 (Other or undefined protocol status)>/WARNING
DSN_FP_OTHER	<5.5.0 (Other or undefined protocol status)>/FATAL
DSN_FP_INVCMD	<5.5.1 (Invalid command)>/FATAL
DSN_FP_SYNERR	<5.5.2 (Syntax error)>/FATAL
DSN_FP_TOMANY	<5.5.3 (Too many recipients)>/FATAL
DSN_FP_INVARG	<5.5.4 (Invalid command arguments)>/FATAL
DSN_FP_MISMAT	<5.5.5 (Wrong protocol version)>/FATAL

DSN_SC_OTHER	<2.6.0 (Other or undefined media/content status)>/SUCCESS
DSN_WC_OTHER	<4.6.0 (Other or undefined media/content status)>/WARNING
DSN_FC_OTHER	<5.6.0 (Other or undefined media/content status)>/FATAL
DSN_FC_MNOSUP	<5.6.1 (Media not supported)>/FATAL
DSN_FC_CNVPRO	<5.6.2 (Conversion required and prohibited)>/FATAL
DSN_FC_CNVUNS	<5.6.3 (Conversion required but not supported)>/FATAL
DSN_SC_CNVLOS	<2.6.4 (Conversion with loss performed)>/SUCCESS
DSN_FC_CNVLOS	<5.6.4 (Conversion with loss performed)>/FATAL
DSN_WC_CNVFAI	<4.6.5 (Conversion failed)>/WARNING

DSN_SY_OTHER	<2.7.0 (Other or undefined security status)>/SUCCESS
DSN_WY_OTHER	<4.7.0 (Other or undefined security status)>/WARNING
DSN_FY_OTHER	<5.7.0 (Other or undefined security status)>/FATAL
DSN_FY_NOAUTH	<5.7.1 (Delivery not authorized, message refused)>/FATAL
DSN_FY_NOEXPN	<5.7.2 (Mailing list expansion prohibited)>/FATAL
DSN_FY_NOCONV	<5.7.3 (Security conversion required but not possible)>/FATAL
DSN_FY_NOSUPP	<5.7.4 (Security features not supported)>/FATAL
DSN_WY_CRPFAI	<4.7.5 (Cryptographic failure)>/WARNING
DSN_FY_CRPFAI	<5.7.5 (Cryptographic failure)>/FATAL
DSN_FY_ALGUNS	<5.7.6 (Cryptographic algorithm not supported)>/FATAL
DSN_SY_INTFAI	<2.7.7 (Message integrity failure)>/SUCCESS
DSN_WY_INTFAI	<4.7.7 (Message integrity failure)>/WARNING
DSN_FY_INTFAI	<5.7.7 (Message integrity failure)>/FATAL

INTERNAL_ERROR	<MX detected internal logic error>/FATAL
SHUTDOWN    	<MX shutdown in progress>/WARNING
NOCONTACT   	<could not establish contact with any mail servers for this destination>/WARNING
REJECTED    	<delivery rejected by administrative filter>/FATAL
NONJEGWY  	<gateway of non-BSMTP messages from NJE hosts is not permitted>/FATAL
NOPATH	    	<could not identify path for this address>/FATAL
ADDRALIAS   	<delivered to an alias address>/SUCCESS
MSGTOOLARGE 	<message is too large to be accepted by the mail system>/ERROR
BADDSNCTL   	<DSN control settings are invalid and have been ignored>/INFORMATIONAL
AUTHFAIL    	<user authentication failed>/ERROR
UAEXISTS    	<user already in authentication database>/WARNING
UANOTFOUND  	<user not found in authentication database>/WARNING
UAINVPWD    	<invalid password specified>/ERROR
TLSINIERR	<error initializing TLS>/ERROR

.END

