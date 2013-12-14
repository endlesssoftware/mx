	.TITLE	MXALIAS_MSG
	.IDENT	/01-001/
!++
!
!  File:	MXALIAS_MSG.MSG
!
!  Author:	Hunter Goatley
! Copyright (c) 2008, Hunter Goatley.
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
!  Date:	January 16, 1993
!
!  Description:
!
!	Message file for MXALIAS.B32.
!
!
!  Modification history:
!
!	01-001		Hunter Goatley		25-APR-1993 17:08
!		Added MXALIAS__INVBGN.
!
!	01-000		Hunter Goatley		16-JAN-1993 23:57
!		Genesis.
!
!--
	.FACILITY	MXALIAS,1/PREFIX=MXALIAS__

	.SEVERITY	INFORMATIONAL

FILCREAT	<created MX alias database !AS>/fao_count=1
ADDED		<added alias !AS to MX alias database>/fao_count=1
MODIFIED	<modified alias !AS>/fao_count=1
REMOVED		<removed alias !AS>/fao_count=1
ALIASFIL	<current MX alias database is !AS>/fao_count=1

	.SEVERITY	WARNING
ALRDY		<alias is already defined>
LNMEXISTS	<a logical name is defined that matches alias !AS>/fao_count=1

	.SEVERITY	ERROR
MODERR		<error modifying alias !AS>/fao_count=1
ADDERR		<error adding alias !AS>/fao_count=1
REMERR		<error removing alias !AS>/fao_count=1
NOSUCHID	<alias !AS is not defined>/fao_count=1
SHONONE		<no aliases found matching !AS>/fao_count=1
SHOERR		<error showing aliases matching !AS>/fao_count=1
NOMODS		<nothing to modify; please specify /ADDRESS and/or /DESCRIPTION>
INVALIAS	<invalid alias specification>
INVADDR		<invalid address specification: !AS>/fao_count=1
ALTOOLONG	<alias specification is too long (!UB characters maximum)>/fao=1
ADTOOLONG	<e-mail address is too long (!UB characters maximum)>/fao=1
COTOOLONG	<comment is too long (!UB characters maximum)>/fao_count=1
ALINVCHAR	<alias specification contains invalid characters>
NOOPNOUT	<error opening output file !AS>/fao_count=1
ERROPN		<error opening MX alias database !AS>/fao_count=1
INVFILE		<alias database !AS has an invalid file format>/fao_count=1
INVBGN		<alias cannot begin with character '!AS'>/fao_count=1

	.END
