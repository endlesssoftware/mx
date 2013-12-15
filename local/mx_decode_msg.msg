.TITLE		MX_DECODE_MSG
.FACILITY	MXD,1/PREFIX=MXD__
.IDENT		"V1.1"
!++
!
! DESCRIPTION:
!
!   These are the message codes used by the MX_DECODE facility.
!
! AUTHOR:	Hunter Goatley
!
! Copyright (c) 2008, Matthew Madison and Hunter Goatley.
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
! CREATION DATE: 11-OCT-1996
!
! MODIFICATION HISTORY:
!
!   11-OCT-1996 V1.0	Goatley	    Initial coding.
!   26-APR-1997	V1.1	Madison	    More generalized.
!--

.SEVERITY   	FATAL
NOHEADERS	<MIME mail headers not found; use /NOHEADERS and /TEXT or /IMAGE>
GENERR		<error decoding MIME message>
UNRECHDRS   	<unrecognized content type or encoding headers>

.SEVERITY   	INFO
BEGIN		<decoding !AS file !AS....>/FAO=2
END		<file successfully decoded to !AS>/FAO=1

.END
