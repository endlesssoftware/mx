README file for MX Source Kit
March 2012

The MX sources are divided up into subdirectories.  Each subdirectory
contains a DESCRIP.MMS file for building the components in that
subdirectory.

To build MX, you will need:

   * MMK (the makefiles are MMK-specific and will not work with MMS)
   * the BLISS-32 compiler
   * the C compiler
   * DEC Document if you want to build the document files

To build a full distribution kit, you need to be able to build all
the binaries for Alpha and IA-64 systems.  I have removed the kitting
for VAX systems, although the source code still contains VAX support.

The makefiles are set up for "build-out" -- that is, most files generated
by the build are placed a directory tree under separate logical device
names (for which I use rooted logical names).  The top of the tree is
assumed to be [MX] by default, but you can define the symbol MX_TOP
to have it be a different name.   The logical device names are:

MG_SRC:         source tree
MG_BIN_AXP:     Alpha binaries
MG_BIN_I64:     IA64 binaries
MG_BIN_VAX:     VAX binaries
MG_BIN:         binaries for current architecture

MG_ETC_AXP:     Alpha-specific generated files (non-binary)
MG_ETC_I64:     IA64-specific generated files (non-binary)
MG_ETC_VAX:     VAX-specific generated files (non-binary)
MG_ETC:         generated files for current architecture

MG_KIT:         Installation kit working directory

MG_CMS:         CMS library tree (not required)

Here is a brief description of the contents of the subdirectories
in the source tree:

[.ALIAS]       source for MXALIAS utility.
[.COMMON]      common include files and libraries used throughout MX.
[.DOC]         DEC DOCUMENT sources for the documentation.
[.EXAMPLES]    various example files included with the MX kit.
[.FLQ]         file queuing library used throughout MX.
[.KIT]         installation kit files.
[.LOCAL]       local delivery agent.
[.MAILSHR]     VMS MAIL "foreign" mail transport interface.
[.MCP]         MX Control Program.
[.MLF]         mailing list/file server agent.
[.MLFAKE]      source for MLFAKE.
[.OPENSSL]     object kits for OpenSSL 1.0.0e
[.ROUTER]      source for message router.
[.SITE]        source for "SITE" entry and delivery agents.
[.SMTP]        source for SMTP server and agent (TCP/IP and DECnet)

If you have used older versions of MX, you may note that the Jnet,
SMTP-over-X.25, ListServ, and UUCP interfaces are missing.  I have
excluded thes from the kit because I don't know whether or not they
work any more. I have also removed all code relating to license keys,
as MX is now distributed as open source software.

To build MX binaries:

   1. Set up the logical names mentioned above.  You don't
      need the MG_CMS: logical name unless you've put the
      MX sources into CMS libraries.
   2. Set default to the top-level source directory and
      use MMK to build.

To build the installation kit, you need to make the binaries
for both Alpha and IA64 architectures first.  Once everything
else is built, use "MMK KIT" at the top-level directory to
build the complete installation kit.

Versioning:

For the open-source distribution, I have bumped the major version
of MX from 5 to 6.  Users of the commercial V5.x version of MX,
or the older V4.x freeware version, cannot upgrade directly to
the open-source version.  I did this because there was no way for
me to verify that upgrades would work properly.


------
COPYRIGHT NOTICE

Copyright (c) 2008, Matthew Madison.
Copyright (c) 2013, Endless Software Solutions.

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

