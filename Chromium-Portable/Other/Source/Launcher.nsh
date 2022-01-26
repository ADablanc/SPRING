################################################################################
# Chromium Portable - Launcher brand                                           #
# Copyright © 2010 Aluísio Augusto Silva Gonçalves. All rights reserved.       #
# ---------------------------------------------------------------------------- #
# Redistribution and use in source and binary forms, with or without           #
# modification, are permitted provided that the following conditions are met:  #
#                                                                              #
#    * Redistributions of source code must retain the above copyright notice,  #
#      this list of conditions and the following disclaimer.                   #
#    * Redistributions in binary form must reproduce the above copyright       #
#      notice, this list of conditions and the following disclaimer in the     #
#      documentation and/or other materials provided with the distribution.    #
#    * Neither the name of the copyright holder nor the names of its           #
#      contributors may be used to endorse or promote products derived from    #
#      this software without specific prior written permission.                #
#                                                                              #
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS "AS IS"   #
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    #
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE,  #
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE     #
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR          #
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF         #
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS     #
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN      #
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)      #
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE   #
# POSSIBILITY OF SUCH DAMAGE.                                                  #
################################################################################


################################################################################
# Branding #####################################################################
################################################################################

# Program name and version
!define AppName       "Chromium"
!define Version       "9.0.597.19"
!define WebKitVersion "534.13"

# Copyright
VIAddVersionKey CompanyName    "Aluísio Augusto Silva Gonçalves"
VIAddVersionKey LegalCopyright "© 2010 Aluísio Augusto Silva Gonçalves. All rights reserved."


################################################################################
# Hooks ########################################################################
################################################################################

# Registry
!macro onRegistyUpdate
	${Registry_BackupKey} HKCU "Software\Chromium"
!macroend
!macro onRegistryRestore
	${Registry_SaveKey}   HKCU "Software\Chromium"
!macroend

# $LOCALAPPDATA\Chromium folder
!macro beforeExec
	${File_Backup}  "$LOCALAPPDATA\Chromium" "LocalAppData"
!macroend
!macro afterExec
	${File_Restore} "LocalAppData"
!macroend


################################################################################

# Compression
SetCompressorDictSize 54
SetDateSave Off

!define SupportedFrameworkVersion 5
