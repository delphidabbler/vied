{ ##
  @FILE                     UHelp.pas
  @COMMENTS                 Contains constant declarations for help context
                            numbers in help project file.
  @PROJECT_NAME             Version Information Editor.
  @PROJECT_DESC             Program that allows creation of resource source
                            files containining version information resources.
  @DEPENDENCIES             None.
  @OTHER_NAMES              Renamed from VInfoExp.inc to UHelp.pas as of v3.0.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 25/05/1998
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 05/05/2002
      @COMMENTS             Complete revision. Constants are now declared in a
                            map file and simply included here.
    )
    @REVISION(
      @VERSION              3.0
      @DATE                 17/03/2003
      @COMMENTS             + Converted from include file to unit that provides
                              help topic ids.
                            + Renamed from VInfoExp.inc to UHelp.pas
                            + Now includes the renamed VIEd.map (instead of
                              VInfoExp.map) for topic number maps.
    )
  )
}


{
 * ***** BEGIN LICENSE BLOCK *****
 * 
 * Version: MPL 1.1
 * 
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 * 
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 * 
 * The Original Code is UHelp.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 1998-2005 Peter
 * Johnson. All Rights Reserved.
 * 
 * Contributor(s):
 * 
 * ***** END LICENSE BLOCK *****
}


unit UHelp;


interface


const
  // We get the constants we export from map file (also used by help compiler)
  {$Include Help\VIEd.map}


implementation


end.