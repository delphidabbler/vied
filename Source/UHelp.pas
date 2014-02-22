{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2014, Peter Johnson (www.delphidabbler.com).
 *
 * Contains constant declarations for help context numbers in the help project
 * file.
}


unit UHelp;


interface


uses
  // Delphi
  Windows;


type
  ///  <summary>
  ///  Record that provides methods used to manage the HTML help system.
  ///  </summary>
  THelp = record
  strict private
    ///  <summary>Returns fully qualified name of help file.</summary>
    class function HelpFileName: string; static;
    ///  <summary>Returns fully specified topic URL from a topic name.</summary>
    ///  <remarks>Name of topic is same as topic HTML file, without the
    ///  extension.</remarks>
    class function TopicURL(const TopicName: string): string; static;
    ///  <summary>Calss the HtmlHelp API with a specified command and
    ///  parameters.</summary>
    ///  <param name="Command">LongWord [in] Command to send to HTML Help.
    ///  </param>
    ///  <param name="TopicName">string [in] Names an HTML topic file within the
    ///  help file, without extension. May be '' if no specific topic is
    ///  required.</param>
    ///  <param name="Data">LongWord [in] Command dependent data to pass to HTML
    ///  Help.</param>
    class procedure DoAppHelp(const Command: LongWord;
      const TopicName: string; const Data: LongWord); static;
  public
    const
      ///  <summary>Topic displayed when a dialog box has no associated a-link
      ///  keyword or a-link keyword matches no dialog box.</summary>
      DlgErrTopic = 'dlg-nohelp';
  public
    ///  <summary>Displays help contents.</summary>
    class procedure Contents; static;
    ///  <summary>Displays a given help topic.</summary>
    ///  <param name="Topic">string [in] Help topic to display. This must be the
    ///  name of the topic HTML file, without the extension.</param>
    class procedure ShowTopic(Topic: string); static;
    ///  <summary>Closes down the help system.</summary>
    class procedure Quit; static;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UHTMLHelp;


{ THelp }

class procedure THelp.Contents;
begin
  DoAppHelp(HH_DISPLAY_TOC, '', 0);
end;

class procedure THelp.DoAppHelp(const Command: LongWord;
  const TopicName: string; const Data: LongWord);
var
  HelpURL: string; // URL of help file, or topic with help file
begin
  if TopicName = '' then
    HelpURL := HelpFileName
  else
    HelpURL := TopicURL(TopicName);
  HtmlHelp(GetDesktopWindow(), PChar(HelpURL), Command, Data);
end;

class function THelp.HelpFileName: string;
begin
  Result := ChangeFileExt(ParamStr(0), '.chm');
end;

class procedure THelp.Quit;
begin
  HtmlHelp(0, nil, HH_CLOSE_ALL, 0);
end;

class procedure THelp.ShowTopic(Topic: string);
begin
  DoAppHelp(HH_DISPLAY_TOPIC, Topic, 0);
end;

class function THelp.TopicURL(const TopicName: string): string;
begin
  Result := HelpFileName + '::/HTML/' + TopicName + '.htm';
end;

end.

