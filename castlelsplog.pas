{
  Copyright 2022-2022 Michalis Kamburelis

  Extensions to the Pascal Language Server specific to Castle Game Engine.

  Distributed on permissive "modified BSD 3-clause",
  https://github.com/castle-engine/castle-engine/blob/master/doc/licenses/COPYING.BSD-3-clause.txt ,
  so that it can be combined with other licenses without issues.
}
{ Logging facility used by CGE / Michalis fork.
  Not in CastleLog, because this is not shared with other LSP server fork. }
unit CastleLspLog;

interface

function IsLogging: Boolean;
procedure InitializeLog;
procedure WritelnLog(S: AnsiString);

implementation

uses SysUtils, Classes,
  CastleLsp;

var
  Log: TFileStream;

function IsLogging: Boolean;
begin
  Result := Log <> nil;
end;

procedure InitializeLog;
var
  LogPath: String;
begin
  LogPath := UserConfig.ReadString('log', 'filename', '');
  if LogPath <> '' then
  begin
    LogPath := LogPath + '.pid' + IntToStr(GetProcessID);
    Log := TFileStream.Create(LogPath, fmCreate);
  end;
end;

procedure WritelnLog(S: AnsiString);
begin
  S := S + LineEnding;
  if Log <> nil then
  begin
    Assert(S <> ''); // at least, S contains newline
    Log.WriteBuffer(S[1], Length(S));
    FileFlush(Log.Handle);
  end;
end;

finalization
  FreeAndNil(Log);
end.
