// Pascal Language Server
// Copyright 2020 Arjan Adriaanse

// This file is part of Pascal Language Server.

// Pascal Language Server is free software: you can redistribute it
// and/or modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.

// Pascal Language Server is distributed in the hope that it will be
// useful, but WITHOUT ANY WARRANTY; without even the implied warranty
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Pascal Language Server.  If not, see
// <https://www.gnu.org/licenses/>.

unit general;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  CodeToolManager, CodeToolsConfig, URIParser, LazUTF8,
  lsp, basic, capabilities, documentSymbol, settings;

type

  { TWorkspaceFolder }

  TWorkspaceFolder = class(TCollectionItem)
  private
    fUri: TDocumentUri;
    fName: String;
  published
    // The associated URI for this workspace folder.
    property uri: TDocumentUri read fUri write fUri;
    // The name of the workspace folder. Used to refer to this
    // workspace folder in the user interface.
    property name: string read fName write fName;
  end;
  TWorkspaceFolderItems = specialize TGenericCollection<TWorkspaceFolder>;

  { TVoidParams }

  TVoidParams = class(TPersistent);

  { TInitializationOptions }

  TInitializationOptions = class(TServerSettings)
  end;

  { TInitializeParams }

  TInitializeParams = class(TPersistent)
  private
    fProcessId: integer;
    fClientInfo: TClientInfo;
    fRootUri: string;
    fCapabilities: TClientCapabilities;
    fTrace: string;
    fInitializationOptions: TInitializationOptions;
    fWorkspaceFolders: TWorkspaceFolderItems;
  published
    // The process Id of the parent process that started
    // the server. Is null if the process has not been started by another process.
    // If the parent process is not alive then the server should exit (see exit notification) its process.
    property processId: integer read fProcessId write fProcessId;
    // Information about the client
    property clientInfo: TClientInfo read fClientInfo write fClientInfo;
    // The rootUri of the workspace. Is null if no
    // folder is open. If both `rootPath` and `rootUri` are set
    // `rootUri` wins.
    property rootUri: string read fRootUri write fRootUri;
    // User provided initialization options.
    property initializationOptions: TInitializationOptions read fInitializationOptions write fInitializationOptions;
    // The capabilities provided by the client (editor or tool)
    property capabilities: TClientCapabilities read fCapabilities write fCapabilities;
    // The initial trace setting. If omitted trace is disabled ('off').
    property trace: string read fTrace write fTrace;
    // The workspace folders configured in the client when the server starts.
    // This property is only available if the client supports workspace folders.
    // It can be `null` if the client supports workspace folders but none are
    // configured.
    property workspaceFolders: TWorkspaceFolderItems read fWorkspaceFolders write fWorkspaceFolders;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

  { TInitializeResult }

  TInitializeResult = class(TPersistent)
  private
    fCapabilities: TServerCapabilities;
  published
    property capabilities: TServerCapabilities read fCapabilities write fCapabilities;
  end;

  { TInitialize }

  TInitialize = class(specialize TLSPRequest<TInitializeParams, TInitializeResult>)
    function Process(var Params : TInitializeParams): TInitializeResult; override;
  end;

  { TInitialized }

  TInitialized = class(specialize TLSPNotification<TVoidParams>)
    procedure Process(var Params : TVoidParams); override;
  end;

  { TCancelParams }

  TCancelParams = class(TPersistent)
  private
    fId: Integer;
  published
    property id: Integer read fId write fId;
  end;

  { TShutdown }

  TShutdown = class(specialize TLSPRequest<TVoidParams, TPersistent>)
    function Process(var Params : TVoidParams): TPersistent; override;
  end;

  { TExit }

  TExit = class(specialize TLSPNotification<TVoidParams>)
    procedure Process(var Params : TVoidParams); override;
  end;

  { TCancel }

  TCancel = class(specialize TLSPNotification<TCancelParams>)
    procedure Process(var Params : TCancelParams); override;
  end;

implementation
uses
  SysUtils, RegExpr, DefineTemplates;

{ TInitializeParams }

procedure TInitializeParams.AfterConstruction;
begin
  inherited;

  if clientInfo = nil then
    clientInfo := TClientInfo.Create;
    
  if initializationOptions = nil then
    initializationOptions := TInitializationOptions.Create;

  workspaceFolders := TWorkspaceFolderItems.Create;
end;

destructor TInitializeParams.Destroy; 
begin
  // note: it's a hack for now but we don't want to free initializationOptions and clientInfo
  //initializationOptions.Free;
  //clientInfo.Free;
  
  workspaceFolders.Free;

  inherited;
end;

{ TInitialize }

function TInitialize.Process(var Params : TInitializeParams): TInitializeResult;
const
  kStatusPrefix = '✓ ';
  kFailedPrefix = '⚠️ ';

  procedure ShowConfigStatus(CodeToolsOptions: TCodeToolsOptions); 
  begin
    writeln(StdErr, kStatusPrefix+'Client: ', Params.clientInfo.name, ' ', Params.clientInfo.version);

    writeln(StdErr, kStatusPrefix+'FPCPath: ', CodeToolsOptions.FPCPath);
    writeln(StdErr, kStatusPrefix+'FPCSrcDir: ', CodeToolsOptions.FPCSrcDir);
    writeln(StdErr, kStatusPrefix+'TargetOS: ', CodeToolsOptions.TargetOS);
    writeln(StdErr, kStatusPrefix+'TargetProcessor: ', CodeToolsOptions.TargetProcessor);

    writeln(StdErr, kStatusPrefix+'Working directory: ', GetCurrentDir);

    if CodeToolsOptions.FPCOptions <> '' then
      writeln(stderr, kStatusPrefix+'FPCOptions: ', CodeToolsOptions.FPCOptions)
    else
      writeln(stderr, kStatusPrefix+'FPCOptions: [unspecified]');

    if ServerSettings.&program <> '' then
      writeln(StdErr, kStatusPrefix+'Main program file: ', ServerSettings.&program);

    if CodeToolsOptions.ProjectDir <> '' then
      writeln(stderr, kStatusPrefix+'ProjectDir: ', CodeToolsOptions.ProjectDir)
    else
      writeln(stderr, kStatusPrefix+'ProjectDir: [unspecified]');
    
    if ServerSettings.symbolDatabase <> '' then
      writeln(stderr, kStatusPrefix+'Symbol Database: ', ServerSettings.symbolDatabase)
    else
      writeln(stderr, kStatusPrefix+'Symbol Database: [unspecified]');

    writeln(stderr, kStatusPrefix+'Maximum Completions: ', ServerSettings.maximumCompletions);
    writeln(stderr, kStatusPrefix+'Overload Policy: ', ServerSettings.overloadPolicy);
    Flush(stderr);
  end;

var
  CodeToolsOptions: TCodeToolsOptions;
  Option, Path: String;
  URI: TURI;
  Item: TCollectionItem;
  re: TRegExpr;
  DirectoryTemplate,
  UnitPathTemplate: TDefineTemplate;
  ServerCapabilities: TServerCapabilities;
  Macros: TMacroMap;
begin with Params do
  begin
    CodeToolsOptions := TCodeToolsOptions.Create;

    // TODO: we need to copy this or implement ref counting
    // once we figure out how memory is going to work with
    // the JSON-RPC streaming model
    ServerSettings := initializationOptions;
    settings.ClientInfo := clientInfo;

    // replace macros in server settings
    Macros := TMacroMap.Create;
    Macros.Add('tmpdir', GetTempDir(true));
    Macros.Add('root', ParseURI(rootUri).path);

    ServerSettings.ReplaceMacros(Macros);

    // load the symbol manager if it's enabled
    if ServerSettings.options.documentSymbols or ServerSettings.options.workspaceSymbols then
      SymbolManager := TSymbolManager.Create;

    ServerCapabilities := TServerCapabilities.Create(initializationOptions);

    re := TRegExpr.Create('^(-(Fu|Fi)+)(.*)$');
    with CodeToolsOptions do
      begin
        // set some built-in defaults based on platform
        {$ifdef DARWIN}
        FPCPath := '/usr/local/bin/fpc';
        FPCSrcDir := '/usr/local/share/fpcsrc';
        {$endif}

        {$ifdef MSWINDOWS}
        {$endif}

        {$ifdef LINUX}
        {$endif}

        InitWithEnvironmentVariables;

        // attempt to load optional config file
        Path := ExpandFileName(initializationOptions.CodeToolsConfig);
        if FileExists(Path) then
          begin
            writeln(StdErr, 'Loading config file: ', Path);
            LoadFromFile(Path);
          end;

        // include workspace paths as search paths
        if ServerSettings.options.includeWorkspaceFoldersAsUnitPaths or
          ServerSettings.options.includeWorkspaceFoldersAsIncludePaths then
          for Item in workspaceFolders do
            begin
              URI := ParseURI(TWorkspaceFolder(Item).uri);
              Path := URI.Path + URI.Document;
              
              if ServerSettings.options.includeWorkspaceFoldersAsUnitPaths then
                begin
                  initializationOptions.FPCOptions.Add('-Fu'+Path);
                  initializationOptions.FPCOptions.Add('-Fi'+Path);
                end;

              // if the server supports workspace symbols then
              // cane the workspace folder for symbols
              if ServerCapabilities.workspaceSymbolProvider then
                SymbolManager.Scan(Path, false);
            end;

        for Option in initializationOptions.FPCOptions do
          begin
            // expand file names in switches with paths
            if re.Exec(Option) then
              FPCOptions := FPCOptions + '-' + re.Match[2] + ExpandFileName(re.Match[3]) + ' '
            else
              FPCOptions := FPCOptions + Option + ' ';
          end;

        if ServerSettings.&program <> '' then
          begin
            Path := ExpandFileName(ServerSettings.&program);
            if FileExists(Path) then
              ServerSettings.&program := Path
            else
              begin
                writeln(StdErr, kFailedPrefix+'Main program file ', Path, ' can''t be found.');
                ServerSettings.&program := '';
              end;
          end;

        if rootUri <> '' then
          ProjectDir := ParseURI(rootUri).Path;

        ShowConfigStatus(CodeToolsOptions);
      end;
    re.Free;
    
    with CodeToolBoss do
      begin
        Init(CodeToolsOptions);
        IdentifierList.SortForHistory := True;
        IdentifierList.SortForScope := True;
      end;

    Result := TInitializeResult.Create;
    Result.capabilities := ServerCapabilities;
  end;
end;

{ TInitialized }

procedure TInitialized.Process(var Params : TVoidParams);
begin
  // do nothing
end;

{ TShutdown }

function TShutdown.Process(var Params : TVoidParams): TPersistent;
begin
  // do nothing
  result := nil;
end;

{ TExit }

procedure TExit.Process(var Params : TVoidParams);
begin
  Halt(0);
end;

{ TCancel }

procedure TCancel.Process(var Params : TCancelParams);
begin
  // not supported
end;

initialization
  LSPHandlerManager.RegisterHandler('initialize', TInitialize);
  LSPHandlerManager.RegisterHandler('initialized', TInitialized);
  LSPHandlerManager.RegisterHandler('shutdown', TShutdown);
  LSPHandlerManager.RegisterHandler('exit', TExit);
  LSPHandlerManager.RegisterHandler('$/cancelRequest', TCancel);
end.

