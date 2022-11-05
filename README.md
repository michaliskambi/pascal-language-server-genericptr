
# Pascal Language Server

An [LSP](https://microsoft.github.io/language-server-protocol/) server
implementation for Pascal variants that are supported by [Free
Pascal](https://www.freepascal.org/), including Object Pascal. It uses
[CodeTools](https://wiki.lazarus.freepascal.org/Codetools) from
Lazarus as backend.

https://github.com/michaliskambi/pascal-language-server-genericptr notes:

This a fork of a fork. We add capability to configure using `castle-pasls.ini`, in particular to define _Castle Game Engine_ path that will make `pasls` aware of CGE units and autocomplete CGE API.

## Features

The implementation is still incomplete.

### Implemented Methods

 - textDocument
   - declaration
   - implementation
   - references
   -  signatureHelp
   - documentSymbol (only `SymbolInformation`)
   - documentHighlight
   - completion
   - inlayHint
 - window
   - showMessage
 - workspace
   - symbol
   - executeCommand
- diagnostics (incomplete)

### Initialization Options

Editors can supply [initialization options](https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#initialize) to the server, however each client handles this differently so please refer to your editors LSP plugin for more information.

The follow options are supported:

```json
"initializationOptions":
{
  "fpcOptions":
  [
    "-Fu/path/to",
    "-Fi/path/to",
    "-dMY_MACRO"
  ],
  "options":
  {
    "name": "string|number|boolean"
  },
  "symbolDatabase": "/path/to/symbols.db",
  "maximumCompletions": "number",
  "overloadPolicy": [1,  // duplicate function names appear in the list
                     2,  // after the original definition ignore others
                     3   // add a suffix which denotes the overload count
                     ],
  "program": "pasls.lpr"
}
```

 - `fpcOptions`: compiler flags used to specifiy paths, macros etc...
 - `symbolDatabase`:  if a valid path is provided the server will use an SQL database to store symbol information (recommended for faster documentSymbol queries).
 - `maximumCompletions`: the maximum number of completions returned per query. If the maximum is exceeded then `CompletionList.isIncomplete` will be set to true and results will be recomputed as the user types.
 - `overloadPolicy`: The preferred method to handle overloaded functions in document symbol requests.

Macros are supported in initialization options. The following macros will be expanded:

- `$(tmpdir)` - Path to your systems temporary directory.
- `$(root)` - Path to the rootURI as specified by the clients [initialize request](https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#initialize).

The following macro formats are valid:

- `$macro`
- `$MACRO`
- `$(macro)`
- `$(MACRO)`

### Optional Settings

Boolean values used in *initializationOptions.options*.

```json
// procedure completions with parameters are inserted as snippets
insertCompletionsAsSnippets
// procedure completions with parameters (non-snippet) insert
// empty brackets (and insert as snippet)
insertCompletionProcedureBrackets
// workspaces folders will be added to unit paths (i.e. -Fu)
includeWorkspaceFoldersAsUnitPaths
// workspaces folders will be added to include paths (i.e. -Fi)
includeWorkspaceFoldersAsIncludePaths
// syntax will be checked when file opens or saves
checkSyntax
// syntax errors will be published as diagnostics
publishDiagnostics
// enable workspace symbols
workspaceSymbols
// enable document symbols
documentSymbols
// completions contain a minimal amount of extra information
minimalisticCompletions
// syntax errors as shown in the UI with ‘window/showMessage’
showSyntaxErrors
```

### Ini file

Note: The initializationOptions described above are in general a better way to provide options for LSP. However... the `castle-pasls.ini` is

1. central configuration for both LSP server forks (this one, and based on Philip Zander, https://github.com/castle-engine/pascal-language-server ),

2. central configuration for both VS Code and Emacs, which makes it easier for Michalis who uses both.

Allowed options:

```
[castle]
;; Castle Game Engine location.
;; Set this to make pasls automatically know paths to CGE units,
;; and thus autocomplete CGE API.
;; Alternatively you can define CASTLE_ENGINE_PATH environment variable.
path=/home/michalis/sources/castle-engine/castle-engine/
```

### TODO:

 - Optional properties are not implemented so the JSON payloads are bloated.
 - `documentHighlight` should select the begin/end keywords only.
 - `textDocument/codeAction`
- `DocumentSymbol`class for document symbols

## Clients

### Emacs

To use the server from `lsp-mode` in Emacs, install the separate
[`lsp-pascal`](https://github.com/arjanadriaanse/lsp-pascal) module.

### Sublime Text

Example settings JSON for the [LSP](https://github.com/sublimelsp/LSP) package on macOS.

```json
"pascal-language-server":
{
  "command":
  [
    "/pascal-language-server/lib/x86_64-darwin/pasls"
  ],
  "env":
  {
    "FPCDIR": "/usr/local/share/fpcsrc",
    "FPCTARGET": "darwin",
    "FPCTARGETCPU": "x86_64",
    "LAZARUSDIR": "/usr/share/lazarus",
    "PP": "/usr/local/lib/fpc/3.0.4/ppcx64"
  },
  "initializationOptions": {
    // global options which apply to all projects
  },
  "languageId": "pascal",
  "scopes":
  [
    "source.pascal"
  ],
  "syntaxes":
  [
    "Packages/FPC/FPC.sublime-syntax"
  ]
}
```

### Visual Studio Code

Install the [extension](
https://github.com/genericptr/pasls-vscode) and configure the settings accordingly. You must have the actual language installed before the extension will work.


## Building

Requires Free Pascal Compiler version 3.2.0 and Lazarus version 2.0.8,
open the project file in Lazarus or use the commandline:

```sh
lazbuild pasls.lpi
```
