# SVG Drawing Demo using Fable, Electron, Elmish, React


## Project Structure

Electron bundles Chromium (View) and node.js (Engine), therefore as in every node.js project, the `package.json` file specifies the (Node) module dependencies.

* dependencies: node libraries that the executable code (and development code) needs
* dev-dependencies: node libraries only needed by development tools

Additionally, the section `"scripts"`:
```
 "scripts": {
    "compile": "dotnet fable src/main && dotnet fable src/renderer",
    "dev": "dotnet fable src/main && dotnet fable watch src/renderer --run electron-webpack dev",
    "watchmain": "dotnet fable watch src/main",
    "webpack": "electron-webpack dev",
    "dist": "npm run compile && electron-builder",
    "distwin": "npm run compile && electron-builder -w",
    "distmac": "npm  compile && electron-builder -m"
  }
```
Defines the in-project shortcut commands, therefore when we use `npm run <stript_key>` is equivalent to calling `<script_value>`. 
For example, in the root of the project, running in the terminal `npm run dev` is equivalent to the command line:

```
dotnet fable src/main && dotnet fable watch src/renderer --run electron-webpack dev
```

This runs fable 3 to compile the main process, then again to compile and watch the renderer process. After the renderer compilation is finished 
`electron-webpack dev` will be run. This invokes `electron-webpack` to pack and launch the code under electron. Fable 3 will watch the renderer F# files
and convert to javascript on the fly. Electron-webpack will watch the changes in javascript files and hot load these into the running application.

The build system depends on a `Fake` file `build.fsx`. This has targets representing build tasks, and normally these are used, 
accessed via `build.cmd` or `build.sh`, instead of using `dotnet fake` directly.

## Code Structure

The source code consists of two distinct sections compiled separately to Javascript to make a complete Electron application.

* The electron main process runs the Electron parent process under the desktop native OS, it starts the app process and provides desktop access services to it.
* The electron client (app) process runs under Chromium in a simulated browser environment (isolated from the native OS).

Electron thus allows code written for a browser (HTML + CSS + JavaScript) to be run as a desktop app with the additional capability of desktop filesystem access via communication between the two processes.

Both processes run Javascript under Node.

The `src/Main/Main.fs` source configures electron start-up and is boilerplate. It is compiled to the root project directory so it can be automatically picked up by Electron.

The remaining app code is arranged in five different sections, each being a separate F# project. This separation allows all the non-web-based code (which can equally be run and tested under .Net) to be run and tested under F# directly in addition to being compiled and run under Electron.

The project relies on the draw2d JavaScript library, which is extended to support digital electronics components. The extensions are in the `app/public/lib/draw2d_extensions` folder and are loaded by the `index.html` file. The `index.html` file is otherwise empty as the UI elements are dynamically generated with [React](https://reactjs.org/), thanks to the F# Elmish library.

The code that turns the F# project source into `renderer.js` is the FABLE compiler followed by the Node Webpack bundler that combines multiple Javascript files into a single `renderer.js`. Note that the FABLE compiler is distributed as a node package so gets set up automatically with other Node components.

The compile process is controlled by the `.fsproj` files (defining the F# source) and `webpack.additions.main.js`, `webpack.additions.renderer.js`
which defines how Webpack combines F# outputs for both electron main and electron app processes and where the executable code is put. 
This is boilerplate which you do not need to change; normally the F# project files are all that needs to be modified.

## File Structure

### `src` folder

|   Subfolder   |                                             Description                                            |
|:------------:|:--------------------------------------------------------------------------------------------------:|
| `main/` | Code for the main electron process that sets everything up - not normally changed |
| `Renderer/`     | The renderer project which implements the window and all user interaction in it. This and `main` are the only projects that cannot run under .Net, as they contain JavaScript related functionalities. |



### `Static` folder

Contains static files used in the application.


## Build Magic

This project uses modern F# / dotnet cross-platform build. The build tools all install cross-platform under dotnet, downloaded automatically by paket. The Fake 
tool (F# make) automates the rest of the build and also downloads Node.


## Getting Started


If you want to get started as a developer, follow these steps:

1. Download and install the latest [dotnet Core SDK](https://www.microsoft.com/net/learn/get-started).  
For Mac and Linux users, download and install [Mono](http://www.mono-project.com/download/stable/) from official website 
(the version from brew is incomplete, may lead to MSB error later). If the build fails due to lack of Node.js, 
download and install [Node.js v12](https://nodejs.org/dist/latest-v12.x/) and npm.

2. Download & unzip the [Issie repo](https://github.com/tomcl/ISSIE), or if contributing clone it locally, or fork it on github and then clone it locally.

3. Navigate to the project root directory (which contains this README) in a command-line interpreter. For Windows usage make sure if possible for convenience 
that you have a _tabbed_ command-line interpreter that can be started direct from file explorer within a specific directory (by right-clicking on the explorer directory view). 
That makes things a lot more pleasant. The new [Windows Terminal](https://github.com/microsoft/terminal) works well.

4. Run `build.cmd` under Windows or `build.sh` under linux or macos. This will download all dependencies and create auto-documentation and binaries, then launch the application with HMR.
  
  * HMR: the application will automatically recompile and update while running if you save updated source files
  * To initialise and reload: `File -> reload page`
  * To exit: after you exit the application the auto-compile script will terminate after about 15s
  * To recompile the application `npm run dev`.
  * To generate distributable binaries for dev host system `npm run dist`.
  * If you have changed node modules use `build dev`. Note that this project uses npm, not yarn. If npm gets stuck use `build cleannode` and try again.
  * From time to time run `build killzombies` to terminate orphan node and dotnet processes which accumulate using this dev chain.


## Reinstalling Compiler and Libraries

To reinstall the build environment (without changing project code) rerun `build.cmd` (Windows) or `build.sh` (Linux and MacOS). You may need first to
run `build killzombies` to remove orphan processes that lock build files.

## Information on SVG React Helpers for Elmish




### Old-style Syntax

you can use this, and even mix it with new-style syntax, but

React virtual DOM elements are created in Elmish using F# helper functions of form:

```
elementName (props:IProp list) (children: ReactElement list)
```

They divide into SVG elements (that can be used as children of an `svg` element that creates an SVG canvas) and DOM elments 
that correspond to HTML DOM elements. There are also some `void` elements:

```
voidElementName (props: IProp list)


That deal with non-visible interaction and do NOT allow react element children.

See [Fable standard react documentation](https://github.com/fable-compiler/fable-react/blob/master/src/Fable.React.Standard.fs) 
for the list of SVG, DOM and Void React element helper functions. That file also indicates

    "core-js": "^3.6.5",
    "cross-zip": "^3.1.0",
    "cross-zip-cli": "^1.0.0",

### New-style Feliz Syntax

The demo in this repo and your porject work will all use new-stle helper functions to create react virtual DOM. 
The difference in syntax is that the new-style functions use a single list:

```
elementName (props: )

