# Installation
Use the following commands to install the required libraries for this application on Windows (through WSL: Ubuntu):

```bash
opam install cohttp-lwt-unix cohttp-async plplot http-lwt-client bogue tsdl tsdl-image tsdl-ttf
```

On Mac systems, there seems to be issues with installing plplot. If that issue arises, run the following commands in this order (must have Homebrew installed for the first one):

```bash
brew install plplot
opam install cohttp-lwt-unix cohttp-async plplot http-lwt-client bogue tsdl tsdl-image tsdl-ttf
```

Disclaimer: Installing this project on a Mac does not always work. If possible, install it on Windows.