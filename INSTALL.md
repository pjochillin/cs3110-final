# Installation
Use the following commands to install the required libraries for this application on Windows (through WSL: Ubuntu):

```bash
opam install cohttp-lwt-unix cohttp-async plplot http-lwt-client bogue tsdl tsdl-image tsdl-ttf lablgtk3 cairo2
```

On Mac systems, there seems to be issues with installing plplot. If that issue arises, run the following commands in this order (must have Homebrew installed for the second one):

```bash
sudo apt install cmake pkg-config
brew install plplot
opam install cohttp-lwt-unix cohttp-async plplot http-lwt-client bogue tsdl tsdl-image tsdl-ttf lablgtk3 cairo2
```

To get rid of debug messages from libraries regarding permissions (like the one listed below), run this command (note that a password might need to be entered):

```bash
QStandardPaths: wrong permissions on runtime directory /run/user/1000/, 0755 instead of 0700

sudo chmod 0700 /run/user/1000/
```