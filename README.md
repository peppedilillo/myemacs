# LSP suppport

On Debian, to add eglot support for python you should install pyright.
Installing from npm seems to work fine.
For java you should install jdtls.

To enable the LSP, `M-x eglot`.

# Daemon-Client

## On Linux

Run `systemctl --user enable --now emacs`. Then, create the next service file:
```txt
[Unit]
Description=Emacs text editor
Documentation=info:emacs man:emacs(1) https://gnu.org/software/emacs/

[Service]
Type=forking
ExecStart=/usr/bin/emacs --daemon
ExecStop=/usr/bin/emacsclient --eval "(kill-emacs)"
#Environment=SSH_AUTH_SOCK=%t/keyring/ssh
Restart=on-failure

[Install]
WantedBy=default.target
```
And add this function to your .zshrc:
```shell
# emacs standalone
emacs() {
  # `command` avoid recursion
  command emacs "@" 2>/dev/null &
}

# emacs gui client
e() {
  emacsclient -c -n "$@"
}

# emacs terminal client
et() {
  TERM=xterm-256color emacsclient -t "$@"
}
```
At start-up the daemon won't load-file `init.el`. To solve this, start a client and run: `M-x load ~/.emacs.d/init.el`. All clients afterwards should retain the style.
To start a client use `e file.txt`. You can still load a standalone emacs instance with `emacs file.txt`. The main advantage of going stand-alone is to inherit the environment variables from the terminal.

I tested this method on IGOR PC and it works there.


## On MacOS:

Create `~/Library/LaunchAgents/org.gnu.emacs.daemon.plist` with content:
```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key>
  <string>org.gnu.emacs.daemon</string>

  <key>ProgramArguments</key>
  <array>
    <string>/Applications/Emacs.app/Contents/MacOS/Emacs</string>
    <string>--fg-daemon</string>
  </array>

  <key>RunAtLoad</key>
  <true/>

  <key>LimitLoadToSessionType</key>
  <string>Aqua</string>

  <key>StandardOutPath</key>
  <string>/tmp/emacs-daemon.log</string>

  <key>StandardErrorPath</key>
  <string>/tmp/emacs-daemon-error.log</string>
</dict>
</plist>
```
Start the daemon and enable it:
```shell
launchctl bootstrap gui/$(id -u) \
  ~/Library/LaunchAgents/org.gnu.emacs.daemon.plist
```
Add to `~/.zshrc`:
```shell
# fix paths as needed, these are the brew deafult
emacs() {
    /Applications/Emacs.app/Contents/MacOS/Emacs "$@" 2>/dev/null &
}

# emacs gui client
EMACSCLIENT=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient

e() {
  "$EMACSCLIENT" -c -n "$@"
}

et() {
  TERM=xterm-256color "$EMACSCLIENT" -t "$@"
}
```
