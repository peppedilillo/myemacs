# LSP suppport

On Debian, to add eglot support for python you should install pyright.
Installing from npm seems to work fine.
For java you should install jdtls.

To enable the LSP, `M-x eglot`.


# Daemon-Client

## On Linux

Create the user service:

```text
~/.config/systemd/user/emacs.service
```

with:

```ini
[Unit]
Description=Emacs text editor daemon
Documentation=info:emacs man:emacs(1)

[Service]
Type=notify
ExecStart=/usr/bin/emacs --fg-daemon
SuccessExitStatus=15
Restart=on-failure

[Install]
WantedBy=default.target
```

Load and start it **from a terminal inside the graphical session**:

```shell
systemctl --user daemon-reload
systemctl --user import-environment DISPLAY XAUTHORITY WAYLAND_DISPLAY
systemctl --user enable --now emacs.service

# you can restart with
# systemctl --user import-environment DISPLAY XAUTHORITY WAYLAND_DISPLAY
# systemctl --user restart emacs.service
# restarting in this way you can lose unsaved buffers
```

Check the service and process:

```shell
systemctl --user status emacs.service
systemctl --user list-units --type=service --all | grep emacs
ps aux | grep '[e]macs'
```

Add to `~/.zshrc`:

```shell
# Independent standalone Emacs process
emacs() {
  # `command` avoids calling this function recursively.
  command emacs "$@" &!
}

# Graphical daemon client
e() {
  emacsclient --create-frame --no-wait --timeout=10 "$@"
}

# Terminal daemon client
et() {
  emacsclient --tty --timeout=10 "$@"
}
```

---------

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

  <key>KeepAlive</key>
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

`KeepAlive` restarts the daemon after crashes and normal exits, including
Cmd+Q. Restarting does not restore unsaved changes. launchd throttles repeated
starts to avoid a rapid restart loop.

After changing an already loaded plist, save your work and reload the service
(this closes existing Emacs frames):

```shell
launchctl bootout gui/$(id -u)/org.gnu.emacs.daemon
launchctl bootstrap gui/$(id -u) \
  ~/Library/LaunchAgents/org.gnu.emacs.daemon.plist
```

To deliberately stop the daemon, use the `bootout` command above; use
`bootstrap` to enable it again. Close individual client frames with `C-x C-c`.

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
