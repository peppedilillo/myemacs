# LSP suppport

On Debian, to add eglot support for python you should install pyright.
Installing from npm seems to work fine.
For java you should install jdtls.

To enable the LSP, `M-x eglot`.

# Daemon-Client

## Solution 1

Run `systemctl --user enable --now emacs`. Then, create the next service file:
```
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
```bash
# emacs gui client
e() {
  emacsclient -c "$@"
}
# emacs terminal client
et() {
    # TERM definition is needed since many systems do not
    # recognize ghostty own terminfo yet. the command is
    # not needed with gnome-terminal and other terminal
    # emulator. remove in the future.
  TERM=xterm-256color emacsclient "$@"
}
# emacs standalone
emacs() {
  # `command` avoid recursion
  command emacs "$@" &
}
```
At start-up the daemon won't load-file `init.el`. To solve this, start a client and run: `M-x load ~/.emacs.d/init.el`. All clients afterwards should retain the style.
To start a client use `e file.txt`. You can still load a standalone emacs instance with `emacs file.txt`. The main advantage of going stand-alone is to inherit the environment variables from the terminal.

I tested this method on IGOR PC and it works there.

## Solution 2

A simpler solution requires just creating an alias like `alias ec='emacsclient -c -a ""'`. The command should automatically start the daemon when first called. In essence this method requires copypasting to .zshrc the following:
```bash
# emacs gui client
e() {
  emacsclient -c -a "" "$@" &
}
# emacs terminal client
et() {
    # TERM definition is needed since many systems do not
    # recognize ghostty own terminfo yet. the command is
    # not needed with gnome-terminal and other terminal
    # emulator. remove in the future.
  TERM=xterm-256color emacsclient "$@"
}
# emacs standalone
emacs() {
  # `command` avoid recursion
  command emacs "$@" &
}
```
