# LSP suppport

On Debian, to add eglot support for python you should install pyright.
Installing from npm seems to work fine.
For java you should install jdtls.

To enable the LSP, `M-x eglot`.

# Daemon-Client
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
# emacs client
e() {
  emacsclient -c "$@"
}
emacs() {
  # `command` avoid recursion
  command emacs "$@" &
}
```
At start-up the daemon won't load-file `init.el`.
Start a client with `emacs` and run: `M-x load ~/.emacs.d/init.el`
All clients afterwards should retain the style. To start a client use `e file.txt`. You can still load a standalone emacs instance with `emacs file.txt`. The main advantage of going stand-alone is to inherit the environment variables from the terminal.
