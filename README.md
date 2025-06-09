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
Finally, add this to your .zshrc.
Usage: `emacs test.txt`
```bash
# emacs client
emacs() {
  emacsclient -c -n "$@"
}
```
