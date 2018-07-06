my dotfiles
===========

Managing dotfiles using git bare repositories.

_For details, see
[ycombinator](https://news.ycombinator.com/item?id=11070797)._


**Setup**

```bash
git init --bare $HOME/.dotfiles
alias dotfiles='git --git-dir=$HOME/.dotfiles --work-tree=$HOME'
dotfiles config --local status.showUntrackedFiles no
```


**Usage**

```bash
# dotfiles <git-command>
dotfiles add <file>
dotfiles commit -m <message>
dotfiles push <repo>
```


**Install**

```bash
git clone --bare <repo> $HOME/.dotfiles
alias dotfiles='git --git-dir=$HOME/.dotfiles --work-tree=$HOME'
dotfiles config --local status.showUntrackedFiles no
# here be dragons; checkout fails if local files already exist;
# backup
# cd $HOME
# mkdir /tmp/.dotfiles-backup
# dotfiles ls-files | xargs -I {} cp --parents {} /tmp/.dotfiles-backup
dotfiles checkout -f
```

