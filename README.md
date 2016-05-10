# .emacs.d

**My .emacs.d - use it, break it, fix it, trash it**

This emacs configuration is tested in emacs 25.1 on macOS. Some of the configurations aren't compatible with Windows, yet.

## Usage

Clone this into your home folder as `.emacs.d`. Rember to backup your old `.emacs.d` if you already have one:

```
git clone https://github.com/tmn/.emacs.d.git ~/.emacs.d
```

## Install Emacs

```
brew tap d12frosted/emacs-plus
brew install emacs-plus          # Install the latest release (currently Emacs 27)
```

## OS dependencies

Install system dependencies for some of the emacs packages.

### macOS

```bash
brew install the_silver_searcher
```

### Linux (apt)

```bash
apt-get install silversearcher-ag
```

## Other dependencies

The language configurations depends on some outside packages. These are installed outside of emacs.

### Swift

Download and install latest snapshot of Swift toolchain from [swift.org](https://swift.org/download/#snapshots).

### Packages through npm

* typescript-language-server
* javascript-typescript-langserver
* typescript
* eslint
* babel
* babel-eslint
* elm-format

```bash
npm install -g eslint babel babel-eslint typescript-language-server javascript-typescript-langserver typescript elm-format
```


Cheers!
