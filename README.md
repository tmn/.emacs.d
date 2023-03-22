# .emacs.d

**My .emacs.d - use it, break it, fix it, trash it**

This emacs configuration is tested in emacs 27.1 on macOS.


## Usage

Clone this into your home folder as `.emacs.d`. Rember to backup your old `.emacs.d` if you already have one:

```bash
git clone https://github.com/tmn/.emacs.d.git ~/.emacs.d
```


## Install Emacs

Whatever port of emacs would do. I prefer the pure one using cask:

```bash
brew install --cask emacs
```


## Dependencies

This emacs configuration depends on a few packages installed outside of emacs.


### OS Dependencies

Install system dependencies for some of the emacs packages:

* ripgrep
* ctags
* node
* openjdk@11
* coreutils

I.e. using brew:

```bash
brew install ripgrep ctags node openjdk@11 coreutils librsvg pyright
```

> Rember to add Java to `PATH`.


### Other dependencies

The language configurations depends on some outside packages. These are installed outside of emacs.


#### Packages through npm

* typescript-language-server
* typescript
* eslint
* babel
* babel-eslint
* elm-format

```bash
npm install -g eslint babel babel-eslint typescript typescript-language-server elm-format
```


#### Swift

This configuration uses `sourcekit-lsp` for Swift. The `sourcekit-lsp` is bundled with Xcode 11.4 and later. Download latest Xcode from the Mac App Store or from the Apple Developer Portal. Or simply do a `xcode-select --install` in your terminal.


#### Java

The Java setup depends on `lombok.jar`. It may be downloaded form [projectlombok.org/download](https://projectlombok.org/download).

Place the file inside `~/.emacs/lib/`.


Cheers!
