# .emacs.d

**My .emacs.d - use it, break it, fix it, trash it**

This emacs configuration is currentlig being used with emacs 29.0, and tested in 28.2.


## Usage

Clone this into your home folder as `.emacs.d`. Rember to backup your old `.emacs.d` if you already have one:

```bash
git clone https://github.com/tmn/.emacs.d.git ~/.emacs.d
```


## Get Emacs

### Install from som repository

There are many ways of installing emacs for your favorite operating system. I'm currently rocking macs with the Apple Silicone so my prefered way is to build emacs from source. But whatever port of emacs would do. A pure one using cask:

```bash
brew install --cask emacs
```

### Building from source

#### Dependencies

Dependencies are installed through [brew](https://brew.sh/).

* autoconf
* coreutils
* gcc
* gnu-sed
* gnutls
* gpg
* gpg-config
* imagemagick
* jansson
* libgccjit
* librsvg
* libxml2
* ncurses
* ripgrep
* texinfo
* tree-sitter


```sh
brew install  autoconf coreutils gcc gnu-sed gnutls gpg gpg-config imagemagick jansson libgccjit librsvg libxml2 ncurses ripgrep texinfo tree-sitter
```

#### Building

GNU Emacs source code and development is hosted on [savannah.gnu.org](https://savannah.gnu.org/projects/emacs/).

My `build.sh`:

```bash
#!/bin/bash

readonly GCC_DIR="$(realpath $(brew --prefix)/opt/libgccjit)"
[[ -d $GCC_DIR ]] ||  { echo "${GCC_DIR} not found"; exit 1; }

readonly SED_DIR="$(realpath $(brew --prefix)/opt/gnu-sed)"
[[ -d $SED_DIR ]] ||  { echo "${SED_DIR} not found"; exit 1; }

readonly GCC_INCLUDE_DIR=${GCC_DIR}/include
[[ -d $GCC_INCLUDE_DIR ]] ||  { echo "${GCC_INCLUDE_DIR} not found"; exit
1; }

readonly GCC_LIB_DIR=${GCC_DIR}/lib/gcc/13
[[ -d $GCC_LIB_DIR ]] ||  { echo "${GCC_LIB_DIR} not found"; exit 1; }

export PATH="${SED_DIR}/libexec/gnubin:${PATH}"
export CFLAGS="-I${GCC_INCLUDE_DIR}"
export LDFLAGS="-L${GCC_LIB_DIR} -I${GCC_INCLUDE_DIR}"
export DYLD_FALLBACK_LIBRARY_PATH="${GCC_LIB_DIR}"
export LIBRARY_PATH="${GCC_LIB_DIR}"

echo "----------- Environment -----------"
echo PATH: $PATH
echo CFLAGS: $CFLAGS
echo LDFLAGS: $LDFLAGS
echo DYLD_FALLBACK_LIBRARY_PATH: $DYLD_FALLBACK_LIBRARY_PATH
echo LIBRARY_PATH: $LIBRARY_PATH
echo "----------- /Environment -----------"

./autogen.sh
./configure \
    --with-native-compilation \
    --with-gnutls \
    --with-xml2 \
    --with-modules \
    --with-imagemagick \
    --with-json \
    --with-tree-sitter
```

Run these commands:

```sh
./build.sh
make -j $(nproc)
make install
```

If you're building it on a mac you'll find the `Emacs.app` in the `nextstep` directory.


## Dependencies

This emacs configuration depends on a few packages installed outside of emacs.


### OS Dependencies

Install system dependencies for some of the emacs packages:

* node
* openjdk@11
* semgrep
* pyright

I.e. using brew:

```sh
brew install node openjdk@11 pyright semgrep
```

> Rember to add Java to `PATH`.


### Other dependencies

The language configurations depends on some outside packages. These are installed outside of emacs.

#### Packages through npm

* typescript-language-server
* typescript-eslint-language-service
* typescript
* eslint
* babel
* babel-eslint


```bash
npm install -g eslint babel babel-eslint typescript typescript-language-server typescript-eslint-language-service
```


#### Swift

This configuration uses `sourcekit-lsp` for Swift. The `sourcekit-lsp` is bundled with Xcode 11.4 and later. Download latest Xcode from the Mac App Store or from the Apple Developer Portal. Or simply do a `xcode-select --install` in your terminal.


#### Rust

Install components for the rust lsp to work propperly:

```sh
rustup component add rust-src
rustup component add rust-analyzer
```


#### Java

The Java setup depends on `lombok.jar`. It may be downloaded form [projectlombok.org/download](https://projectlombok.org/download).

Place the file inside `~/.emacs/lib/`.


Cheers!
