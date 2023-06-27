# Compile and install Python from source code

1. Select an install location.

       VERSION='3.11.4'
       PREFIX="${HOME}/.python/${VERSION}"
       # mkdir -p "$PREFIX"

2. Download, verify and unpack the source code.

       URL="https://www.python.org/ftp/python/${VERSION}"
       TAR="Python-${VERSION}.tar.xz"
       tmp_dir="$(mktemp -d)"
       cd "$tmp_dir"
       wget -q "${URL}/${TAR}" "${URL}/${TAR}.asc"
       gpg --dearmor < ~/.dotfiles/python/python.asc > python.gpg
       gpg --no-default-keyring --keyring python.gpg --homedir ./ --verify "${TAR}.asc"
       xz --decompress < "$TAR" | tar -xf -

3. Install required libraries and headers.

       sudo apt-get install tk-dev libgdbm-dev libdb-dev libffi-dev

4. Compile, test and install.

       cd "${TAR%.tar.xz}"
       PREFIX="${HOME}/.python/${VERSION}"
       SQLITE_PREFIX="${HOME}/.sqlite/3.42.0"
       LD_RUN_PATH="${SQLITE_PREFIX}/lib" \
       LDFLAGS="-L${SQLITE_PREFIX}/lib" \
       CPPFLAGS="-I${SQLITE_PREFIX}/include" \
       ./configure --prefix="$PREFIX" --enable-optimizations --with-lto \
           --enable-loadable-sqlite-extensions --with-ensurepip=upgrade
       LD_RUN_PATH="${SQLITE_PREFIX}/lib" \
       make -j
       make testall  # test_readline does not terminate, use Ctrl-C.
       make altinstall  # altinstall omits the generic "python3" symlink.

## Compiling sqlite

    wget -q -P /tmp 'https://www.sqlite.org/2023/sqlite-autoconf-3420000.tar.gz'
    sha3_256 = "643898e9fcc8f6069bcd47b0e6057221c1ed17bbee57da20d2752c79d91274e8"
    if [["$(openssl dgst -sha3-256 < /tmp/sqlite-autoconf-3420000.tar.gz)" != "$sha3_256"]]; then
        echo "Checksum failed! Aborting..." 1>&2
        exit 1
    fi
    tar -xzf sqlite-autoconf-3420000.tar.gz
    cd sqlite-autoconf-3420000
    sudo apt-get update && sudo apt-get install --yes libreadline-dev
    CFLAGS="-DSQLITE_THREADSAFE=1 -DSQLITE_DEFAULT_WAL_SYNCHRONOUS=1 -DSQLITE_DQS=0 -DSQLITE_LIKE_DOESNT_MATCH_BLOBS -DSQLITE_USE_ALLOCA" ./configure --prefix="$HOME/.sqlite/3.42.0" --enable-readline --enable-threadsafe --enable-dynamic-extensions
    make -j
    make check
    make install
    make clean
    make distclean

Compile-time options:

SQLITE_THREADSAFE=1  # Default, safe for multiple threads to share a connection.
SQLITE_DEFAULT_WAL_SYNCHRONOUS=1  # Use synchronous=1/NORMAL in WAL mode.
SQLITE_DQS=0  # Disable double-quoted string literals.
SQLITE_LIKE_DOESNT_MATCH_BLOBS=1  # Faster LIKE for text operands.
SQLITE_USE_ALLOCA=1  # Use alloca, assuming the system supports it.

The following can be used after Python fixes its build scripts:

SQLITE_OMIT_SHARED_CACHE=1  # Shared cache is discouraged, Python has deprecated it, but the build scripts require it.

The following, being the default, are not needed:

HAVE_READLINE  # configure --enable-readline sets this and -lreadline -lncurses
SQLITE_ENABLE_FTS4  # Default.
SQLITE_ENABLE_FTS5  # Default.
HAVE_ZLIB  # Default.

Don't use the following:

SQLITE_MAX_EXPR_DEPTH=0  # More efficient but unbounded.
SQLITE_OMIT_DEPRECATED=1  # Enforce deprecations, no performance benefits.
SQLITE_OMIT_PROGRESS_CALLBACK=1  # Slightly faster.
