# Compile and install Python from source code

1. Select an install location.

       PREFIX="${HOME}/.python/3.10.5"
       mkdir -p "$PREFIX"

2. Download, verify and unpack the source code.

       VERSION='3.10.5'
       URL="https://www.python.org/ftp/python/${VERSION}"
       TAR="Python-${VERSION}.tar.xz"
       tmp_dir="$(mktemp -d)"
       cd "$tmp_dir"
       wget -q "${URL}/${TAR}" "${URL}/${TAR}.asc"
       gpg --dearmor < ~/.dotfiles/python/python.asc > python.gpg
       gpg --no-default-keyring --keyring python.gpg --homedir ./ --verify "${TAR}.asc"
       xz --decompress < "$TAR" | tar -xf -

3. Install required libraries and headers.

       sudo apt-get install tk-dev libgdbm-dev libdb-dev

4. Compile, test and install.

       src_dir="${TAR%.tar.xz}"
       cd "$src_dir"
       ./configure --prefix="$PREFIX" --enable-optimizations --with-lto \
           --enable-loadable-sqlite-extensions --with-ensurepip=upgrade
       make -j
       make testall
       make [alt]install  # altinstall omits the generic "python3" symlink.
