#!/bin/sh
set -e -u

# Usage: install_texlive.sh

BIN="${BIN:-${HOME}/bin}"
TL_DIR="${TL_DIR:-${HOME}/.texlive}"
TL_REPO="${TL_REPO:-https://ctan.math.illinois.edu/systems/texlive/tlnet}"
TL_TAR="install-tl-unx.tar.gz"
TL_GPG="${TL_GPG:-$(cd "$(dirname "$0")" && pwd)/texlive.asc}"
TL_PKGS="${TL_PKGS:-$(cd "$(dirname "$0")" && pwd)/texlive_pkgs.txt}"

# Create a download directory.
tmp_dir="$(mktemp -d)"
cleanup () {
    trap - EXIT INT TERM
    rm -rf "$tmp_dir"
}
trap cleanup EXIT INT TERM

# Download, verify and unpack the texlive installer.
cd "$tmp_dir"
tl_url="${TL_REPO}/${TL_TAR}"
wget -q "$tl_url" "${tl_url}.sha512" "${tl_url}.sha512.asc"
gpg --dearmor < "$TL_GPG" > texlive.gpg
gpg --no-default-keyring --keyring texlive.gpg --homedir ./ \
    --verify "${TL_TAR}.sha512.asc"
sha512sum --check --quiet --status --strict "${TL_TAR}.sha512"
tar -xzf "$TL_TAR"
tl_install="$(tar -tzf "$TL_TAR" | head -n1 | sed -e 's#/.*$##')"

# Install a "portable" texlive base in a temporary location (atomic install).
tl_profile="infraonly.profile"
cat > "$tl_profile" <<EOF
selected_scheme scheme-infraonly
TEXDIR ./
TEXMFSYSCONFIG ./texmf-config
TEXMFLOCAL ./texmf-local
TEXMFSYSVAR ./texmf-var
TEXMFCONFIG \$TEXMFSYSCONFIG
TEXMFHOME \$TEXMFLOCAL
TEXMFVAR \$TEXMFSYSVAR
option_doc 0
option_src 0
option_autobackup 0
portable 1
tlpdbopt_sys_bin ${BIN}
EOF
mkdir texlive
cd texlive
TL_INSTALL_ENV_NOCHECK=true TL_INSTALL_NO_WELCOME=true \
    "../${tl_install}/install-tl" --no-gui \
    --logfile "../install-tl.log" \
    --profile "../${tl_profile}" \
    --repository "$TL_REPO"
rm -f install-tl bin/man bin/*/man

# Copy the portable install to its final location (atomic install).
if [ -e "${TL_DIR}/bin" ]; then
    tlmgr="$(find "${TL_DIR}/bin" -name tlmgr -print)"
    if [ -n "$tlmgr" ]; then
        "$tlmgr" path remove
    fi
fi
rm -rf "$TL_DIR"
mkdir -p "$TL_DIR"
cp -a ./ "${TL_DIR}/"

# Install packages.
tlmgr="$(find "${TL_DIR}/bin" -name tlmgr -print)"
"$tlmgr" option repository "$TL_REPO"
"$tlmgr" install $(tr '\n' ' ' < "$TL_PKGS")

# Place symlinks to executables in PATH.
mkdir -p "$BIN"
"$tlmgr" path add
