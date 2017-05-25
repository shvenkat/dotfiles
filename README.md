# dotfiles

At home anywhere.

# Install.

1.  Download the required files from the online repository, using one of the following options. The
    repository URL is https://github.com/shvenkat/dotfiles.

    If you get SSL errors because the relevant certificate authority certificates are not available
    on your system, install these certificates. Or consider using an insecure connection with `curl
    --insecure` or `wget --no-check-certificate`.

    * Requires curl/wget, tar and gzip:
        ```
        mkdir dotfiles \
        && curl -fsSL https://github.com/shvenkat/dotfiles/archive/master.tar.gz \
        | tar -xzf- -C dotfiles \
        && cd dotfiles
        ```

    * Requires git:
        ```
        git clone https://github.com/shvenkat/dotfiles \
        && cd dotfiles
        ```

2. Run the installer: `install/install bootstrap`.
