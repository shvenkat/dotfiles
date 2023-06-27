## Migrating LUKS KDF

    sudo cryptsetup luksDump /dev/whatever
    sudo cryptsetup convert /dev/whatever --type luks2
    sudo cryptsetup luksConvertKey /dev/whatever --pbkdf argon2id

See https://mjg59.dreamwidth.org/66429.html

## Customizing Firefox UI

<profile-dir>/chome/userChrome.css:

#unified-extensions-button {
  display: none !important;
}

Enable toolkit.legacyUserProfileCustomizations.stylesheets in about:config.
