# Firefox config to prioritize privacy, usability then security

## Update instructions

1. Obtain the stable version of ghacks/arkenfox user.js. Review the changes, and
   update user-overrides.js to remove obsolete preferences, modify existing
   ones, and add any new ones as needed.
2. Check whether to update extensions in extensions.txt. Ensure that extensions
   are still trustworthy, and that ownership has not changed. Check with ghacks
   list of recommended extensions.
3. Export and review current extension configuration.
4. Update firefox.
5. ``bin/ff --new``, check about:config and the console for any preference
   errors.
6. Import extension configuration, review/modify, re-export and save.
7. If something breaks, check the troubleshooting guide on the ghacks wiki and
   the extensions known to be broken.

## TODO

* [ ] Construct alternative profiles for video conference, media playback. See
      ghacks user-override recipes.
* [ ] Use theme colors to visually identify different profiles.
* [ ] Examine SQLite files in the profile directory.
* [ ] Avoid using the Downloads directory as the "cwd" for any shell or process.
* [ ] Remove/disable System Add-ons.
* [ ] Use an IP filter to restrict access to Facebook, etc. See https://tech.michaelaltfield.net/2019/03/26/ephemeral-firefox-as-a-site-specific-browser-03/
