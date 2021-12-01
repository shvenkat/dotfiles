
// ----------  OVERRIDE ghacks/arkenfox  -------------------------------------

// See github.com/ghacksuserjs/ghacks-user.js
user_pref("_user.js.parrot", "overriding ghacks ... error!");

// 0201: Disable Location-Aware Browsing. This is a fingerprinting risk.
user_pref("geo.enabled", false);
// 0202: Set a default permission for Location (see 0201) [FF58+]
// 0=always ask (default), 1=allow, 2=block
// user_pref("permissions.default.geo", 2);

// 0302b: Disable auto-INSTALLING extension and theme updates (after the check in 0301b).
user_pref("extensions.update.autoUpdateDefault", false);
// 0306: disable extension metadata
user_pref("extensions.getAddons.cache.enabled", false);

// 0419: Disable 'ignore this warning' on SB warnings.
user_pref("browser.safebrowsing.allowOverride", false);

// 0515: Disable the built-in Screenshots extension.
user_pref("extensions.screenshots.disabled", true); // [FF55+]
user_pref("extensions.screenshots.upload-disabled", true); // [FF60+]

// 0708: Disable FTP [FF60+], an insecure protocol.
user_pref("network.ftp.enabled", false);

// 0805: Enable coloring of visited links. Firefox tries not to leak history.
// user_pref("layout.css.visited_links_enabled", true);

// 0850a: Customize location bar suggestion types.
// user_pref("browser.urlbar.suggest.history", false);
// user_pref("browser.urlbar.suggest.bookmark", false);
// user_pref("browser.urlbar.suggest.openpage", false);
user_pref("browser.urlbar.suggest.topsites", false); // [FF78+]

// 0862: Disable browsing and download history.
user_pref("places.history.enabled", false);

// 0901: Disable saving passwords.
user_pref("signon.rememberSignons", false);

// 1244: Enable HTTPS-Only mode [FF76+].
// user_pref("dom.security.https_only_mode", true);  // true in arkenfox v87.
user_pref("dom.security.https_only_mode_pbm", true);  // No effect if above is true.
// user_pref("dom.security.https_only_mode.upgrade_local", true);

// 1261: Disable 3DES. This is a fingerprinting risk.
user_pref("security.ssl3.rsa_des_ede3_sha", false);

// 1603: CROSS ORIGIN: Control when to send a referer.
// 0=always (default), 1=only if base domains match, 2=only if hosts match
user_pref("network.http.referer.XOriginPolicy", 1);
// 1604: CROSS ORIGIN: Control the amount of information to send.
// 0=send full URI (default), 1=scheme+host+port+path, 2=scheme+host+port
user_pref("network.http.referer.XOriginTrimmingPolicy", 2);
// 1606: ALL: set the default Referrer Policy [FF59+]
// 0=no-referer, 1=same-origin, 2=strict-origin-when-cross-origin, 3=no-referrer-when-downgrade
user_pref("network.http.referer.defaultPolicy", 2);
user_pref("network.http.referer.defaultPolicy.pbmode", 2);

// 1703: Display the container menu when using the "+" button to open a new tab.
user_pref("privacy.userContext.newTabContainerOnLeftClick.enabled", true);

// 1820: disable GMP (Gecko Media Plugins)
// user_pref("media.gmp-provider.enabled", false);

// 2024: Set a default permission for Camera/Microphone.
// 0=always ask (default), 1=allow, 2=block
user_pref("permissions.default.camera", 2);
user_pref("permissions.default.microphone", 2);

// 2030: Disable autoplay of HTML5 media.
// 0=Allow all, 1=Block non-muted media (default in FF67+), 5=Block all (FF69+)
user_pref("media.autoplay.default", 5);

// 2204: Disable Fullscreen API. Use F11 instead.
user_pref("full-screen-api.enabled", false);

// 2304: Disable Web Notifications.
user_pref("dom.webnotifications.enabled", false);
user_pref("dom.webnotifications.serviceworker.enabled", false);

// 2402: Disable website access to clipboard events/content.
user_pref("dom.event.clipboardevents.enabled", false);

// 2421: Disable Ion and baseline JIT to harden against JS exploits.
// Extensions (trusted principals) can still use Ion.
user_pref("javascript.options.ion", false);
user_pref("javascript.options.baselinejit", false);
user_pref("javascript.options.jit_trustedprincipals", true);

// 2502: Disable Battery Status API.
user_pref("dom.battery.enabled", false);

// 2508: Disable hardware acceleration to reduce graphics fingerprinting.
// user_pref("layers.acceleration.disabled", true);

// 2520: Disable virtual reality devices.
user_pref("dom.vr.enabled", false);
// 2521: Set a default permission for Virtual Reality.
// 0=always ask (default), 1=allow, 2=block
user_pref("permissions.default.xr", 2);

// 2615: Disable websites overriding Firefox's keyboard shortcuts.
// 0 (default) or 1=allow, 2=block
user_pref("permissions.default.shortcuts", 2);

// 2650: Discourage downloading to desktop.
// 0=desktop, 1=downloads (default), 2=last used
user_pref("browser.download.folderList", 1);
// 2651: Always ask where to download, for security.
user_pref("browser.download.useDownloadDir", false);
// 2654: Disable "open with" in download dialog.
user_pref("browser.download.forbid_open_with", true);

// 2701: Disable 3rd-party cookies and site-data.
// 0=Accept cookies and site data
// 4=(Block) Cross-site and social media trackers (default)
// 5=(Isolate All) Cross-site cookies (Total Cookie Protection / dynamic FPI)
// 3=(Block) Cookies from unvisited websites
// 1=(Block) All third-party cookies
// 2=(Block) All cookies
user_pref("network.cookie.cookieBehavior", 1);
user_pref("browser.contentblocking.category", "custom");
// 2703: delete cookies and site data on close
// 0=keep until they expire (default), 2=keep until you close Firefox
user_pref("network.cookie.lifetimePolicy", 2);

user_pref("_user.js.parrot", "overriding ghacks ... complete");

// ----------  PERSONAL PREFERENCES  -----------------------------------------

user_pref("_user.js.parrot", "applying personal prefs ... error!");

// [SECTION 5000]: PERSONAL
// WELCOME & WHAT's NEW NOTICES
user_pref("browser.startup.homepage_override.mstone", "ignore"); // master switch
user_pref("startup.homepage_welcome_url", "");
user_pref("startup.homepage_welcome_url.additional", "");
user_pref("startup.homepage_override_url", ""); // What's New page after updates
// WARNINGS
user_pref("browser.tabs.warnOnClose", false);
user_pref("browser.tabs.warnOnCloseOtherTabs", false);
user_pref("browser.tabs.warnOnOpen", false);
user_pref("full-screen-api.warning.delay", 0);
user_pref("full-screen-api.warning.timeout", 0);
// CONTENT BEHAVIOR
user_pref("accessibility.typeaheadfind", false);
user_pref("clipboard.autocopy", false); // disable autocopy default [LINUX]
user_pref("layout.spellcheckDefault", 0); // 0=none, 1-multi-line, 2=multi-line & single-line
// UX BEHAVIOR
user_pref("browser.backspace_action", 2); // 0=previous page, 1=scroll up, 2=do nothing
user_pref("browser.tabs.closeWindowWithLastTab", false);
user_pref("browser.tabs.loadBookmarksInTabs", true); // open bookmarks in a new tab [FF57+]
user_pref("general.autoScroll", false);
// UX FEATURES: disable and hide the icons and menus ***/
user_pref("browser.messaging-system.whatsNewPanel.enabled", false); // What's New [FF69+]
user_pref("extensions.pocket.enabled", false); // Pocket Account [FF46+]
user_pref("identity.fxaccounts.enabled", false); // Firefox Accounts & Sync [FF60+] [RESTART]
// OTHER
user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons", false); // disable CFR [FF67+]
user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features", false); // disable CFR [FF67+]
user_pref("network.manage-offline-status", false); // see bugzilla 620472
user_pref("xpinstall.signatures.required", true);

// ETP: enhanced tracking protection. Redundant with arkenfox/user.js v87.
user_pref("privacy.trackingprotection.enabled", true);  // Default false.
user_pref("privacy.trackingprotection.pbmode.enabled", true);  // Default true.
user_pref("privacy.trackingprotection.socialtracking.enabled", true);  // Default false.
user_pref("privacy.trackingprotection.fingerprinting.enabled", true);  // Default true.
user_pref("privacy.trackingprotection.cryptomining.enabled", true);  // Default true.

// SEARCH: Setting these doesn't work. Use search.json.mozlz4 instead.
// user_pref("browser.search.defaultenginename", "DuckDuckGo");
// user_pref("browser.urlbar.placeholderName", "DuckDuckGo");
// user_pref("browser.search.hiddenOneOffs", "Google,Amazon.com");

// Do not reset bookmarks.
user_pref("browser.bookmarks.restore_default_bookmarks", false);

// Only install extensions from whitelisted sources.
user_pref("xpinstall.whitelist.required", true);

// Highlight all matches.
user_pref("findbar.highlightAll", true);

// Developer Tools.
user_pref("devtools.accessibility.enabled", false);
user_pref("devtools.application.enabled", false);
user_pref("devtools.command-button-measure.enabled", true);
user_pref("devtools.command-button-rulers.enabled", true);
user_pref("devtools.inspector.showUserAgentStyles", true);
user_pref("devtools.memory.enabled", false);
user_pref("devtools.performance.enabled", false);
user_pref("devtools.screenshot.audio.enabled", false);
user_pref("devtools.serviceWorkers.testing.enabled", false);

// Fonts.
user_pref("font.default.x-western", "sans-serif");
user_pref("font.name.monospace.x-western", "Fira Code");
user_pref("font.name.sans-serif.x-western", "Noto Sans");
user_pref("font.name.serif.x-western", "Noto Serif");
user_pref("font.minimum-size.x-western", "12");
user_pref("font.size.monospace.x-western", "18");
user_pref("font.size.variable.x-western", "18");

// Misc.
// user_pref("lightweightThemes.selectedThemeID", "firefox-compact-light@mozilla.org");
user_pref("extensions.activeThemeID", "default-theme@mozilla.org");
user_pref("reader.color_scheme", "sepia");

user_pref("_user.js.parrot", "applying personal prefs ... complete");
