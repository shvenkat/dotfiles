// ----------  Search  -------------------------------------------------------

// Default search engine.
user_pref("browser.search.defaultenginename", "DuckDuckGo");
// user_pref("browser.search.defaultenginename.US", "DuckDuckGo");
// user_pref("browser.search.region", "US");
// user_pref("browser.search.countryCode", "US");

// Enable search engine suggestions.
user_pref("browser.search.suggest.enabled", true);

// Combine URL and search bar.
user_pref("browser.search.widget.inNavBar", true);


// ----------  Browsing  -----------------------------------------------------

// Block phishing sites. See https://wiki.mozilla.org/Safe_Browsing.
user_pref("browser.safebrowsing.phishing.enabled", true);

// Attempt to block tracking. See https://wiki.mozilla.org/Security/Tracking_protection.
user_pref("privacy.donottrackheader.enabled", true);
user_pref("privacy.trackingprotection.enabled", true);
user_pref("privacy.trackingprotection.pbmode.enabled", true);

// Disable popups (exceptions allowed).
user_pref("dom.disable_open_during_load", true);

// Site verification for HTTPS connections.
user_pref("security.default_personal_cert", "Ask Every Time");
user_pref("security.OCSP.enabled", 1);
user_pref("security.OCSP.require", false);

// Use worker processes for rendering and remote code execution.
// user_pref("browser.tabs.remote.autostart", true);
user_pref("browser.tabs.remote.autostart.2", true);

// Do not reset bookmarks.
user_pref("browser.bookmarks.restore_default_bookmarks", false);


// ----------  Browser  ------------------------------------------------------

// Keep browser up to date.
user_pref("app.update.enabled", true);
user_pref("app.update.auto", true);
user_pref("browser.search.update", true);

// Disable the request to set the default browser.
user_pref("browser.shell.checkDefaultBrowser", false);

// Opt out of statistics and error reporting.
user_pref("datareporting.healthreport.uploadEnabled", false);
user_pref("app.shield.optoutstudies.enabled", false);
user_pref("browser.crashReports.unsubmittedCheck.autoSubmit", false);


// ----------  Plugins and Extensions  ---------------------------------------

// Disable certain plugins.
user_pref("plugin.state.flash", 0);
// Java, Silverlight, Acrobat and other NPAPI plugins are no longer supported.
// user_pref("plugin.state.java", 0);

// Disable binary blob plugins for handling DRM content.
user_pref("media.eme.enabled", false);

// Disable bundled third-party extensions.
user_pref("extensions.pocket.enabled", false);

// Only install signed extensions from whitelisted sources.
user_pref("xpinstall.whitelist.required", true);
user_pref("xpinstall.signatures.required", true);


// ----------  Downloads  ----------------------------------------------------

// Download directory.
user_pref("browser.download.useDownloadDir", true);
user_pref("browser.download.lastDir", "DOWNLOAD_DIR");

// Block (potential) malware and unwanted software.
// See https://wiki.mozilla.org/Security/Application_Reputation.
user_pref("browser.safebrowsing.malware.enabled", true);
user_pref("browser.safebrowsing.downloads.enabled", true);

// Use a remote service (such as one provided by Google) to detect malware.
// See https://wiki.mozilla.org/Security/Download_Protection.
// See https://feeding.cloud.geek.nz/posts/how-safe-browsing-works-in-firefox/.
user_pref("browser.safebrowsing.downloads.remote.enabled", true);
user_pref("browser.safebrowsing.downloads.remote.block_dangerous", true);
user_pref("browser.safebrowsing.downloads.remote.block_dangerous_host", true);
user_pref("browser.safebrowsing.downloads.remote.block_potentially_unwanted", true);
user_pref("browser.safebrowsing.downloads.remote.block_uncommon", true);


// ----------  Autofill and Passwords  ---------------------------------------

// Do not store passwords in the browser.
user_pref("signon.rememberSignons", false);
user_pref("signon.autofillForms", false);

// Do not store form data or search terms.
user_pref("browser.formfill.enable", false);


// ----------  Local Storage  ------------------------------------------------

// Use custom settings for saving history.
user_pref("browser.privatebrowsing.autostart", false);

// Do not remember history. Does not affect persistence of windows and tabs.
user_pref("places.history.enabled", false);

// Accept cookies, including third-party cookies from visited sites.
user_pref("network.cookie.cookieBehavior", 3);

// Keep cookies until they go stale.
user_pref("network.cookie.lifetimePolicy", 0);
user_pref("network.cookie.thirdparty.sessionOnly", false);

// On shutdown, clear/sanitize certain categories of data; persist others (see below).
user_pref("privacy.sanitize.sanitizeOnShutdown", true);
user_pref("privacy.sanitize.timeSpan", 0);

// Persist: windows and tabs; cookies and site settings; and offline app data.
// Clear/sanitize: cache, non-cookie based sessions and form data.
user_pref("privacy.clearOnShutdown.openWindows", false);
user_pref("privacy.clearOnShutdown.history", false);
user_pref("privacy.clearOnShutdown.downloads", false);
user_pref("privacy.clearOnShutdown.cookies", false);
user_pref("privacy.clearOnShutdown.siteSettings", false);
user_pref("privacy.clearOnShutdown.offlineApps", false);
user_pref("privacy.clearOnShutdown.cache", true);
user_pref("privacy.clearOnShutdown.sessions", true);
user_pref("privacy.clearOnShutdown.formdata", true);

// Clear/persist the above data categories on demand.
user_pref("privacy.cpd.openWindows", false);
user_pref("privacy.cpd.history", false);
user_pref("privacy.cpd.downloads", false);
user_pref("privacy.cpd.cookies", false);
user_pref("privacy.cpd.siteSettings", false);
user_pref("privacy.cpd.offlineApps", false);
user_pref("privacy.cpd.cache", true);
user_pref("privacy.cpd.sessions", true);
user_pref("privacy.cpd.formdata", true);


// ----------  UI  -----------------------------------------------------------

// On launch, re-open windows and tabs from the previous session.
user_pref("browser.startup.page", 3);

// New windows and tabs start with a blank page. Not "top" sites or ads.
user_pref("browser.startup.homepage", "about:blank");
// Disabling the "enhanced new tab" feature requires an add-on since v. 41. ಠ_ಠ
// user_pref("browser.newtabpage.enabled", false);
// user_pref("browser.newtabpage.enhanced", false);
// So, disable all sections of the "enhanced new tab".
user_pref("browser.newtabpage.activity-stream.feeds.section.highlights", false);
user_pref("browser.newtabpage.activity-stream.feeds.section.topstories", false);
user_pref("browser.newtabpage.activity-stream.feeds.snippets", false);
user_pref("browser.newtabpage.activity-stream.prerender", false);
user_pref("browser.newtabpage.activity-stream.showSearch", false);
user_pref("browser.newtabpage.activity-stream.showTopSites", false);

// Search text only on request. Highlight matches.
user_pref("accessibility.typeaheadfind", false);
user_pref("findbar.highlightAll", true);

// Spell-check text in forms.
user_pref("layout.spellcheckDefault", 1);
user_pref("spellchecker.dictionary", "en-US");

// Disable auto-scroll.
user_pref("general.autoScroll", false);


// ----------  Developer Tools  ----------------------------------------------

user_pref("devtools.inspector.showUserAgentStyles", true);
user_pref("devtools.performance.enabled", false);
user_pref("devtools.responsiveUI.rotate", false);
user_pref("devtools.storage.enabled", true);
user_pref("devtools.theme", "light");

// ----------  Misc  ---------------------------------------------------------

user_pref("lightweightThemes.selectedThemeID", "firefox-compact-light@mozilla.org");
user_pref("reader.color_scheme", "sepia");
user_pref("general.warnOnAboutConfig", false);
