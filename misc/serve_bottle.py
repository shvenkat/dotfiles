"""Run a static file HTTP server for a local directory mirroring a site.

Usage:
    python3 serve_bottle.py site-dir 127.0.0.1 8080 [quiet] [debug]

Logging: Unless --quiet is used, requests and error are logged to stderr. To save the log,
redirect stderr to a file. Use append-mode (i.e. >>redwood.log) to facilitate file truncation
during log rotation.
"""

import sys
import urllib.parse

from bottle import HTTPResponse, request, route, run, static_file


SITE_DIR = "."


@route("/")
def root() -> HTTPResponse:
    return static_file("index.html", root=SITE_DIR, mimetype="auto", charset="UTF-8")


@route("/<filepath:path>")
def serve_file(filepath: str) -> HTTPResponse:
    """Serve a file whose name may include the URL query.

    Args:
        filepath: File path relative to ``SITE_DIR``. MIME type will be automatically set
            based on the file extension, so use ``.js`` or ``.css`` as appropriate.

    Returns:
        File contents. These are transmitted as is.
    """
    headers = {}
    if filepath.endswith(".gz"):
        headers["Content-Encoding"] = "gzip"
    if filepath.endswith(".js") or filepath.endswith(".js.gz"):
        mimetype = "text/javascript"
    elif filepath.endswith(".css") or filepath.endswith(".css.gz"):
        mimetype = "text/css"
    else:
        mimetype = "auto"
    _, _, path, query_string, _ = request.urlparts
    path = urllib.parse.unquote(path)
    query_string = urllib.parse.unquote(query_string)
    actual_filepath = f"{path}?{query_string}" if query_string else path
    return static_file(
        actual_filepath, root=SITE_DIR, mimetype=mimetype, charset="UTF-8", headers=headers
    )


if __name__ == "__main__":
    args = sys.argv[1:]
    if len(args) < 3 or len(args) > 5:
        print("Usage error: ./serve_bottle.py site-dir 127.0.0.1 8080 [quiet] [debug]", file=sys.stderr)
        sys.exit(1)
    site_dir, host, port = args[:3]
    quiet = "quiet" in args[3:]
    debug = "debug" in args[3:]
    SITE_DIR = site_dir
    run(host=host, port=int(port), quiet=quiet, debug=debug)  # type: ignore[arg-type]
