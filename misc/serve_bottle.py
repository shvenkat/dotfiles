"""Run a static file HTTP server for a local directory mirroring a site.

Usage:
    python3 serve_bottle.py 127.0.0.1 8080 [quiet] [debug]

Logging: Unless --quiet is used, requests and error are logged to stderr. To save the log,
redirect stderr to a file. Use append-mode (i.e. >>redwood.log) to facilitate file truncation
during log rotation.
"""

import sys
from bottle import HTTPResponse, request, route, run, static_file


SITE_DIR = "/Users/shiv/tmp/panel.holoviz.org"


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
    actual_filepath = f"{path}?{query_string}" if query_string else path
    return static_file(
        actual_filepath, root=SITE_DIR, mimetype=mimetype, charset="UTF-8", headers=headers
    )


if __name__ == "__main__":
    args = sys.argv[1:]
    if len(args) < 2 or len(args) > 4:
        print("Usage error: ./serve_bottle.py 127.0.0.1 8080 [quiet] [debug]", file=sys.stderr)
        sys.exit(1)
    host, port = args[:2]
    quiet = "quiet" in args[2:]
    debug = "debug" in args[2:]
    run(host=host, port=int(port), quiet=quiet, debug=debug)  # type: ignore[arg-type]
