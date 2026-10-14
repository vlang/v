# net.http.file

`file.serve` serves static files from its configured `folder`.
Requests may access that folder and its descendants after resolving the requested path.
Paths outside the folder return HTTP 403, including sibling folders with a shared name prefix
and symlinks whose targets are outside the served folder.
Symlinks that resolve inside the folder remain accessible.
The same containment check applies to the configured index file for directory requests.
