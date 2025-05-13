#lang scribble/text
[Unit]
Description=@app-name %i

[Service]
User=signify
EnvironmentFile=@|destination|/environment-%i
WorkingDirectory=@|versions-path|/current-%i/
ExecStart=@|versions-path|/current-%i/bin/signify
Restart=on-failure

[Install]
WantedBy=multi-user.target
