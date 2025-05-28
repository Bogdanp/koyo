#lang scribble/text
[Unit]
Description=@app-name %i

[Service]
User=@user
EnvironmentFile=@|destination|/environment-%i
WorkingDirectory=@|versions-path|/current-%i/
ExecStart=@|versions-path|/current-%i/bin/@exec-name @exec-flags
Restart=on-failure
RestartSec=30
LimitNOFILE=65535

[Install]
WantedBy=multi-user.target
