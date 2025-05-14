#lang scribble/doc

@(require scribble/core
          "koyo.rkt")

@title[#:tag "deployment"]{Deployment}

@(define (flag name)
   (list "the " (make-element 'tt (list (format "--~a" name))) " flag"))

The @tt{raco koyo deploy} tool can be used in conjunction with @tt{raco
koyo dist} to perform blue-green deployments. For example:

@verbatim[#:indent 2]{
  $ cd link-shortener
  $ npm run build
  $ raco koyo dist ++lang north
  $ raco koyo deploy dist/ v1.0 target-host-1 target-host-2 ...
}

The tool works by copying the given distribution to each of the target
hosts, installing a systemd service, and tracking some state about
which variant of the service is currently running. After uploading the
distribution, it checks which variant is running, and starts the other
variant, then runs health checks (if enabled) on the target variant. It
writes an Nginx config file listing the @tt{host:port} that the target
variant is listening on and reloads Nginx. Finally, it stops the old
variant and deletes any existing versions older than 30 days.

The target host must

@itemlist[
 @item{be running some Linux variant with systemd,}
 @item{have Nginx installed as a systemd service, and}
 @item{have an Nginx virtual host pointing at the
  @filepath{backend.conf} file.}
]

For example, given an application called ``link-shortener'', a call to
@tt{raco koyo deploy dist/ v1.0 target-host} will deploy the following
files to @tt{target-host}:

@itemlist[
 @item{@filepath{/etc/systemd/system/link-shortener@"@".service}
  --- the systemd service template that is used to run each variant
  (eg. @tt{link-shortener@"@"blue.service}).}
 @item{@filepath{/opt/link-shortener/backend.conf} --- an Nginx config
  listing the host & port the current version is listening on.}
 @item{@filepath{/opt/link-shortener/environment-blue} --- environment
  variables associated with the last-deployed blue variant.}
 @item{@filepath{/opt/link-shortener/environment-green} --- environment
  variables associated with the last-deployed green variant.}
 @item{@filepath{/opt/link-shortener/variant} --- contains the variant
  that was last deployed successfully.}
 @item{@filepath{/opt/link-shortener/versions/2025_05_13T00_00_00-v1.0}
  --- a copy of @tt{dist}.}
 @item{@filepath{/opt/link-shortener/versions/current-blue} --- a
  symlink to the last-deployed blue variant.}
 @item{@filepath{/opt/link-shortener/versions/current-green} --- a
  symlink to the last-deployed green variant.}
 @item{@filepath{/opt/link-shortener/versions/current} --- a symlink to
  the last-deployed variant.}
]

The application name is determined by looking at the name of the
directory the tool is invoked in. The default deployment destination
is a path constructed by prefixing the application name with
@filepath{/opt/} (eg. @filepath{/opt/link-shortener/}). The application
name can be customized using @flag{app-name} and the destination using
@flag{destination}.

When health checking is enabled (@flag{health-check}), the app's
executable is called with a @tt{-c PORT} flag and deployment aborts if
it exits with a non-zero exit code.

You can specify environment variables using the @tt{-e} flag:

@verbatim[#:indent 2]{
 $ raco koyo deploy \
     -e LINK_SHORTENER_DEBUG false \
     -e LINK_SHORTENER_URL_HOST link-shortener.example \
     dist v1.0 target-host
}

The environment variables are written to the @filepath{environment-blue}
and @filepath{environment-green} files on deploy, depending on the
variant being deployed, and loaded into the app's runtime environment
by the systemd service. The @tt{PLTUSERHOME}, @tt{APP_NAME_HTTP_HOST}
and @tt{APP_NAME_HTTP_PORT} environment variables are set automatically
based on the target environment.

By default, the @tt{blue} variant listens on localhost port
@racket[8001] and the @tt{green} variant on @racket[8002]. These ports
can be customized passing @flag{port}. For example:

@verbatim[#:indent 2]{
 $ raco koyo deploy \
     -p blue 8000 \
     -p green 9000 \
     dist v1.0 target-host
}

@section{Pre and Post Scripts}

You can run arbitrary scripts before and after a service is deployed.
The tool accepts @flag{pre-script} and @flag{post-script} for this
purpose. The value of each of these flags must be the path to a script
that will be copied to the target server and executed as a bash script
with the old variant and the new variant as arguments.

The pre script executes after the service is installed, after the
environment variable file is expanded, but before the service is
started. The post script is executed after the service is successfully
started, but before old deployments are deleted.

@section{Additional SSH Flags}

The tool calls @tt{ssh} and @tt{rsync} to perform the deployment
steps. Each call to these tools includes the flags @tt{-T -o
StrictHostKeyChecking=accept-new}. You can add flags to this list using
@flag{ssh-flags}.

One way to simplify SSH configuration is to store an infrastructure
map in the form of an @tt{ssh_config} file in the repo, then pass that
file to the tool using @flag{ssh-flags}. For example:

@verbatim[#:indent 2]{
 $ raco koyo deploy \
     --ssh-flags '-F infra/ssh_config' \
     dist v1.0 prod-01 prod-02
}

Where @filepath{infra/ssh_config} might look something like this:

@verbatim[#:indent 2]|{
Host bastion # jump host
  User root
  Port 22
  HostName X.XXX.XXX.XX
  IdentityFile ~/.ssh/deploy-key
  HostKeyAlias bastion

Host prod-01 # app server 1
  User root
  Port 22
  HostName 10.0.0.1
  ProxyJump bastion
  IdentityFile ~/.ssh/deploy-key
  HostKeyAlias prod-01

Host prod-02 # app server 2
  User root
  Port 22
  HostName 10.0.0.2
  ProxyJump bastion
  IdentityFile ~/.ssh/deploy-key
  HostKeyAlias prod-02
}|

In this configuration, the publicly-accessible @tt{bastion} is used as a
jump box to connect to the @tt{prod-01} and @tt{prod-02} hosts through a
private network and perform the deployment.

@section{Sudo}

The tool uses @tt{sudo} to perform the following commands:

@itemlist[
 @item{@tt{sudo chown -R <user>:<group> <destination>}}
 @item{@tt{sudo systemctl daemon-reload}}
 @item{@tt{sudo systemctl reload nginx}}
 @item{@tt{sudo systemctl start <app-name>@"@"blue.service}}
 @item{@tt{sudo systemctl start <app-name>@"@"green.service}}
 @item{@tt{sudo systemctl stop <app-name>@"@"blue.service}}
 @item{@tt{sudo systemctl stop <app-name>@"@"green.service}}
 @item{@tt{sudo tee /etc/systemd/system/<app-name>@"@".service}}
]

When running as a non-root user, it needs to be able to perform these
commands without being asked for a password. You can add the following
configuration file to @filepath{/etc/sudoers.d/} to allow the SSH user
to run these commands without being prompted for a password:

@margin-note{
 Don't forget to replace the values in angle brackets with the
 appropriate values for your app.
}

@verbatim[#:indent 2]|{
<user> \
  ALL=(ALL) \
  NOPASSWD: \
    /bin/chown -R <user>:<group> <destination>, \
    /bin/systemctl daemon-reload, \
    /bin/systemctl reload nginx, \
    /bin/systemctl start <app-name>@blue.service, \
    /bin/systemctl start <app-name>@green.service, \
    /bin/systemctl stop <app-name>@blue.service, \
    /bin/systemctl stop <app-name>@green.service, \
    /bin/tee /etc/systemd/system/<app-name>@.service
}|

@section{Example Nginx Config}

Here is an example Nginx virtual host config that points at a
deployed @filepath{backends.conf} and proxies incoming requests
to the deployed app. Static files are served directly from
@filepath{/opt/link-shortener/versions/current/static/}.

@verbatim|{
 upstream link_shortener_backend {
   include /opt/link-shortener/backends.conf;

   keepalive 128;
 }

 server {
   listen 80;
   server_name link-shortener.example;

   gzip            on;
   gzip_vary       on;
   gzip_proxied    any;
   gzip_comp_level 6;
   gzip_types      text/plain text/css text/xml application/json application/javascript application/xml+rss application/atom+xml image/svg+xml;

   location /static/ {
     root /opt/link-shortener/versions/current;

     sendfile          on;
     tcp_nopush        on;
     tcp_nodelay       on;
     keepalive_timeout 65;

     expires    max;
     add_header Cache-Control public;
   }

   location / {
     client_max_body_size 20M;

     add_header "Referrer-Policy" "no-referrer" always;
     add_header "X-Content-Type-Options" "nosniff" always;
     add_header "X-Frame-Options" "DENY" always;
     add_header "X-XSS-Protection" "1; mode=block" always;

     proxy_pass             http://link_shortener_backend;
     proxy_http_version     1.1;
     proxy_connect_timeout  60s;
     proxy_send_timeout     60s;
     proxy_read_timeout     60s;
     proxy_set_header       Connection      "";
     proxy_set_header       Host            $host;
     proxy_set_header       X-Forwarded-For $proxy_add_x_forwarded_for;
     proxy_set_header       X-Real-IP       $remote_addr;
     proxy_intercept_errors on;
   }
 }
}|
