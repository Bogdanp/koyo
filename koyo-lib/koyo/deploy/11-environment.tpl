HOST=127.0.0.1
PORT=@(hash-ref ports target-variant)
PLTUSERHOME=@destination
@(~env-var 'HTTP_HOST)=127.0.0.1
@(~env-var 'HTTP_PORT)=@(hash-ref ports target-variant)
@(~env-var 'VARIANT)=@target-variant
@(add-newlines
  @in[var environment]{
    @(car var)=@(cdr var)
  })
