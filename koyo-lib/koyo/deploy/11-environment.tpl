PLTUSERHOME=@destination
@(~env-var 'HTTP_HOST)=127.0.0.1
@(~env-var 'HTTP_PORT)=@(hash-ref ports target-variant)
@in[var environment]{
  @(car var)=@(cdr var)
}
