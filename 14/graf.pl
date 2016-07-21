% graf

  graf([a,b,c,d]).
  sasiedzi(a, [kr(c,0), kr(b,1)]).
  sasiedzi(b, [kr(d,0)]).
  sasiedzi(c, [kr(c,0), kr(d,1)]).
  sasiedzi(d, [kr(c,0), kr(a,1), kr(d,1)]).
