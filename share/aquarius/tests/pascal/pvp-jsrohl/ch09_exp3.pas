{ This program evaluates and prints the value of exp(x) by 
summing sufficient terms of the Taylor series expansion
to produce a relative accuracy eps where x and eps are read
as data. }
program exp2(input,output);
    var x, eps, term, sum : real;
        i : integer;
begin
    read(x, eps);
    term := 1.0;
    sum := term;
    i := 1;
    repeat
        term := term * x / i;
        sum := sum + term;
        i := i + 1;
    until term < eps * sum;
    writeln('exp(', x:0:2, ') = ', sum:0:6);
end.