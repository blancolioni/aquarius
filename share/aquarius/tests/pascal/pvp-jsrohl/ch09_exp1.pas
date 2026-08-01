{ This program evaluates and prints the value of exp(x) by 
summing n+1 terms of the Taylor series expansion. }
program exp1(input,output);
    var x, term, sum : real;
        n, i : integer;
begin
    read(x, n);
    term := 1.0;
    sum := term;
    for i := 1 to n do
    begin
        term := term * x / i;
        sum := sum + term;
    end;
    writeln('exp(', x:0:2, ') = ', sum:0:6);
end.