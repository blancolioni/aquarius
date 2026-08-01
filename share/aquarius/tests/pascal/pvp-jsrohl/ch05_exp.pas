program exp(input, output);
    var n, i : integer;
        x, sum, term : real;
begin
    sum := 0;
    read(n,x);
    term := 1;
    sum := 1;
    for i := 1 to n do
    begin
        term := term * x / i;
        sum := sum + term;
    end;
    write(n,sum)
end.