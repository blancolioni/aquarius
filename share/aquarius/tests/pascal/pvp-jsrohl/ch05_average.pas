program average(input, output);
    var n, i : integer;
        sum, next, average : real;
begin
    sum := 0;
    read(n);
    for i := 1 to n do
    begin
        read(next);
        sum := sum + next;
    end;
    average := sum / n;
    write(sum, average);
    writeln
end.