program averages(input, output);
    var i, j, m, n : integer;
        sum, next, average : real;
begin
    read(m);
    for i := 1 to m do
    begin
        sum := 0;
        read(n);
        for j := 1 to n do
        begin
            read(next);
            sum := sum + next;
        end;
        average := sum / n;
        write(sum, average);
        writeln
    end
end.