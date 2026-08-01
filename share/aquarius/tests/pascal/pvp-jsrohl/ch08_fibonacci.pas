program Fibonacci(input,output);
    var n, i, penultimate, last, this : integer;
begin
    read(n);
    last := 0; this := 1;
    write(last, this);
    for i := 3 to n do
    begin
        penultimate := last;
        last := this;
        this := penultimate + last;
        write(this);
    end;
    writeln
end.
