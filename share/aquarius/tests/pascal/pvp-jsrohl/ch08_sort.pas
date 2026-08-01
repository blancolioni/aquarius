{ A program to read in a sequence of integers, preceded by the
  number of integers, and sort them in ascending order. }
program sort(input,output);
var a : array [1 .. 100] of integer;
    n, minval, minsub, i, j : integer;
begin
    read(n);
    for i := 1 to n do
        read(a[i]);
    for i := 1 to n - 1 do
    begin
        minval := a[i];
        minsub := i;
        for j := i + 1 to n do
            if a[j] < minval then
            begin
                minval := a[j];
                minsub := j;
            end;
        a[minsub] := a[i];
        a[i] := minval;
    end;
    for i := 1 to n do
        write(a[i]);
    writeln
end.