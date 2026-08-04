program payroll(input, output);
    var code, n, i : integer;
        hours, rate, gross, overtime : real;
begin
    read(n);
    for i := 1 to n do
    begin
        read(code, hours, rate);
        overtime := hours - 37.5;
        if overtime < 0 then gross := hours * rate
        else gross := 37.5 * rate + overtime * 1.5 * rate;
       write(code,gross);
        writeln
    end
end.
