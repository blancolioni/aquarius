program triangle(input, output);
    var a, b, c, s, area : real;
begin
    read(a, b, c);
    s := (a + b + c) / 2;
    area := sqrt(s * (s - a) * (s - b) * (s - c));
    write(area)
end.