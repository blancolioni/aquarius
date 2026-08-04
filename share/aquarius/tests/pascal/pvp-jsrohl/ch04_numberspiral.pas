program numberspiral(input, output);
    var n, r, x, y, i : integer;
begin
    read(n);
    r := trunc((sqrt(n)+1)/2);
    if n <= 4 * sqr(r) - 2*r then
        begin
            x := r;
            y := n - (4*sqr(r) - 3*r)
        end
    else if n <= 4 * sqr(r) then
        begin
            y := r;
            x := n - (4*sqr(r) - r) - n
        end
    else if n <= 4 * sqr(r) + 2*r then
        begin
            x := -r;
            y := (4*sqr(r) + r) - n
        end
    else
        begin
            y := -r;
            x := n - (4*sqr(r) + 3*r)
        end
end.
