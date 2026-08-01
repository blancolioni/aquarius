{ This reads two numbers and prints them and their HCF.
The HCF is calculated using Euclid's algorithm. }
program Euclid(input,output);
    var p, q, dividend, divisor, remainder : integer;
begin
    read(p, q);
    dividend := p;
    divisor := q;
    remainder := dividend mod divisor;
    while remainder <> 0 do
    begin
        dividend := divisor;
        divisor := remainder;
        remainder := dividend mod divisor;
    end;
    writeln('HCF of ', p, ' and ', q, ' is ', divisor);
end.