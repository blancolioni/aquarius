program payroll(input, output);
var code : integer; hours, rate, gross, overtime : real;
begin
read(code, hours, rate);
overtime := hours - 37.5;
gross := 37.5 * rate + overtime * 1.5 * rate;
write(code,gross)
end.