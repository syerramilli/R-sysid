% Generating an IDPOLY object of type ARMAX
proc_armax = idpoly([1 -1.2 0.35],[0 0 2],[1 0.3],'Noisevariance',1);
% Simulate the process with a PRBS input
uk = idinput(2046,'prbs',[0 0.4],[-1 1]); % Full -length PRBS
yktrue = sim(proc_armax ,uk); % Noise -free simulation
% Adjusting the noise variance s.t. SNR = 10
mult_fact = (1 + 0.3^2)*(1 + 0.35)/((1 + 0.35)*(1 - 0.35^2) - 1.2^2 + ...
0.35*1.2^2);
proc_armax.Noisevar = 0.1*var(yktrue)/mult_fact;
% Simulate with noise
yk = sim(proc_armax ,uk,simOptions('Addnoise',true));