% fldur
%Calculates the flow duration from the time series of daily discharges
load('MRHq.txt')
dmq=MRHq(:,4);
mth=MRHq(:,1);
day=MRHq(:,2);
yr=MRHq(:,3);
QMA = mean(dmq)
sm2lq = sort(dmq);
numq = length(dmq);
m = 1;
for n = numq:-1:1;
    qsrt(m) = sm2lq(n);
    m = m + 1;
end
for m = 1:100;
    test = qsrt(m);
end
pbs=[0.01 0.03 0.05 0.1 0.5 1.0 2 3 5 10 15 20 30 40 50 60 70 80 85 90 ...
    95 97 98 99 99.5 99.9 99.95 99.97 99.99];
for m = 1:numq;
    prbq(m) = 100*(m/(numq + 1));
end
for l = 1:29;
    for m = 1:numq;
        if (prbq(m) <= pbs(l))&& (prbq(m+1)>= pbs(l));
            qfd(l) = qsrt(m+1)+(qsrt(m)-qsrt(m+1))*((pbs(l)-prbq(m+1))/...
                (prbq(m)-prbq(m+1)));
            dqfd(l) = qfd(l)/QMA;
        end
    end
end
    fid1 = fopen('MRHq_fld','w');
    for l = 1:29
    fprintf(fid1,'   %5.2f    %8.2f   %8.3f\n'...
        ,pbs(l), qfd(l),   dqfd(l));
    end
fclose(fid1)
   
    