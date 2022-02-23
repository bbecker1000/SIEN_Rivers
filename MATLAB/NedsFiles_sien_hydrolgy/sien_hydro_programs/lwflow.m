% lwflow
%Calculates the low flow statistics from the time series of daily values,
load('MRHq.txt')
dmq=MRHq(:,4);
mth=MRHq(:,1);
day=MRHq(:,2);
yr=MRHq(:,3);
%recdyr = input('Years of Record');
recdyr = 94;
    w3df = [];
    w7df = [];
    w14df = [];
    s3df = [];
    s7df = [];
    s14df = [];
mindys = 1;
maxdys = 365;
tma = mindys + 61;
tmb = mindys + 151;
tmc = tmb;
tmy = maxdys - 92;
tmz = maxdys;
for n = 1:recdyr;
        for m = tma:tmc;
            if(mth(m)== 2)&& (day(m)== 29);
                maxdys = maxdys + 1;
                tmb = tmb + 1;
            end
        end
        wyr(n) = yr(maxdys);
        dq = 0.0;
        for m = mindys:maxdys;
            dq = dq + dmq(m);
        end
        amQ(n) = dq/((maxdys-mindys) + 1);
        w3df(n) = 1000;
        w7df(n) = 1000;
        w14df(n) = 1000;
        s3df(n) = 1000;
        s7df(n) = 1000;
        s14df(n) = 1000;
        for m = tma:tmb;
            wq3d = (dmq(m)+dmq(m-1)+dmq(m-2))/3;
            wq7d = (dmq(m)+dmq(m-1)+dmq(m-2)+dmq(m-3)+dmq(m-4)+dmq(m-5)...
                +dmq(m-6))/7;
            wq14d = (dmq(m)+dmq(m-1)+dmq(m-2)+dmq(m-3)+dmq(m-4)+dmq(m-5)...
                +dmq(m-6)+dmq(m-7)+dmq(m-8)+dmq(m-9)+dmq(m-10)+dmq(m-11)...
                +dmq(m-12)+dmq(m-13))/14;
            if wq3d < w3df(n);
                w3df(n) = wq3d;
                mw3df(n) = m;
            end
            if wq7d < w7df(n);
                w7df(n) = wq7d;
                mw7df(n) = m - mindys;
            end
            if wq14d < w14df(n);
                w14df(n) = wq14d;
                mw14df(n) = m;
            end
        end
        for m = tmy:tmz;
            sq3d = (dmq(m)+ dmq(m-1)+ dmq(m-2))/3;
            sq7d = (dmq(m)+ dmq(m-1) + dmq(m-2) + dmq(m-3) + dmq(m-4)...
                +dmq(m-5)+ dmq(m-6))/7;
            sq14d = (dmq(m)+dmq(m-1)+dmq(m-2)+dmq(m-3)+dmq(m-4)+dmq(m-5)...
                +dmq(m-6)+dmq(m-7)+dmq(m-8)+dmq(m-9)+dmq(m-10)+dmq(m-11)...
                +dmq(m-12)+dmq(m-13))/14;
            if sq3d < s3df(n);
                s3df(n) = sq3d;
                ms3df(n) = m;
            end
            if sq7d < s7df(n);
                s7df(n) = sq7d;
                ms7df(n) = m - mindys;
            end
            if sq14d < s14df(n);
                s14df(n) = sq14d;
                ms14df(n) = m;
            end
        end
        mindys = maxdys + 1;
        maxdys = maxdys + 365;
        tma = mindys + 61;
        tmb = mindys + 151;
        tmc = tmb;
        tmy = maxdys - 92;
        tmz = maxdys;
end
for n = 1:recdyr;
    w3df(n);  
    mw3df(n);
    w7df(n);
    mw7df(n);
end
        w3dx = sort(w3df);
        w7dx = sort(w7df);
        w14dx = sort(w14df);
        s3dx = sort(s3df);
        s7dx = sort(s7df);
        s14dx = sort(s14df);
        for n = 1:recdyr;
            exdP(n) = n/(recdyr + 1);
        end
        xxp = [0.98, 0.95, 0.90, 0.80, 0.70, 0.6, 0.50, 0.3333, 0.20,...
            0.1429, 0.10, 0.0667, 0.05, 0.040, 0.0333, 0.02857, 0.025,...
            0.02222, 0.020];
        for p = 2:19;
            ri(p) = 1/xxp(p);
            for n = 2:recdyr;
                if exdP(n-1) < xxp(p) && exdP(n) >= xxp(p);
                    Px = ((xxp(p)-exdP(n))/(exdP(n-1)-exdP(n)));
                    w7D(p) = w7dx(n) + Px*(w7dx(n-1)-w7dx(n));
                    w3D(p) = w3dx(n) + Px*(w3dx(n-1)-w3dx(n));
                    w14D(p) = w14dx(n) + Px*(w14dx(n-1)-w14dx(n));
                    s3D(p) = s3dx(n) + Px*(s3dx(n-1)-s3dx(n));
                    s7D(p) = s7dx(n) + Px*(s7dx(n-1)-s7dx(n));
                    s14D(p) = s14dx(n) + Px*(s14dx(n-1)-s14dx(n));
                end
            end
        end
mean(amQ)
fid1 = fopen('mrhlwf1D','w');
fid2 = fopen('mrhlwf2D','w');
fid3 = fopen('mrhlwf3DX','w');
fid4 = fopen('mrhMAQ','w');
for n = 1:recdyr;
    fprintf(fid1,'%7.f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.f %7.f\n'...
            ,wyr(n), w3df(n),w7df(n),w14df(n),s3df(n),s7df(n),s14df(n),...
            mw7df(n),ms7df(n));
        fprintf(fid4,'   %7.f     %10.2f\n',wyr(n),amQ(n));
end
for n = 1:recdyr;
    fprintf(fid2,' %7.3f  %7.3f  %7.3f  %7.3f  %7.3f  %7.3f   %7.3f\n'...
        ,exdP(n),w3dx(n),w7dx(n),w14dx(n),s3dx(n),s7dx(n),s14dx(n));
end
for p = 2:19;
    fprintf(fid3,' %7.3f  %7.3f   %7.3f  %7.3f  %7.3f  %7.3f  %7.3f\n'...
        ,xxp(p),w3D(p),w7D(p),w14D(p),s3D(p),s7D(p),s14D(p));
    p = p + 1;
end
fclose(fid1);
fclose(fid2);
fclose(fid3);
fclose(fid4);
    
   
    