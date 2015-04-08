function [dacout,Cmaindac_bin,Csubdac_bin]=fbindac14(N,M,sigmacap);
%general N bit DAC with M bit main-dac; N-M bit sub-dac.
%Last 2 LSB bits in sub-dac are realized as 0.5C and 0.25C

total_units=2^M-1+2^(N-M-2)-1+2+2; %MSB dac=2^M-1,LSB dac=2^M-1,2 LSB bits, Term cap, coupling cap
caps=(1+randn(1,total_units)*sigmacap);
%caps=ones(1,total_units);
Cp1=0; %parasitic on main dac side of coupling cap
Cp2=0; %parasitic on sub dac side of coupling cap

for i=1:M
   Cmaindac_bin(M-(i-1))=sum(caps(2^(i-1):2^i-1));  
end
 
Cmdtotal=sum(Cmaindac_bin(1:M))+Cp1; %sum of all caps in Main Dac
disp(Cmaindac_bin);
for i=1:N-M-2
   Csubdac_bin(N-M-2-(i-1))=sum(caps(2^(i-1)+2^M-1:2^i-1+2^M-1));  
end

%Calculation of last 2 bits in sub-dac which are binary and realized as
%series combination of unit cap
Csubdac_bin(N-M-1)  = 0.5*caps(2^M-1+2^(N-M-2));
Csubdac_bin(N-M)= 0.25*caps(2^M+2^(N-M-2));
Csdterm= 0.25*caps(2^M+2^(N-M-2)+1); %termination cap always connected to ground
Csdtotal=sum(Csubdac_bin(1:N-M))+Csdterm+Cp2; % sum of all caps in Sub-dac
Cc=Csdtotal*caps(2^M+2^(N-M-2)+2)/(Csdtotal-1);

disp(Csubdac_bin);
%Cc=32/31;
for maindacindex=1:2^(M)
  %%%% Binary Main DAC
    mdi_bin=dec2bin(maindacindex-1,M);
    Cmref=sum(mdi_bin.*Cmaindac_bin)-sum(48*Cmaindac_bin);
    for subdacindex=1:2^(N-M)
    sdi_bin=dec2bin(subdacindex-1,N-M);
    Csref=sum(sdi_bin.*Csubdac_bin)-sum(48*Csubdac_bin);
    dacout(2^(N-M)*(maindacindex-1)+subdacindex)=(Cmref*(Csdtotal+Cc)+Csref*Cc)*2^N/(Cc*Cmdtotal+Csdtotal*(Cc+Cmdtotal));
    end
end
end
