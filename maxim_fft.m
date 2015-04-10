%Example program routine to generate FFT plots and determine the dynamic performance of
%a high-speed dataconverter from the data records taken with a HP16500C Logic Analyzer
%System. Data was extracted through the HPIB interface and read into the following MATLAB
%program routine. The same data can be extracted from the controller interface of the LA
%and simply be copied to a floppy disk??a rather time-consuming way, but possible.
%m = load('output.mat');      % load the simulink data

nfft=8192;
numpt=nfft;       %('Data Record Size (Number of Points)? ');
fclk=fs;       %('Sampling Frequency (MHz)? '); 
numbit=numb; 
%Discard first 13 lines from the data file, which do not contain data
% n = fieldnames(m);      % data to v1
% v1 = m.(n{1});
% v1=v1'; 
% code=v1(:,2); 
code=simout1;
code=code';
%Display a warning, when the input generates a code greater than full-scale
if (max(code)==2^numbit-1) || (min(code)==0)
disp('Warning: ADC may be clipping!!!'); 
end 
Dout=code-(2^numbit-1)/2;       %Recenter the digital sine wave

%If no window function is used, the input tone must be chosen to be unique and with 
%regard to the sampling frequency. To achieve this prime numbers are introduced and the
%input tone is determined by fIN = fSAMPLE * (Prime Number / Data Record Size).
%Doutw=Dout;
%Doutw=Dout.*hamming(length(Dout));
Doutw=Dout.*(hanning(length(Dout)))';      % window
Dout_spect=fft(Doutw);      %Performing the Fast Fourier Transform 
Dout_dB=20*log10(abs(Dout_spect)); 
maxdB=max(Dout_dB(1:numpt/2));
%For TTIMD, use the following short routine, normalized to ??6.5dB full-scale.
%plot([0:numpt/2-1].*fclk/numpt,Dout_dB(1:numpt/2)-maxdB-6.5); 

%Calculate SNR, SINAD, THD and SFDR values
fin=find(Dout_dB(1:numpt/2)==maxdB);    %Find the signal bin number, DC = bin 1
span=max(round(numpt/200),5);   %Span of the input frequency on each side
spanh=2;        %Approximate search span for harmonics on each side
spectP=(abs(Dout_spect)).*(abs(Dout_spect));    %Determine power spectrum
Pdc=sum(spectP(1:span));    %Find DC offset power 
Ps=sum(spectP(fin-span:fin+span));  %Extract overall signal power 
Fh=[];      %Vector/matrix to store both frequency and power of signal and harmonics
Ph=[];      %The 1st element in the vector/matrix represents the signal, the next element represents
%the 2nd harmonic, etc.

%Find harmonic frequencies and power components in the FFT spectrum 
for har_num=1:10
%Input tones greater than fSAMPLE are aliased back into the spectrum
tone=rem((har_num*(fin-1)+1)/numpt,1); 
    if tone>0.5     %Input tones greater than 0.5*fSAMPLE (after aliasing) are reflected
    tone=1-tone;
    end 
Fh=[Fh tone]; 
%For this procedure to work, ensure the folded back high order harmonics do not overlap 
%with DC or signal or lower order harmonics 
har_peak=max(spectP(round(tone*numpt)-spanh:round(tone*numpt)+spanh)); 
har_bin=find(spectP(round(tone*numpt)-spanh:round(tone*numpt)+spanh)==har_peak);
har_bin=har_bin+round(tone*numpt)-spanh-1;
Ph=[Ph sum(spectP(har_bin-1:har_bin+1))]; 
end 
%Determine the total distortion power 
Pd=sum(Ph(2:5)); 
%Determine the noise power 
Pn=sum(spectP(1:numpt/2))-Pdc-Ps-Pd;
format;
A=(max(code)-min(code))/2^numbit; 
AdB=20*log10(A);
SNR=10*log10(Ps/Pn);
SINAD=10*log10(Ps/(Pn+Pd));
ENOB=(SINAD-1.76)/6.02; 
THD=10*log10(Pd/Ph(1));
SFDR=10*log10(Ph(1)/max(Ph(2:10))); 
HD=10*log10(Ph(1:10)/Ph(1));

figure;
plot((0:numpt/2-1).*fclk/numpt,Dout_dB(1:numpt/2)-maxdB);       % plot result
grid on; 
title(['SNR= ',num2str(SNR),'dB, SNDR= ',num2str(SINAD),'dB, SFDR= ',num2str(SFDR),'dBc, THD= ',num2str(THD),'dB']);
legend(sprintf('%5g point FFT',nfft));
xlabel('ANALOG INPUT FREQUENCY (MHz)');
ylabel('AMPLITUDE (dBFS)');
axis1=axis;
axis([axis1(1) axis1(2) -140 axis1(4)]);
%legend();
%Distinguish all harmonics locations within the FFT plot
% hold on; 
% plot(Fh(2)*fclk,0,'mo',Fh(3)*fclk,0,'cx',Fh(4)*fclk,0,'r+',Fh(5)*fclk,0,'g*',...
%     Fh(6)*fclk,0,'bs',Fh(7)*fclk,0,'bd',Fh(8)*fclk,0,'kv',Fh(9)*fclk,0,'y^');
% %legend('1st','2nd','3rd','4th','5th','6th','7th','8th','9th');
% hold off;
