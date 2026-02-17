function getSongSelectionTables_measurefrombegintime()
fftSize=2048
%process Raven selection tables
%will ask for directory containg files, assumes that selection table filenames
%have same file names as wav files but with 'Table.1.selections.txt' instead of '.wav'
%%
nFFTsToClip = 1; % how many fftsize samples to clip either side of 5% and 95% energy duration points

fileFolder = uigetdir(); %prompt user to choose directory
wavFiles = dir([fileFolder filesep '*.wav']); %get list of all wav files
txtFiles = dir([fileFolder filesep '*.txt']); %get list of all txt files

for f = 1:numel(wavFiles) %loop through wav files
    fName = wavFiles(f).name;
    disp(['working on ' fName(1:end-4)]);
    fMatch = find(contains({txtFiles.name},fName(1:end-4))); %look for matching selection table file
    if isempty(fMatch) %check only one matching file found...
        disp('No selection table found for this file - skipping... ')
    elseif numel(fMatch)>1
        disp('Multiple matching selection tables found - check filenames - skipping')
    else
        x = readtable([fileFolder filesep txtFiles(fMatch).name],'delimiter','\t'); %if file found, load selection table
        x = sortrows(x);
        x.StartFreqHz = zeros(height(x),1); %add empty columns for start and end frequencies
        x.EndFreqHz = zeros(height(x),1);
        info = audioinfo([fileFolder filesep wavFiles(f).name]); %get sample rate
        fs = info.SampleRate;
        %%
        for u = 1:height(x) %loop through all selected units
            disp(['Unit ' num2str(u) ' of ' num2str(height(x)) '; Selection #: ' num2str(x.Selection(u))])
            startSample = round(fs*x.BeginTime_s_(u)); %get times of selection start
            startSampleend = round(fs*x.Time5__s_(u)); %get times of selection start
            if (startSampleend-startSample)<fftSize
                startSampleend = startSample+fftSize;
            end
            startClipRaw = audioread([fileFolder filesep wavFiles(f).name],[startSample startSampleend]); % read relevant clip from file
            startClipRaw = startClipRaw(:,1);
            %filter here
            if x.LowFreq_Hz_(u)<0.1
                LF = x.LowFreq_Hz_(u)+5
            else
                    LF = x.LowFreq_Hz_(u)
                    end
            [x.LowFreq_Hz_(u),x.HighFreq_Hz_(u)]
            [b1,a1] = butter(4,[LF,x.HighFreq_Hz_(u)]/(fs/2)) ;
           startClip = filter(b1,a1,startClipRaw);
           
   %how to then apply the BandPassClip1 to the measurement code?
            [startSample startSampleend startSample-startSampleend]
            [SL,freqs] = spectrum_level(startClip,fftSize,fs); %get power spectrum
            fStart = freqs(SL==max(SL));
            endSample = round(fs*x.Time95__s_(u)); %same with end
            endSampleend = round(fs*x.EndTime_s_(u)); %get times of selection end
            if (endSampleend-endSample)<fftSize
                endSample = endSampleend-fftSize;
            end
            endClipRaw = audioread([fileFolder filesep wavFiles(f).name],[endSample endSampleend]); % read relevant clip from file
            endClipRaw = endClipRaw(:,1);
            %Filter here
           
           endClip = filter(b1,a1,endClipRaw);
          
            [SL,freqs] = spectrum_level(endClip,fftSize,fs); %get power spectrum
            fEnd = freqs(SL==max(SL));
            max(SL)
            disp(['StartFreq = ' num2str(fStart) 'Hz; EndFreq = ' num2str(fEnd) 'Hz'])
            x.StartFreqHz(u) = fStart;           
            x.EndFreqHz(u) = fEnd;
        end
    end
    writetable(x,[fileFolder filesep fName(1:end-4) '.csv'])
end