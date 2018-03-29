stream = 1;         % select stream/request mode
plotit = 1;           % turn on plotting

host = 'flyscan:3333';
xnam = 'RmapSource/c0';
%xnam = 'shortcuttofly3/RmapSource/c0';

snk = rbnb_sink(host,'mySink');
snk.GetChannelList('...')

if(stream) rbnb_monitor(snk, xnam, 1);
end
srate = 0;
mintim = 0;
tic

for i=1:1000
    if(stream) 
        xget = rbnb_next(snk, 10000);
    else
        xget = rbnb_request(snk, xnam, -.1, .1, 'newest');
    end
    
    dt = max(.001,toc);     % avoid divby0
    tic
    srate = (srate + 1/dt)/2;
    
    if(mintim == 0) mintim = min(xget.time);
    end
    
    if(plotit) 
        plot(xget.time-min(xget.time),xget.data);
        title(sprintf('I: %d, FrameRate: %g, Time: %g', i, srate, min(xget.time)-mintim));
        drawnow;
    else
        i
        srate
    end
    
    pause(.01)
end

snk.CloseRBNBConnection
  