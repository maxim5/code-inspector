%% data set loading script.
% chooses the data set and sets some optional parameters
% add your data sets and data set specific parameters here
% dont for get to put ,... at the end of settings lines
switch dataset
    case 'fourtube' % an real example dataset to see if the code is working.
        mr_data='four_tube_phantom_lowg/data/pinecone_01';
        settings={
        'freq_off',35,... 
        'whichfid',1:4,...
        'caldata','circ_lowg_calibration_09jan2007/circ_lowg_calibration.mat'
        };
    case 'fourtube_se' % se test data set
        mr_data='four_tube_phantom_lowg/data/pinecone_03';
        settings={
        'freq_off',55 ,...
        'caldata','circ_lowg_calibration_09jan2007/circ_se_lowg_calibration.mat',...
        'parse_mode','se',...
        'echo_time',8300
        };
    case 'fatsat' % test data for fat saturation block
        mr_data='kiwi_pinecone_fatsat_fix_s_20070118_02/data/pinecone_07';
        settings={
        'freq_off',35,...
        'whichfid',1,...
        'caldata','circ_lowg_calibration_09jan2007/circ_fs_lowg_calibration.mat'
        };
    case 'kiwi_20sec_block' % test data for functional analysis, 20s AB block design
        mr_data='kiwi_20sec_block/data/pinecone_01';
        settings={
        'whichfid',1:30,... % this will average them together not solve individually
        'caldata','circ_lowg_calibration_09jan2007/circ_lowg_calibration.mat'
        };
    case 'kiwi_higher_slices' % test data for slice offset and nicer freq map
        mr_data='kiwi_higher_slices/data/pinecone_03';
        settings={
        'freq_off',82,...
        'whichfid',1:4,...
        'caldata','circ_lowg_calibration_09jan2007/circ_lowg_calibration.mat'
        };
    otherwise
        error('Cannot find that dataset. Look at the dataset load script.')
end

%% parameter notes:
% all of the settings should have reasonable defaults in se_opts.m
% mr_data: should give the location of the directory containing the data to
% process
% settings: is a cell array containing the optional settings
% echo time: set thiss to whatever the real echo time is :) Inf for 'fid'
% data
% freq_off: uwinds the signal try 10s of hertz in 5 Hz steps
% which_fid: pick the signal you want. averaging does not help a lot
% because there isnt a lot of noise?
% caldata: choose the k-trajectory that matches the one you used to collect
% the data.
% parse_mode: 'fid' for data without a spin echo. 'se' for the first spin
% echo without the fid part.
