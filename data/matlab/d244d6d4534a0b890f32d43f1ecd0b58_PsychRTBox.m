function varargout = PsychRTBox(varargin)
% Driver for the USTC reaction time button box (RTBox) by Xiangrui Li et al.
% varargout = PsychRTBox(cmd, varargin);
%
% This driver allows to control most functions of the USTC RTBox response
% button box. In theory this driver should support boxes up to Box/Firmware
% version 5. In practice it has only been tested by the Psychtoolbox
% developer up to firmware and box version 1.3, therefore you may encounter
% bugs for later versions. The box itself comes bundled with an alternative
% driver called "RTBox" which is maintained by the developers of the box
% hardware itself, in case this driver doesn't work with your box or
% firmware.
%
% The RTBox is a USB device which provides 4 response buttons (pushbuttons)
% for subject responses and can report any button press- or release by the
% subject. Additionally it has an input for reporting of external
% electronic trigger signals and a photo-diode input for reporting of
% visual stimulus onset. The box uses a built-in high-resolution clock to
% timestamp all button- or trigger events, independent of the host
% computers clock in order to make it more reliable for response time
% measurements than most other response devices. It also buffers all events
% internally, so experiment scripts can read back events when it is most
% convenient. Timestamps can be either reported in Psychtoolbox standard
% GetSecs timebase for direct comparison with timestamps from GetSecs,
% WaitSecs, KbCheck et al., Screen('Flip') and PsychPortAudio, etc. This
% simplifies reaction time calculations. Timestamps can also be reported in
% the timebase of the boxe, e.g., time of a button press relative to the
% photo-diode light trigger signal or electronic trigger signal, if this is
% more convenient for a given experiment setup.
%
% Current versions of the RTBox have additional functionality, e.g., more
% digital trigger inputs, some sound trigger, and TTL trigger outputs.
%
% See http://lobes.usc.edu/RTbox for up to date product information and
% additional driver software.
%
% Please note that while the device documentation claims that the external
% electronic pulse port is able to receive TTL trigger signals, we couldn't
% verify that this is the case with our test sample of version 1 of the
% RTbox hardware. While the pulse input port responded to some pulses sent
% by one TTL compatible device, it failed to detect the majority of signals
% from TTL most other test devices.
%
% This indicates that the pulse port may not be fully TTL compliant and
% will need additional tinkering for your setup. However, results with
% later versions of the hardware may be different than our experience with
% our test sample.
% 
%
% The following subcommands are currently suppported:
% ===================================================
%
%
% handle = PsychRTBox('Open' [, deviceID] [, skipSync=0]);
% -- Try to open a connected RTBox, return a device handle 'handle' to it
% on success. The handle can be used in all further subcommands to refer to
% the box. By default, all USB ports (or rather USB-Serial ports) are scanned
% for a connected RTBox and the driver will connect to the first box found.
% Alternatively you can specify which box to use via the optional
% 'deviceID' namestring. This can be either the name of a box, or the name
% of the USB-Serial port to which the box is connected. This way you can avoid
% scanning of all ports and disambiguate in case multiple boxes are
% connected to your computer.
%
% Btw., if you only make use of one single RTBox, you don't need to specify
% the 'handle' parameter to all following subfunctions. Instead you can
% specify that parameter as [] or omit it and the driver will use the only
% open box connected.
%
% The optional parameter 'skipSync', if set to 1, will prevent the open
% routine from performing an initial clock synchronization. By default, it
% will perform an initial clock synchronization.
%
% After opening the box, you may want to invoke this method:
%
%
% clockRatio = PsychRTBox('ClockRatio' [, handle] [, durationSecs]);
% -- Perform a clock drift calibration between the computers GetSecs host
% clock and the internal clock of the box 'handle'. Restrict calibration to
% a maximum of 'durationSecs' (default 60 seconds if omitted). Return the
% computed 'clockRatio' and use it for all further operations.
%
% Due to manufacturing imperfections and environmental factors, no two
% clocks ever run at exactly the same speed. Therefore the computer clock
% and box clock will slowly "drift out of sync" under normal conditions,
% rendering retrieved event timestamps inaccurate over the course of a long
% experiment session. This calibration routine will exercise the clocks and
% compute the clock drift due to this speed difference, then use the
% computed drift (= clockRatio) to correct all reported timestamps for this
% drift, thereby providing the accuracy needed for reaction time studies.
%
% The clockRatio value tells, how many seconds of GetSecs time elapse when
% the box clock measures 1 second elapsed time. Ideally this value would be
% 1, ie. both clocks run at the same speed. A more realistic value would be,
% e.g., 1.000009 -- The computer clock goes 9 microseconds faster than the
% box clock, so the drift will accumulate an error of 9 microseconds for
% each elapsed second of your study.
%
% As every calibration, this routine involves some measurement/calibration
% error and is therefore not perfect, so even after a successfull
% 'ClockRatio' calibration, timestamps reported during your experiment will
% accumulate some error during the course of long experiment sessions.
%
% There are multiple ways to handle this:
%
% a) Use a long calibration time in this function for accurate results, and
% a reasonably short experiment duration.
%
% b) Repeat this procedure after every large block of trials, ie., every
% couple of minutes, e.g., while the subject is allowed to take a break in
% a long experiment session.
%
% c) Use the PsychRTBox('SyncClocks') function after each short block of
% trials, or even after each trial, for the highest accuracy.
%
% d) Don't care for clock drift throughout the experiment session, just
% collect the event timestamps in box clock format (see the 3rd return
% argument of PsychRTBox('GetSecs') or the returned timing array of
% PsychRTBox('BoxSecs');) and store them in some array. Remap all timestamps
% into the computers GetSecs time at the end of your session via
% PsychRTBox('BoxsecsToGetsecs'). This requires a bit more
% discipline from you in programming and organizing your data, but it
% provides the most accurate timestamps.
%
%
% [syncResult, clockRatio] = PsychRTBox('SyncClocks' [, handle]);
% -- Synchronize or resynchronize the clocks of the host computer and the
% box. Return result in 'syncResult' and the current clockRatio in
% 'clockRatio'. This routine is automatically carried out during invocation
% of PsychRTBox('ClockRatio'); but you can repeat the sync procedure
% anytime between trials via this subfunction for extra accuracy at the
% expense of about 0.5 - 1 second additional time for each invocation. You
% would typically execute this function at the start of each large block of
% trials, or before start of each trial if you are really picky about
% super-exact timing. The syncResult contains three values:
%
% syncResult(1) = Host time (GetSecs time) at time of clock sync.
%
% syncResult(2) = Box time at time of clock sync.
%
% syncResult(3) = Confidence interval for the accuracy of the sync. This
% value (in seconds) provides a reliable upper bound for the possible error
% introduced in all reported timestamps from the box. The real error may be
% significantly smaller, this is just an upper bound that you can check.
% Typical results on a well working system should be in the sub-millisecond
% range, e.g., 0.0003 seconds or 0.3 msecs. Typical results on a rather
% noisy system would be around 0.001 second or 1 msec. Results worse than 2
% msecs indicate some problem with your system setup that should be fixed
% before executing any experiment study which involves reaction time
% measurements. By default, the sync procedure will abort with an error if
% it can't calibrate to an accuracy with a maximum error of 1.3 msecs within
% a duration of 0.5 seconds. You can change these default constraints with
% a call to PsychRTBox('SyncConstraints').
%
%
% [oldmaxDurationSecs, oldgoodEnoughSecs, oldrequiredSecs, oldsyncMethod] = PsychRTBox('SyncConstraints'[, maxDurationSecs][, goodEnoughSecs][, requiredSecs][, syncMethod]);
% -- Change the constraints to apply during calls to PsychRTBox('SyncClocks');
% Optionally return old settings.
%
% 'maxDurationSecs' limits any call to 'SyncClocks' to a duration of at
% most the given number of seconds. Calibration aborts after at most that
% time, even if unsuccessfull - in that case with an error message. By
% default, the duration is limited to 0.5 seconds.
% 'goodEnoughSecs' Calibration will finish before 'maxDurationSecs' have
% elapsed, if the result is more accurate than an error of at most
% 'goodEnoughSecs'. By default, this is set to zero seconds, i.e.,
% calibration will always take 'maxDurationSecs'.
% 'requiredSecs' - The calibration will only use samples with an
% uncertainty of at most 'requiredSecs'. If not even a single sample of the
% required precision can be acquired within 'maxDurationSecs', the call
% will fail with an error, indicating that your system setup doesn't
% provide the required timing precision for your demands. By default, the
% minimum required precision is 0.0013 seconds, ie., it will tolerate an
% error of at most 1.3 msecs.
% 'syncMethod' - Select the synchronization method to use. Either, method 0
% (prewrite sync), or method 1 (postwrite sync), or method 2 (average). If
% you want to know the difference between the methods, please consult the
% source code of this file and read the code for the subroutine 'function
% syncClocks'. All three methods are robust and accurate within the
% returned confidence window, usually better than 1 msec. So far, method 1
% seems to get the best results on our test setups, so this is the default
% for the current driver release. However, we are still evaluating if
% method 0 would be a tiny bit better and worth switching the default to
% that.
%
%
% oldverbose = PsychRTBox('Verbosity' [, handle], verbosity);
% -- Set level of verbosity for driver: 0 = Shut up. 1 = Report errors
% only. 2 = Report warnings as well. 3 = Report additional status info. 4 =
% Be very verbose about what is going on. The default setting is 3 --
% Report moderate status output.
%
%
% devinfo = PsychRTBox('BoxInfo' [, handle] [, newdevinfo]);
% -- Return a struct 'devinfo' with all information about the current
% status and parameter settings for RTBox 'handle'. Optionally set a new
% struct with updated parameters via 'newdevinfo'. This function is mostly
% useful for debugging and benchmarking the driver itself. Most information
% contained in 'devinfo' will be useless for your purpose.
%
%
% PsychRTBox('Close', handle);
% -- Close connection to specific box 'handle'. Release all associated
% ressources.
%
%
% PsychRTBox('CloseAll');
% -- Close connections to all attached RTBox devices. Reset the PsychRTBox
% driver completely. You'll usually use this function at the end of your
% experiment script to clean up.
%
%
% oldeventspec = PsychRTBox('Enable' [,handle][, eventspec]);
% -- Enable specified type of event 'eventspec' on box 'handle'. This
% allows to enable detection and reporting of a specific type of event. By
% default, only reporting of push-button press is enabled, as this is the
% most common use of a response box.
%
% The following names are valid for the name string 'eventspec':
% 'press' = Report push-button press. This is the default setting.
% 'release' = Report push-button release.
% 'pulse' = Report electronic trigger events on external input port.
% 'light' = Report reception of light flashes by photo-diode on light port.
% 'tr' = Report reception of scanner trigger "TR" (TTL input from pin 7 of DB-9 port).
% 'all' = Enable all events.
%
% If called without the 'eventspec' parameter, the function will return the
% names of all currently enabled events.
%
%
% oldeventspec = PsychRTBox('Disable' [,handle][, eventspec]);
% -- Disable specified type of event 'eventspec' on box 'handle'. This
% allows to disable detection and reporting of a specific type of event. By
% default, only reporting of push-button press is enabled, as this is the
% most common use of a response box.
%
% See 'Enable' call for help on parameters.
%
%
% Once you have setup and calibrated the box and selected the type of
% events to detect and report, you will want to actually retrieve
% information about events. For this you use these commands:
%
%
% PsychRTBox('Start' [, handle] [, dontwaitforstart=0]);
% -- Start event detection and reporting by the box. The box will start
% detecting button and trigger events from here on and record them in the
% event buffer.
% 
% You will usually call this at the beginning of a response period. By
% default, the box has reporting already enabled after 'Open'ing it.
%
% The optional 'dontwaitforstart' parameter, if set to 1, will ask the
% 'Start' function to return control as soon as possible, ie., without
% waiting for confirmation of the box that event reporting has actually
% started. By default, the routine waits for an acknowledgement from the
% box, which can take 16 - 30 msecs in some cases.
%
%
% PsychRTBox('Stop' [, handle]);
% -- Stop event detection and reporting by the box. The box will ignore
% detecting button and trigger events from here on and no longerrecord them
% in the event buffer.
% 
% You will usually call this at the end of a response period.
%
%
% PsychRTBox('Clear' [, handle] [, syncClocks=0] [, dontRestart=0]);
% -- Stop event detection and reporting by the box, clear all recorded
% events so far, then restart reporting if it was active before calling
% this function.
% 
% Instead of calling 'Start' and 'Stop' to mark the start and end of a
% response period in a trial you can also simply use this function at the
% beginning of a trial (or its response period) to discard any stale data
% from a previous trial (or non-response interval).
%
% You can prevent an automatic restart of event reporting by setting the
% optional flag 'dontRestart' to a value of 1.
%
% You can ask the box to resynchronize its clock to the host computer clock
% by setting the optional flag 'syncClocks' to a value of 1. This is the
% same as calling PsychRTBox('SyncClocks').
%
%
% [time, event, boxtime] = PsychRTBox('GetSecs' [, handle] [, interTimeout=0.1] [, maxTimeout=interTimeout] [, maxItems=inf]);
% -- Retrieve recorded events from the box 'handle'.
%
% By default, as many events are returned as are available within the
% test interval, but you can select a specific number of wanted events
% by setting the optional parameter 'maxItems'. If there aren't any pending
% events from the box, by default the driver waits for up to 0.1 seconds
% for events to arrive. You can change this 'interTimeout' interval via the
% positive (non-zero) 'interTimeout' parameter. The function will return if
% no new events show up within 'interTimeout' seconds. If something shows
% up, the deadline for return is extended by 'interTimeout' seconds. You
% can set an absolute upper limit to the response interval via the
% 'maxTimeout' parameter. That defaults to 'interTimeout' if omitted.
% Please note that after an event is detected by the box, up to 16-32 msecs
% can elapse until the event is received by the computer, so you may not
% want to set these timeout values too small!
%
% The function will return an array of timestamps in 'time', and an array
% of corresponding names of the events in 'event'. E.g., event(1) will
% report the identity of the first detected event, e.g., '1' if button 1
% was pressed, whereas time(1) will tell you when the event happened, ie.,
% when button 1 was pressed. 'time' is expressed in host clock time, aka
% GetSecs() time. If no events are pending since last invocation of this
% function, empty vectors will be returned.
%
% Additionally, the vector 'boxtime' contains the same timestamp, but
% expressed in box clock time. See below for a use of that.
%
% By default, the following names are possible for 'event's:
%
% '1' = 1st button pressed, '1up' = 1st button released.
% '2' = 2nd button pressed, '2up' = 2nd button released.
% '3' = 3rd button pressed, '3up' = 3rd button released.
% '4' = 4th button pressed, '4up' = 4th button released.
% 'pulse' = electronic pulse received on electronic pulse input port.
% 'light' = Light pulse received by photo-diode connected to light input port.
% 'tr' = Scanner trigger "TR" (TTL input from pin 7 of DB-9 port) received.
% 'serial' = PsychRTBox('Trigger') Softwaretrigger signal received on USB-Serial port.
%
% Note: 'tr' is only supported on boxes with Firmware version 3.0 or later.
%
% However, you can assign arbitrary names to the buttons and events if you
% don't like this nomenclature via the PsychRTBox('ButtonNames') command.
%
% The reported timestamps are expressed in host clock time, ie., in the
% same units as the timestamps returned by GetSecs, Screen('Flip'),
% PsychPortAudio, KbCheck, KbWait, etc., so you can directly calculate
% reaction times to auditory stimuli, visual stimuli and other events.
%
% See the help for PsychRTBox('SyncClocks') and PsychRTBox('ClockRatio')
% for the accuracy of these timestamps and tips for obtaining optimal
% accuracy.
%
% Additionally the event times are also returned in 'boxtime', but this
% time expressed in box time -- the time of the box internal clock.
% 
%
% There are multiple variants of this query command with the same optional
% input arguments, but different return arguments. All of these return
% timestamps in box time without remapping to GetSecs time by calling:
%
% [boxtime, event] = PsychRTBox('BoxSecs' ...);
% -- Timestamps are in raw box clock time, everything else is the same as
% in PsychRTBox('GetSecs' ...).
%
% If you have the 'boxtime' timestamps from one of the previous functions
% around, you can map them later to GetSecs time with very high precision
% at the end of your experiment session via:
%
% [GetSecs, Stddev] = PsychRTBox('BoxsecsToGetsecs' [, handle], boxTimes);
% -- Perform a post-hoc mapping of a vector of raw box timestamps
% 'boxTimes' into a vector of host clock 'GetSecs' timestamps. Return some
% error measure in 'Stddev' as well, if available.
%
% This method can be used to convert event timestamps expressed in the box
% clocks timebase into timestamps in Psychtoolbox GetSecs host clock
% timebase. It has the advantage of providing the highest possible accuracy
% in mapping, because it computes an optimal mapping function for this
% purpose, which is based on all the timing information collected
% throughout a whole experiment session. The disadvantage is that it will
% only provide meaningful results if you call it at the end of your
% experiment session, so you'll need to manage all your collected
% timestamps in a format that is suitable as input to this function.
%
%
% Timestamps can also be returned relative to a specific trigger event: You
% specify which event acts as a trigger. Then all timestamps of all events
% are expressed relative to the time of that trigger event, i.e., as
% deltas. Any event can be the trigger. Format of all arguments is
% as in PsychRTBox('BoxSecs' ...);
%
% E.g., PsychRTBox('serial', ...); Returns timestamps relative to the first
% occurence of a electronic input port trigger signal since the last query.
% PsychRTBox('light', ...); Returns timestamps relative to photo-diode
% light pulse. PsychRTBox('1'); returns relative to press of 1st button,
% etc. etc.
%
%
% sendTime = PsychRTBox('SerialTrigger' [, handle]);
% -- Send a software generated trigger to the box via the serial port
% connection. This will register as a event of type 'serial' and you can
% retrieve timestamps relative to the first trigger within a response
% period via the PsychRTBox('serial', ...); command.
%
%
% sendTime = PsychRTBox('EngageLightTrigger [, handle]);
% sendTime = PsychRTBox('EngagePulseTrigger [, handle]);
% sendTime = PsychRTBox('EngageTRTrigger [, handle]);
% 
% -- Engage trigger input on the box for reception of a one-shot trigger
% signal. This function will return immediately after submitting the
% request to the box. It may take up to 5 msecs worst-case until the
% trigger input is really enabled. If you want to wait for the trigger to
% be really enabled, call, e.g., PsychRTBox('Enable', handle, 'lighton'); instead,
% as that function will wait until the trigger is really active.
%
% Trigger events are special: If a trigger has been received, the
% box auto-disables the trigger input, preventing reception of any
% further trigger events, until the trigger gets reenabled. The trigger gets
% reenabled on many occasions if it has been enabled once via the
% PsychRTBox('Enable', ...); command, e.g., at each call to
% PsychRTBox('Start'); or PsychRTBox('Clear'). If you want to enable the
% trigger on-the-fly, then this function is your friend.
%
% The reason why light trigger auto-disables itself is because a typical
% CRT display monitor would generate such trigger signals at the rate of
% video refresh, once your stimulus is displayed, e.g., at a rate of 100
% Hz. Usually you only want to know one defined timestamp of initial
% stimulus onset, therefore the box prevents reception of all but the
% first light trigger.
%
% Similar reasoning applies to Pulse and TR triggers.
%
%
% oldNames = PsychRTBox('ButtonNames' [, handle] [, newNames]);
% -- Query or assign labels for the four response box buttons other than
% the default names.
%
% This function allows to assign arbitrary names to the four buttons on the
% box. These names are reported when querying for button presses and
% releases. By default, oldNames = PychRTBox('ButtonNames') would return
% the cell array with the four following names: '1', '2', '3', '4'. These
% are the names reported for button presses. Button releases would report
% the names with an 'up' appended, ie., '1up', '2up', '3up', '4up'. You can
% assign arbitrary new names by passing a cell array with four namestrings,
% e.g., PsychRTBox('ButtonNames', [], {'7', 'whats', 'hick', 'screw'})
% would assign the names '7', 'whats', 'hick' and 'screw' for button press
% events, and '7up', 'whatsup', 'hickup' and 'screwup' for release events
% of the corresponding buttons.
%
% Please note that the assignment of names to buttons must be unique, ie.
% assigning the same name to multiple buttons is not allowed.
%
%
% oldIntervals = PsychRTBox('DebounceInterval' [, handle] [, debounceSecs]);
% -- Query current button debounce intervals (in 4-element vector
% 'oldIntervals', one value for each button), and optionally set new
% debounce interval in seonds via the optional argument 'debounceSecs'.
% 'debounceSecs' can be a scalar, in which case the same setting is applied
% to all buttons, or a 4-element row vector, e.g., [0.1, 0.1, 0.1, 0.1] to
% set an individual interval for each of the four buttons.
%
% The built-in debouncer prevents multiple button responses (button press
% or release actions) from getting recorded/reported within some
% 'debounceSecs' debounce interval. After a button has changed state, only
% the type (press or release), identity (which button) and timestamp (when)
% of the first state change is reported. Any other state change within
% 'debounceSecs' seconds of time after that first change will be ignored.
% After that time has elapsed, further state changes are reported again. By
% default, this dead "debounce" interval is set to 0.050 seconds, ie., 50
% msecs. Button bouncing happens if a subject presses or releases a button
% very rapidly or vigorously. If such quick multiple events or bounces are
% not ignored, they will create multiple apparent button responses which
% are a hazzle to deal with in experiment scripts and data analysis.
%
% If you find multiple responses generated for only one apparent button
% press during piloting, you may want to set a bigger debounce interval
% with this function.
%
% Please note that debouncing doesn't apply to the PsychRTBox('ButtonDown')
% function.
%
% Please also note that there is another hardware debouncer with a duration
% of 0.3 msecs on RTBox versions with firmware versions 1.3 and older, which
% can't be disabled, so even if you'd set a zero interval here, you'd still
% get a minimum 0.3 msecs debounce period from the hardware itself.
%
% Later versions of the firmware support the following
% PsychRTBox('HardwareDebounce') command to control the hardware debounce
% interval more fine-grained.
%
%
% [oldValue] = PsychRTBox('HardwareDebounce' [, handle] [, scanNum]);
% -- Set/get hardware debouncer setting. The hardware will treat a button
% event as valid only if the button state stays stable for at least
% 'scanNum' scanning iterations of the firmware. The scan interval is about
% 67 microseconds. The valid scanNum is from 1 through 255, with a default
% setting of 16 cycles for 1.072 msecs debounce interval.
%
% For software debouncing at the driver level, see PsychRTBox('DebounceInterval')
% above. 
% 
%
% buttonState = PsychRTBox('ButtonDown' [, handle] [, whichButtons]);
% -- This reports the current button state of all response buttons of box
% 'handle', or a subset of response buttons if specified by the optional
% 'whichButtons' argument, e.g., whichButton = {'1', '4'} to only test
% buttons 1 and 4. 'buttonState' is a vector which contains a 1 for each
% pressed button, and a zero for each released button.
%
% This query is as instantaneous and "live" as possible. The reported state
% is not subject to button debouncing, but the measured "raw state".
% Usually you will want to use the PsychRTBox('GetSecs' ...) functions ans
% similar functions to query timestamped button state. They are typically
% as fast as this method and they provide timestamps of when the state was
% queried, whereas this function doesn't give you information about how
% "fresh" or recent the query is. However for simple button queries outside
% the response interval, e.g., while the box is PsychRTBox('Stop')'ped with
% no need for timestamps, this may be an option.
%
% Due to the design of the USB bus, the query may be outdated wrt. to the
% real state by up to 16 - 21 msecs, depending on operating system and
% driver configuration.
%
%
% buttonState = PsychRTBox('WaitButtonDown' [, handle] [, whichButtons]);
% -- Wait until at least one of the specified buttons in 'whichButtons' is
% pressed down. If 'whichButtons' is omitted, all buttons are tested.
%
%
% PsychRTBox('WaitButtonUp' [, handle] [, whichButtons]);
% -- Wait until all of the specified buttons in 'whichButtons' are
% released. If 'whichButtons' is omitted, all buttons are tested.
%
%
% [timeSent, confidence] = PsychRTBox('TTL' [, handle] [, eventCode=1]);
% - Send TTL to DB-25 port (pin 8 is bit 0). The second input is event code
% (default 1 if omitted), 4-bit (0~15) for box versions < 5, and 8-bit
% (0~255) for later versions. It can also be equivalent binary string, such
% as '0011'.
%
% The optional return arguments are the 'timeSent' when the TTL update was
% performed, and an upper bound on the uncertainty 'confidence' of
% 'timeSent'.
%
% The width (duration) of the TTL pulse is controlled by the
% PsychRTBox('TTLWidth') command.
%
% This function is only supported for v3.0 RTBoxes and later, the ones with
% EEG event code support.
%
% 
% [oldValue] = PsychRTBox('TTLWidth' [, handle][, widthSecs]);
% - Set/get TTL pulse width in seconds. The default width is 0.97e-3, ie.
% 97 microseconds when the device is opened. The actual width may have some
% small variation. The supported width ranges from 0.14e-3 to 35e-3 secs. A
% infinite width 'inf' is also supported. Infinite width means the TTL will
% stay until it is changed by the next PsychRTBox('TTL') command, such as
% PsychRTBox('TTL',0).
%
% This function is only supported for v3.0 RTBoxes and later, the ones with
% EEG event code port support.
%
% In Version <5.0, the TTL width at DB-25 pins 17~24 is controlled by a
% potentiometer inside the box. In Version >= 5, the width is also
% controlled by 'TTLWidth' command. 
% 
%
% [oldValue] = RTBox('TTLResting' [, handle][, newLevel]);
% - Set/get TTL polarity for DB-25 pins 1~8. The default is 0, meaning the
% TTL resting is low. If you set newLevel to nonzero, the resting TTL will
% be high level. If you need different polarity for different pins, let us
% know. This function is only supported with firmware version 3.1 and
% later.
% 
% In Version 5.0 and later, newLevel has second value, which is the
% polarity for pins 17~24.
% 

% TODO:
%
% - Debouncing for PsychRTBox('Buttondown') as well, or leave it "raw"?
%

% History:
% 08/01/2008 Initial implementation based on RTBox.m from Xiangrui Li (MK).
% 01/29/2009 "Close to beta" release. First checkin to SVN (MK).
% 01/30/2009 Improved syncClocks algorithm, option to spec a specific box
%            by port in the open call (MK).
% 02/08/2009 Huge redesign of API and internal routines. Now we use an
%            internal queue (MK).
% 02/14/2009 Refinements and cleanup (MK).
% 02/15/2009 More refinements and rework of post-hoc timestamp remapping (MK).
% 06/07/2009 Check for ambiguous assignment of buttonnames to avoid errors.
%            Bug found by Vinzenz Schoenfelder (MK).
% 06/14/2009 Remove special case code for Octave. No longer needed (MK).
% 12/14/2009 Update for RTBox'es with firmware v1.4 and later:
%
%            * Store box firmware version numerically for easier comparison.
%
%            * v >= 1.4 sends '?' acknowledge for live button query before
%              the byte that reports buttons state --> Handle this.
%
%            * Detect clock frequency of RTBox and adapt our bytes2secs() mapping
%              accordingly for V1.4 and later.
%
%            * 'lightoff' event is dead, now its called 'tr' instead for TR
%              scanner trigger reception on new V3.0 firmware boxes.
%
%            * Switch v1.3 and later boxes back to 'x' simple E-Prime et
%              al. compatible mode at PsychRTBox('close') time.
%
%            * Support new 'hardwaredebounce' command on v1.4+
%
%            * Support new 'ttlwidth' for v3.0, 'ttlresting' for v3.1+
%
%            * Support 4-bit TTL out port for v3.0+
%
%            * Add fast-engage commands for pulse and TR triggers as well.
%
% 10/20/2011 Switch fast calibration from robustfit to polyfit() - Now we
%            always use polyfit() in all cases to avoid need for Matlab
%            statistics toolbox. The PostHoc routine always did this, but
%            the online routine didn't. (MK)
%
% 11/26/2011 Updates for Firmware versions up to V5 (MK):
%
%            * TTL out is now 8 bit capable and 'ttlwidth' and 'ttlresting'
%              needed to be updated as well.
%            * Event enable handling has changed.
%            * Various other stuff.
%
%            None of this is tested due to lack of hardware/firmware.
%
% 01/06/2012 Bugfix for Firmware versions >= 4.1. Did not receive events
%            due to wrong acknowledgement handling. (MK)
%

% Global variables: Need to be persistent across driver invocation and
% shared with internal subfunctions:
global rtbox_info;
global rtbox_global;

% Start of driver code -- Entry point:

    % First time invocation? Perform init of device arrays and global
    % settings:
    if isempty(rtbox_info)
        % Setup device info struct array, as well as per-device default
        % settings:
        % CAUTION: Same settings are reassigned in the openRTBox()
        % subfunction each time PsychRTBox('Open') is called! The settings
        % made there override the settings made here!!!
        rtbox_info=struct('events',{{'1' '2' '3' '4' '1up' '2up' '3up' '4up' 'pulse' 'light' 'tr' 'serial'}},...
                              'enabled',[], 'ID','','handle',-1,'portname',[],'sync',[],'version',[],'clkRatio',1,'verbosity',3, ...
                              'busyUntil', 0, 'boxScanning', 0, 'ackTokens', [], 'buttons', [0 0 0 0; 0 0 0 0; 0 0 0 0], ...
                              'syncSamples', [], 'recQueue', [], 'boxClockTickIntervalSecs', 1/115200);

        % Setup event codes:
        rtbox_global.eventcodes=[49:2:55 50:2:56 97 48 57 89]; % code for 12 events
        
        % List of supported subcommands:
        rtbox_global.cmds={'close', 'closeall', 'clear', 'stop', 'start', 'test', 'buttondown', 'buttonnames', 'enable', 'disable', 'clockratio', 'syncclocks', ...
              'box2getsecs', 'boxinfo', 'getcurrentboxtime','verbosity','syncconstraints', 'boxsecstogetsecs', 'serialtrigger', ...
              'debounceinterval', 'engagelighttrigger', 'waitbuttondown', 'waitbuttonup', 'ttlwidth', 'ttlresting', 'hardwaredebounce', ...
              'ttl', 'engagepulsetrigger', 'engagetrtrigger' };
          
        % Names of events that can be enabled/disabled for reporting:
        rtbox_global.events4enable={'press' 'release' 'pulse' 'light' 'tr' 'all'};
        
        % Low-level protocol codes corresponding to the events:
        rtbox_global.enableCode='DUPOFA'; % char to enable above events, lower case to disable
        
        % Preload some functions of PTB we'll need:
        eval('GetSecs;WaitSecs(0.001);');
        
        % Selection of blocking strategy that the IOPort driver shall use
        % for blocking writes:
        if IsWin || IsOSX
            % A blocking wait gives very good results on OS/X. On Windows
            % it gives the same results as a polling wait (setting 2), so
            % we prefer blocking for lower cpu load at same quality on
            % Windows as well:
            rtbox_global.blocking = 1;
        else
            % On Linux, a polling wait is of advantage, so we use that:
            rtbox_global.blocking = 2;
        end

        % No devices open at first invocation:
        rtbox_global.nrOpen = 0;
        
        % Default settings for the syncClocks() function:
        % -----------------------------------------------
        
        % Use syncClocks() method 1 with postwrite timestamp by default:
        rtbox_global.syncmode = 1;

        % Maximum duration of a syncClocks calibration run is 0.5 seconds:
        rtbox_global.maxDuration = 0.5;

        % Desired 'minwin' calibration accuracy is 0.0 msecs: If we manage
        % to get better than that, we abort sampling. We also abort
        % sampling of the rtbox_global.maxDuration is reached:
        rtbox_global.optMinwinThreshold = 0.0;

        % Maximum allowable (ie. worst) acceptable minwin for a sample:
        % We default to 1.3 msecs, as a 1.2 msecs minwin is basically never
        % exceeded. It is unlikely that all samples within a syncClocks run
        % are worse than 1.3 msecs and the run would therefore fail.
        rtbox_global.maxMinwinThreshold = 0.0013;
        
        % Worst case delay after a command has been received by the box,
        % before it gets actually dequeued from the microprocessors serial
        % receive buffer and executed: 5 msecs is a very generous value to
        % be on the safe side:
        rtbox_global.maxbusy = 0.005;
    end

    if nargin < 1
        error('You must provide a command string to PsychRTBox!');
    end

    % Command dispatch:
    cmd = lower(varargin{1});
    if isempty(cmd)
        error('You must provide a non-empty command string to PsychRTBox!');
    end
    
    if strcmp(cmd, 'closeall') % Close all devices
        % Only close our devices, not other devices that may be opened
        % via IOPort but unrelated to us:
        for i=1:length(rtbox_info)
            s=rtbox_info(i).handle;
            if s>=0
                % Disable all scanning on box before close:
                stopBox(i);

                % Close connection:
                IOPort('Close', s);
                
                rtbox_info(i).handle = -1;
            end
        end

        rtbox_global.nrOpen = 0;       % Reset count of open devices to zero.
        clear rtbox_info;              % clear main device info struct array.
        clear rtbox_global;            % clear main global settings struct.
        
        return;
    end
    
    % Open the connection to device, do initial setup and sync:
    if strcmp(cmd, 'open')
        % Assign deviceID identifier of device to open, or the default
        % name 'Default' if none specified: User can also specify a serial
        % port device name for a device:
        if nargin < 2
            deviceID=[];
        else
            deviceID=varargin{2};
        end

        if isempty(deviceID)
            deviceID = 'Default';
        end

        if nargin < 3
            skipSync = [];
        else
            skipSync = varargin{3};
        end
        
        if isempty(skipSync)
            skipSync = 0;
        end
        
        % Open and initialize box:
        openRTBox(deviceID, rtbox_global.nrOpen+1);

        % Increment count of open boxes:
        rtbox_global.nrOpen = rtbox_global.nrOpen + 1;

        % Return as handle:
        varargout{1} = rtbox_global.nrOpen;

        if ~skipSync
            % Perform initial mandatory clock sync:
            syncClocks(rtbox_global.nrOpen);
        end
        
        % Perform initial button state query:
        buttonQuery(rtbox_global.nrOpen);
        
        % Start event scanning on box, with the above default enabled setting,
        % i.e., only button press 'D' reporting active:
        startBox(rtbox_global.nrOpen, 1);
        
        return;
    end

    if strcmp(cmd, 'syncconstraints')
        % Return current constraint settings:
        varargout{1} = rtbox_global.maxDuration;
        varargout{2} = rtbox_global.optMinwinThreshold;
        varargout{3} = rtbox_global.maxMinwinThreshold;
        varargout{4} = rtbox_global.syncmode;
        
        % Set constraints for syncClocks:
        if nargin > 1 && ~isempty(varargin{2})
            rtbox_global.maxDuration = varargin{2};
        end

        if nargin > 2 && ~isempty(varargin{3})
            rtbox_global.optMinwinThreshold = varargin{3};
        end

        if nargin > 3 && ~isempty(varargin{4})
            rtbox_global.maxMinwinThreshold = varargin{4};
        end
        
        if nargin > 4 && ~isempty(varargin{5})
            rtbox_global.syncmode = varargin{5};
        end

        return;
    end
    
    % Deal with variable number of inputs:
    if nargin - 1 > 0 
        nIn = nargin - 1;
    else
        nIn = 0;
    end
    
    if nIn > 1
        in2=varargin{3};
    end

    % Device handle provided?
    if nargin > 1
        % Yes: Store it in 'id':
        id = varargin{2};
    else
        % Nope.
        id = [];
    end

    % If no device handle - or empty default handle - provided, just
    % default to the first open RTBox device for convenience in
    % setups/scripts that only use one RTBox -- which is probably the
    % common case:
    if isempty(id)
        id = 1;
    end
    
    % Child protection:
    if ~isscalar(id) || id < 1 || id > length(rtbox_info) || id > rtbox_global.nrOpen
        error('Invalid device handle specified! Did you open the device already?');
    end

    if isempty(rtbox_info(id).handle)
        error('Invalid device handle specified! Maybe you closed this device already?');
    end

    % Build additional cell array of valid read commands:
    read=rtbox_info(id).events;
    read{end+1}='secs';    % Like GetSecs see below.
    read{end+1}='boxsecs'; % All events measured in absolute box time.
    read{end+1}='getsecs'; % All events measured in absolute GetSecs time.

    % Assign serial port handle:
    s = rtbox_info(id).handle;

    % Subcommand dispatch:
    switch cmd
        case 'verbosity'
            if nIn<2
                error('You must provide the new level of "verbosity" to assign!');
            end
            
            % Return old level of verbosity:
            varargout{1} = rtbox_info(id).verbosity;

            % Assign new level of verbosity to device:
            rtbox_info(id).verbosity = in2;
            
        case 'serialtrigger' % send serial trigger to device
            tWritten = sendTrigger(id);
            if nargout, varargout{1}=tWritten; end
            
        case 'engagelighttrigger'
            % Enable light on trigger quickly.
            tWritten = engageTrigger(id, 'O');
            if nargout, varargout{1}=tWritten; end
            
        case 'engagepulsetrigger'
            % Enable pulsetrigger quickly.
            tWritten = engageTrigger(id, 'P');
            if nargout, varargout{1}=tWritten; end
            
        case 'engagetrtrigger'
            % Enable TR-Trigger quickly.
            tWritten = engageTrigger(id, 'F');
            if nargout, varargout{1}=tWritten; end
            
        case 'ttl'
            % Send TTL 4 bit event to output port on supported hardware:
            if rtbox_info(id).version < 3
                RTBoxWarn('notSupported', in1, 3);
                return;
            end
            
            % Default event code is 1:
            if isempty(in2), in2 = 1; end

            % Can be a binary string:
            if ischar(in2), in2=bin2dec(in2); end
            
            % Range check:
            if rtbox_info(id).version < 5
                maxTTL = 15;
            else
                maxTTL = 255;
            end
            
            if (in2 < 0) || (in2 > maxTTL) || (in2~=round(in2))
                RTBoxError('invalidTTL');
            end
            
            % Decode to final trigger byte:
            if rtbox_info(id).version < 3.2
                in2 = dec2bin(in2,4);
                in2 = uint8(bin2dec(in2(4:-1:1))); % reverse bit order
            end

            if rtbox_info(id).version >= 5
                in2=[1 in2];
            end

            % Emit via blocking write:
            [tsend twin] = sendTTLPortEvent(id, in2);
            if nargout
                varargout={tsend twin};
            end

            if twin > 0.003
                fprintf('PsychRTBox: Warning! TTL trigger, send timestamp uncertainty %f msecs exceeds 3 msecs!\n', twin * 1000);
            end
            
        case 'debounceinterval'            
            % Return old debouncer settings for each button:
            varargout{1} = rtbox_info(id).buttons(3, :);

            if nIn<2
                return;
            end
            
            % Assign new settings:
            if isscalar(in2)
                % Single value: Apply to all buttons.
                rtbox_info(id).buttons(3, :) = [in2, in2, in2, in2];
            else
                % Multi value: Apply individually to each button:
                if size(in2,1)~=1 || size(in2, 2)~=4
                    error('Either set a single common debounce value for all buttons or pass a 4-element row vector with 4 settings for all 4 buttons!');
                end
                rtbox_info(id).buttons(3, :) = in2;                
            end
            
            % Reset debouncer:
            rtbox_info(id).buttons(2, :) = [0, 0, 0, 0];
            
        % Retrieve all pending events from the box, aka the serial port
        % receive buffers, parse them, filter/postprocess them, optionally
        % return mapped event timestamps in GetSecs timebase:
        case read % 12 triggers, plus 'secs' 'boxsecs' 'getsecs'
            cmdInd=strmatch(cmd,read,'exact'); % which command

            % Timestamp relative to trigger wanted?
            if cmdInd<13
                ind=[cmdInd<5 (cmdInd<9 && cmdInd>4) cmdInd==9:11];
                if ~rtbox_info(id).enabled(ind), RTboxError('triggerDisabled',rtbox_global.events4enable{ind}); end
                % minbytes=14; % at least 2 events
            end
            
            % Preinit return args to empty in case no event is detected:
            varargout={[] '' []};
            
            % 2nd argument is inter-response timeout: Return if there isn't
            % any data received for that amount of time. Each received new
            % item will extend that timeout by given amount:
            if nIn > 1 && ~isempty(varargin{3})
                intertimeout = varargin{3};
                if intertimeout <=0
                    error('Invalid interTimeout value specified. Must be significantly > 0 secs!');
                end
            else
                % Default is 0.1 secs aka 100 msecs:
                intertimeout = 0.1;
            end

            % 3rd argument is absolute timeout for all responses, the
            % absolute upper bound:
            if nIn > 2 && ~isempty(varargin{4})
                abstimeout = varargin{4};
                if abstimeout <=0
                    error('Invalid maxTimeout value specified. Must be significantly > 0 secs!');
                end
            else
                % Default is to set it to intertimeout:
                abstimeout = intertimeout;
            end
            
            % 4th argument is maximum number of responses to fetch at most:
            if nIn > 3 && ~isempty(varargin{5})
                maxItems = varargin{5};
            else
                % Default is to infinite, i.e., no limits: We get all we
                % can get within the setup timeout intervals:
                maxItems = inf;
            end
            
            % Retrieve events:
            [evid, timing] = getEvents(id, maxItems, maxItems, abstimeout, intertimeout);
            nevent = length(evid);
            
            % Anything retrieved?
            if nevent == 0
                return;
            end

            if nargout > 1 || cmdInd < 13
                % Map event id to human readable label string:
                for i=1:nevent % extract each event and time
                    ind=min(find(evid(i)==rtbox_global.eventcodes)); %#ok<MXFND> % which event
                    if isempty(ind)
                        RTboxWarn('invalidEvent',evid(i));
                        break; % not continue, rest must be messed up
                    end
                    event{i} = rtbox_info(id).events{ind}; %#ok event name
                end
            end

            if isempty(timing), return; end

            % Convert boxtiming and/or map it to host clock time:
            if cmdInd==15 || cmdInd==13
                % Convert into computer time: MK-Style

                % First return optional "raw" array with boxtimes:
                varargout{3} = timing;
                
                % Then remap to GetSecs host timebase:
                timing = box2GetSecsTime(id, timing);
                
            elseif cmdInd<13 % trigger: relative to trigger
                ind=strmatch(cmd,lower(event),'exact'); % trigger index
                if isempty(ind), RTboxWarn('noTrigger',cmd); return; end
                ind=ind(1); % use the 1st in case of more than 1 triggers
                trigT=timing(ind); % time of trigger event
                event(ind)=[]; timing(ind)=[]; % omit trigger and its time from output
                if isempty(event), return; end % if only trigger event, return empty
                timing=timing-trigT;   % relative to trigger time
            end

            varargout{1} = timing;

            if nargout > 1
                if length(event)==1, event=event{1}; end % if only 1 event, use string
                varargout{2} = event;
            end
            
        case 'boxinfo'
            % Return complete device info struct:
            varargout{1} = rtbox_info(id);
            
            % Optionally set a new one -- Only for debugging!!
            if nIn > 1
                rtbox_info(id) = in2;
            end
            
        case 'box2getsecs'
            % Map boxtime to GetSecs time with the recommended method:
            if nIn<2
                error('You must provide the boxtime to map!');
            end
            
            varargout{1} = box2GetSecsTime(id, varargin{3});
            
        case 'boxsecstogetsecs'
            % Map boxtime to GetSecs time post-hoc style:
            % We compute an optimal least-squares fit linear mapping
            % function of boxtime to hosttime, using all collected
            % syncClocks samples from the whole experiment session. Then we
            % remap all given input boxsecs samples to getsecs time via
            % lookup in that linear best-fit. This automatically corrects
            % for clock-drift and should provide the least possible error
            % on the mapping procedure, because it makes use of all
            % available sync information of a whole session:
            if nIn<2
                error('You must provide the boxtimes to map!');
            end
            
            [remapped, sd, clockratio] = box2GetSecsTimePostHoc(id, varargin{3}); 
            varargout = { remapped, sd , clockratio};
                                    
        case 'getcurrentboxtime'
            % Retrieve current time of box clock.
            
            % We do so by performing a syncClocks call, but retaining the
            % current .sync results, so we just (mis-)use the function for
            % our purpose:
            tmpsync = rtbox_info(id).sync;
            syncClocks(id); % clear buffer, sync clocks
            varargout{1} = rtbox_info(id).sync;
            rtbox_info(id).sync = tmpsync;

        case 'stop'
            % Stop event processing and reporting on box:
            % This will store all pending events in internal queue:
            stopBox(id);
            
        case 'start'
            % (Re-)Start event processing and reporting on box:

            if nIn >= 2 && ~isempty(varargin{3}) && varargin{3}
                % Asynchronous start, i.e. don't wait for acknowledge but
                % return asap:
                waitForStart = 0;
            else
                % Wait for acknowledge of start:
                waitForStart = 1;
            end
            
            startBox(id, waitForStart);
            
        case 'clear'
            % Clear all pending events on box, optionally perform clocksync:
            % By default, box is restarted after clear and no clocksync is
            % performed, but box is not restarted if usercode doesn't want
            % this or if it wasn't running before. Optionally clocksync is
            % executed:
            
            % Stop event processing on box, if active:
            boxActive = rtbox_info(id).boxScanning;
            if boxActive
                % This will store all pending events in internal queue:
                stopBox(id);
            end
            
            % Clear all buffers (serial buffers and event queue):
            purgeRTbox(id);
            
            % Optional syncClocks requested?
            if nIn >= 2 && ~isempty(varargin{3}) && varargin{3}
                % Perform clockSync:
                syncClocks(id);
                
                if nargout
                    varargout{1}=rtbox_info(id).sync;
                    varargout{2}=rtbox_info(id).clkRatio;
                end
            end
            
            % Restart box? We restart if it was running before and usercode
            % doesn't forbid a restart:
            if boxActive && (nIn < 3 || isempty(varargin{4}) || varargin{4} == 0)
                startBox(id, 1);
            end
            
        case 'syncclocks'
            % Synchronize host clock and box clock, i.e., establish mapping
            % between both:

            % Stop event processing on box, if active:
            boxActive = rtbox_info(id).boxScanning;
            if boxActive
                % This will store all pending events in internal queue:
                stopBox(id);
            end
            
            syncClocks(id); % clear buffer, sync clocks
            if nargout
                varargout{1}=rtbox_info(id).sync;
                varargout{2}=rtbox_info(id).clkRatio;
            end

            if boxActive
                % Restart box if it was running:
                startBox(id, 1);
            end
            
        case 'buttondown'
            % Perform query:
            b2 = buttonQuery(id);
            
            if nIn<2, in2=read(1:4); end % not specified which button
            in2=cellstr(in2); % convert it to cellstr if it isn't
            for i=1:length(in2)
                ind=strmatch(lower(in2{i}),read(1:4),'exact');
                if isempty(ind), RTboxError('invalidButtonName',in2{i}); end
                bState(i)=b2(ind); %#ok
            end

            varargout{1} = bState;
            
        case 'waitbuttondown'
            if nIn<2, in2=read(1:4); end % not specified which button
            in2=cellstr(in2); % convert it to cellstr if it isn't
            bState = zeros(1,length(in2));
            
            % Repeat queries until at least one of the target buttons is
            % down:
            while ~any(bState)
                b2 = buttonQuery(id);
                for i=1:length(in2)
                    ind=strmatch(lower(in2{i}),read(1:4),'exact');
                    if isempty(ind), RTboxError('invalidButtonName',in2{i}); end
                    bState(i)=b2(ind); %#ok
                end
            end
            
            varargout{1} = bState;

        case 'waitbuttonup'
            if nIn<2, in2=read(1:4); end % not specified which button
            in2=cellstr(in2); % convert it to cellstr if it isn't
            bState = ones(1,length(in2));
            
            % Repeat queries until all of the target buttons are up:
            while any(bState)
                b2 = buttonQuery(id);
                for i=1:length(in2)
                    ind=strmatch(lower(in2{i}),read(1:4),'exact');
                    if isempty(ind), RTboxError('invalidButtonName',in2{i}); end
                    bState(i)=b2(ind); %#ok
                end
            end
            
        case 'buttonnames' % set or query button names
            oldNames=rtbox_info(id).events(1:4);
            if nIn<2, varargout{1}=oldNames; return; end
            if length(in2)~=4 || ~iscellstr(in2), RTboxError('invalidButtonNames'); end
            
            for i=1:length(in2)
                in2{i} = lower(in2{i});
            end
            
            % Check for unambiguous assignment: Each buttonname must be
            % unique!
            for i=1:length(in2)
                if length(strmatch(lower(in2{i}), in2,'exact')) ~= 1
                    % Ambituous naming -> Same buttoname multiple times!
                    error('Same name assigned to multiple buttons! Sorry, names must be unique!');
                end
            end
            
            rtbox_info(id).events(1:4)=in2;
            for i=5:8
                rtbox_info(id).events(i)=cellstr([char(rtbox_info(id).events(i-4)) 'up']);
            end
            if nargout, varargout{1}=oldNames; end

            
        case 'ttlwidth'
            % One of the commands that change Firmware device settings.
            % Need to stop event processing and drain the event queue, as
            % the protocol doesn't implement proper acknowledge tokens:
            if rtbox_info(id).version < 3
                RTBoxWarn('notSupported', in1, 3);
                return;
            end
            
            % Stop event processing on box, if active:
            boxActive = rtbox_info(id).boxScanning;
            if boxActive
                % This will store all pending events in internal queue:
                stopBox(id);
            end

            % Perform active query for current firmware settings:
            b8=get8bytes(id);
            
            % TTL width unit in s, not very accurate:
            wUnit=0.139e-3; 
            
            % Return old/current setting as 1st argument:
            if rtbox_info(id).version > 4, b8(1) = 255 - b8(1); end
            varargout{1} = b8(1) * wUnit;

            % New settings provided?
            if nIn >= 2
                if isempty(in2)
                    % Default to 0.00097 seconds:
                    in2=0.00097;
                end

                % Infinity means: Disable TTL-width, hold setting until
                % manually changed:
                if in2 == inf
                    in2 = 0;
                end

                % Range check:
                if (in2 < wUnit * 0.9 || in2 > wUnit * 255 * 1.1) && (in2>0)
                    RTBoxWarn('invalidTTLwidth', wUnit);
                    return;
                end

                width = double(uint8(in2 / wUnit)) * wUnit;
                b8(1) = width / wUnit;
                if rtbox_info(id).version > 4, b8(1) = 255 - b8(1); end

                % Writeback new firmware settings:
                set8bytes(id, b8);
                
                if (in2 > 0) && (abs(width - in2) / in2 > 0.1)
                    RTBoxWarn('widthOffset', width);
                end
                
                if width==0
                    width=inf;
                end
                
                % Return new / current setting as 1st argument:
                varargout{1} = width;
            end
            
            if boxActive
                % Restart box if it was running:
                startBox(id, 1);
            end
            
        case 'ttlresting'
            % One of the commands that change Firmware device settings.
            % Need to stop event processing and drain the event queue, as
            % the protocol doesn't implement proper acknowledge tokens:
            if rtbox_info(id).version < 3.1
                RTBoxWarn('notSupported', in1, 3.1);
                return;
            end
            
            % Stop event processing on box, if active:
            boxActive = rtbox_info(id).boxScanning;
            if boxActive
                % This will store all pending events in internal queue:
                stopBox(id);
            end

            % Perform active query for current firmware settings:
            b8=get8bytes(id);
            
            % Return current setting as 1st return argument:
            varargout{1} = (b8(3) > 0);
            
            % New settings provided?
            if nIn >= 2
                if isempty(in2)
                    % Default to 0:
                    in2 = logical([0 1]);
                end
                                
                % Assign valid new setting:
                if rtbox_info(id).version < 5
                    b8(3) = in2(1) * 240; % '11110000'
                else
                    b8(3) = bitset(b8(3),1,in2(1));
                    b8(3) = bitset(b8(3),2,in2(2));
                end
                
                % Writeback new firmware settings:
                set8bytes(id, b8);
            end
                        
            if boxActive
                % Restart box if it was running:
                startBox(id, 1);
            end
            
            
        case 'hardwaredebounce'
            % One of the commands that change Firmware device settings.
            % Need to stop event processing and drain the event queue, as
            % the protocol doesn't implement proper acknowledge tokens:
            if rtbox_info(id).version < 1.4
                RTBoxWarn('notSupported', in1, 1.4);
                return;
            end
            
            % Stop event processing on box, if active:
            boxActive = rtbox_info(id).boxScanning;
            if boxActive
                % This will store all pending events in internal queue:
                stopBox(id);
            end

            % Perform active query for current firmware settings:
            b8=get8bytes(id);

            % Return current setting as 1st return argument:
            varargout{1} = b8(2);
            
            % New settings provided?
            if nIn >= 2
                if isempty(in2)
                    % Default to 16 firmware scancycles:
                    in2 = 16;
                end
                
                % Range check:
                if in2 < 1 || in2 > 255
                    % Invalid: Warn & Ignore:
                    RTBoxWarn('invalidScanNum');
                else
                    % Assign valid new setting:
                    b8(2) = uint8(in2);

                    % Writeback new firmware settings:
                    set8bytes(id, b8);
                end                
            end
            
            if boxActive
                % Restart box if it was running:
                startBox(id, 1);
            end
            
        case {'enable', 'disable'} % enable/disable event detection
            if nIn<2 % no event, return current state
                varargout{1}=rtbox_global.events4enable(rtbox_info(id).enabled);
                return;
            end

            % Stop event processing on box, if active:
            boxActive = rtbox_info(id).boxScanning;
            if boxActive
                % This will store all pending events in internal queue:
                stopBox(id);
            end
            
            isEnable=strcmp(cmd,'enable');
            in2=lower(cellstr(in2));
            for i=1:length(in2)
                ind=strmatch(in2{i},rtbox_global.events4enable,'exact');
                if isempty(ind), RTboxError('invalidEnable',rtbox_global.events4enable); end
                if ind==6, ind=1:5; end % all
                rtbox_info(id).enabled(ind)=isEnable; % update state
            end
            if nargout, varargout{1}=rtbox_global.events4enable(rtbox_info(id).enabled); end
            if ~any(rtbox_info(id).enabled), RTboxWarn('allDisabled',rtbox_info(id).ID); end
            
            if boxActive
                % Restart box if it was running:
                startBox(id, 1);
            end
            
        case 'clockratio' % measure clock ratio computer/box
            % Default to 60 seconds for clock ratio calibration, unless
            % specified otherwise:
            if nIn<2, in2=60; end;

            % Interval between trials
            interval = 1.5 * rtbox_global.maxDuration;
            
            % Number of trials to perform:
            ntrial=max(5,round(in2/interval));

            if rtbox_info(id).verbosity > 2
                fprintf('PsychRTBox: Measuring clock ratio on box "%s". Trials remaining:%4.f', rtbox_info(id).ID, ntrial);
            end
            
            % Stop event processing on box, if active:
            boxActive = rtbox_info(id).boxScanning;
            if boxActive
                % This will store all pending events in internal queue:
                stopBox(id);
            end
            
            % Switch to realtime priority if not already there:
            oldPriority=Priority;
            if oldPriority < MaxPriority('GetSecs')
                Priority(MaxPriority('GetSecs'));
            end
            
            % Perform ntrial calibration trials:
            tnow = GetSecs;
            for i=1:ntrial
                % Update rtbox_info.sync via a syncClocks() operation:
                syncClocks(id);
                
                % Store new syncClocks sample in array:
                t(i,:)=rtbox_info(id).sync; %#ok<AGROW>

                % Give status output:
                if rtbox_info(id).verbosity > 2
                    fprintf('\b\b\b\b%4.f',ntrial-i);
                end

                % Make sure that trials are at least 'interval' secs apart:
                tnow = WaitSecs('UntilTime', tnow + interval);
            end

            % Restore priority to old value:
            if Priority ~= oldPriority
                Priority(oldPriority);
            end
            
            % Delete syncClocks samples collected during this clockRatio
            % calibration from internal array, as they might bias later
            % box->host time remapping:
            rtbox_info(id).syncSamples = rtbox_info(id).syncSamples(1:end-ntrial, :);
            
            % Restart scanning on box if it was active before:
            if boxActive
                startBox(id, 1);
            end
            
            % Use always polyfit to fit a line (with least squares error)
            % to the samples of host clock and box clock measurements. We
            % use polyfit because it is part of default Matlab/Octave,
            % doesn't require statistics toolbox or fitting toolbox:
            [coef, st] = polyfit(t(:,2)-t(1,2),t(:,1)-t(1,1), 1);  % fit a line
            sd = st.normr * 1000; % std in ms

            % Assign new clock ratio for use by the timestamp mapping
            % routines later on:
            rtbox_info(id).clkRatio = coef(1);

            if rtbox_info(id).verbosity > 2
                fprintf('\n Clock ratio (computer/box): %.7f