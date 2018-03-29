// manage outgoing and incoming patch queues

var lasttypetime = new Date(); 

var chainpatches = [ ];   // stack going out
var chainpatchnumber = 0; // counts them going out 

var nextchainpatchnumbertoreceive = 0; 
var lastreceivedchainpatch = null; 


// keep delivery load of chain patches down and remove excess typing signals
function sendChainPatches()
{
    if (chainpatches.length > 0)
    {
        var chainpatch = chainpatches.shift(); 
        //writeToChat("-- "+$.toJSON(chainpatch)); 
        if (bConnected)
            sendjson(chainpatch); 
    }
    // clear out the ones that are pure typing messages sent in non-broadcast mode
    while ((chainpatches.length > 0) && (chainpatches[0].insertlinenumber == undefined))
        chainpatches.shift(); 

    if (chainpatches.length > 0)
        window.setTimeout(sendChainPatches, 2); 
}


// code shared with reload code so we can use same system to suppress edited messages from codemirror
function receiveChainpatchFromQueue(reloadedcode)
{
    // handle bail out conditions
    if (reloadedcode == null)
    {
        if (nextchainpatchnumbertoreceive == -1)
            receivechainpatchqueue.length = 0; 
        var chainpatch = (receivechainpatchqueue.length != 0 ? receivechainpatchqueue.shift() : null); 
        if ((chainpatch != null) && ((chainpatch.chainpatchnumber != nextchainpatchnumbertoreceive) || (chainpatch.rev != lastRev)))
        {
                // this will be handled some other time (for someone joining in as we are already in full flow)
            writeToChat('<i>'+chainpatch.chatname+' typed something but this window is not synchronized to receive it</i>'); 
            nextchainpatchnumbertoreceive = -1; 
            receivechainpatchqueue.length = 0; 
            chainpatch = null; 
        }
        if (chainpatch == null)
        {
            receivechainpatchcall = null; 
            $('li#idtopcodetab a').removeClass("othertypingalert").css("background-color", "#ffffff");
            return; 
        }
    }

        // the callback into the onchange function appears to happen in same thread without a timeout 
        // so we have to suppress the re-edits with this flag here.
        // some callbacks into onchange are deferred, so it is unpredictable and hard to detect 
        // (unless we were going to watch the patches being created and compare them to the one we committed to tell the difference between typing)
        // so we're going to use a few second delay to suppress messages going through and highlight with an chatalert colour on the tab
        // which will help see stuff when it appears to go wrong.  
    $('li#idtopcodetab a').addClass("othertypingalert").css("background-color", "#ffff87"); // backgroundcolour setting by class doesn't work
    if ((reloadedcode != null) && (receivechainpatchcall != null))
        window.clearTimeout(receivechainpatchcall); 
    receivechainpatchcall = "doingothertyping"; 
    if (reloadedcode == null)
    {
        var mismatchlines = recordOtherTyping(chainpatch, codeeditor);  

        // log the mismatch cases, which look like they are coming from the unreliability of 
        // CM_newLines where the lines are changed prior to the next undo stack commit
        // therefore the set of patches are actually inconsistent, usually between immediate successor patches, 
        // so we have the previous patch and the ptime difference to verify this
        if (mismatchlines.length != 0)
        {
            writeToChat("Mismatches "+$.toJSON(mismatchlines)); 
            writeToChat("chainpatch " + $.toJSON(chainpatch)); 
            if (lastreceivedchainpatch)
                writeToChat("prevchainpatch " + $.toJSON(lastreceivedchainpatch)); 
        }
        nextchainpatchnumbertoreceive++;  // next value expected
        chainpatchnumber = nextchainpatchnumbertoreceive; 
        lastreceivedchainpatch = chainpatch; 
    }
    else
        codeeditor.setCode(reloadedcode); 
    receivechainpatchcall = window.setTimeout(function() { receiveChainpatchFromQueue(null); }, (((reloadedcode == null) && (receivechainpatchqueue.length != 0)) ? 10 : 5000));  
}

