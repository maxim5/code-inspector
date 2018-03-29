/*
 * Interface for IMasterView. 
 * Author: Anders Høst | ahkj@itu.dk
 * Date 13 dec 2010
 * 
 * */
using System;

namespace AutomaTones.View {
    public interface IMasterView {

        /// <summary>
        /// The notion of 'Play'. Propogates a play message to all CA
        /// </summary>
        Action OnPlayButtonClickEvent { get; set; }

        /// <summary>
        /// The notion of 'Pause'. Propogates a pause message to all CA.
        /// </summary>
        Action OnPauseButtonClickEvent { get; set; }

        /// <summary>
        /// The notion of 'Rewind' functionality being invoked.
        /// incrementially.
        /// </summary>
        Action OnRewindButtonClickEvent { get; set; }

        /// <summary>
        /// The notion of a 'FastForward' functionality being invoked.
        /// </summary>
        Action OnForwardButtonClickEvent { get; set; }

        /// <summary>
        ///  Change the master volume of the system
        /// </summary>
        Action<int> OnVolumeChangeEvent { get; set; }
    }
}
