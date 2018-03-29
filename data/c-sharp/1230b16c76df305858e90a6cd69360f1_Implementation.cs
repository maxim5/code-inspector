
using System;


namespace Boilen {

    /// <summary>
    /// Specifies the kinds of member implementation.
    /// </summary>
    [Flags]
    public enum Implementation {

        None = 0,
        Auto = 1 << 0,
        Custom = 1 << 1,
        AutoAndCustom = Auto | Custom,

    }

}
