package muprog

import ul.{IAttr, IObj, Config};

/** Base class for all interfaces */
abstract class Interface {
    lazy val tr = Config.tr;
    
    var tag = "";
    var name = "";
    var descr = "";
    
    /** Configuration options */
    val options = new IObj;
    
    /** Get all available ports */
    def ports:Seq[String];
    
    /** True if interface supports bitbang mode */
    var bitbangSupported = false;
    /** Number of output bitbang bits */
    var bitbangOutBits = 0;
    /** Number of input bitbang bits */
    var bitbangInBits = 0;
    
}
