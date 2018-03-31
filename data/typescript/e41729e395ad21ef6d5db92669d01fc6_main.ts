# The main file executed by Tritium. The start of all other files.


match(inferred_content_type()) {
  with(/html/) {

    protect_xmlns()

    # Force UTF-8 encoding. If you'd like to auto-detect the encoding,
    # simply remove the "UTF-8" argument.  e.g. html(){ ... }
    html("UTF-8") {
      @import "html.ts"
    }

    restore_xmlns()

    restore_dollar_sign()
  }
  
  else() {
    log("Passing through " + $content_type + " unmodified.")
  }
}
