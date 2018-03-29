/*
 * Implementation of the FuzzExtension.h and IFuzzExtension.h
 * headers. This class is the implementation for the XPCOM interfaces
 * which are called via Javascript to control the Fuzz components.
 */
#if defined(__linux__)
#include <sys/types.h>
#include <signal.h>
#endif

#include <iostream>
#include <stdio.h>
#include "xpcom-config.h"
#include "nsXPCOM.h"
#include "nsCOMPtr.h"
#include "nsIProperties.h"
#include "nsIServiceManager.h"
#include "nsServiceManagerUtils.h"
#include "nsDirectoryServiceDefs.h"
#include "nsIObserverService.h"
#include "nsIGenericFactory.h"
#include "nsIFile.h"
#include "FuzzExtension.h"

#include "rdfa.h"
#include "rdfa_utils.h"

#include "tidy/tidy.h"
#include "tidy/buffio.h"

using namespace std;

// These helper functions are used by this module to provide a link to
// the librdfa library.

/**
 * The buffer status struct is used to keep track of where we are in
 * the current processing buffer.
 */
typedef struct buffer_status
{
   const char* buffer;
   unsigned int current_offset;
   unsigned int total_length;
   fuzzJSTripleHandlerCallback* javascript_callback;
} buffer_status;

/**
 * Processes triples as they come in.
 *
 * @param triple the triple that was generated.
 * @param callback_data a buffer_status struct that contains the
 *                      current state of the buffer.
 */
void process_triple(rdftriple* triple, void* callback_data)
{
   buffer_status* status = (buffer_status*)callback_data;
   
   rdfa_print_triple(triple);

   // Perform a Javascript callback using XPCOM
   nsCOMPtr<fuzzJSTripleHandlerCallback> javascript_callback =
      status->javascript_callback;
   PRBool ret = PR_FALSE;
   //javascript_callback->HandleTriple(
   //   triple->subject, triple->predicate, triple->object, &ret);
   javascript_callback->Call(
      triple->subject, triple->predicate, triple->object, &ret);
   
   rdfa_free_triple(triple);
}

/**
 * Fills the given buffer up to buffer_length if enough data is
 * contained in the current buffer that is being processed.
 *
 * @param buffer the buffer to fill.
 * @param buffer_length the length of the given buffer.
 * @param callback_data a buffer_status struct that contains the
 *                      current state of the buffer.
 */
size_t fill_buffer(char* buffer, size_t buffer_length, void* callback_data)
{
   size_t rval = 0;
   buffer_status* bstatus = (buffer_status*)callback_data;

   if((bstatus->current_offset + buffer_length) < bstatus->total_length)
   {
      rval = buffer_length;
      memcpy(buffer, &bstatus->buffer[bstatus->current_offset], buffer_length);
      bstatus->current_offset += buffer_length;
   }
   else
   {
      rval = bstatus->total_length - bstatus->current_offset;
      memcpy(buffer, &bstatus->buffer[bstatus->current_offset], rval);
   }
   
   return rval;
}

/*
 * Magic Firefox macro that creates a default factory constructor for
 * the Fuzz extension.
 */
NS_GENERIC_FACTORY_CONSTRUCTOR(nsFuzzExtension)

/*
 * Magic Firefox macro that states that the nsFuzzExtension class
 * supports the IDL defined in IFuzzExtension.
 */
NS_IMPL_ISUPPORTS1(nsFuzzExtension, nsIFuzzExtension)

nsFuzzExtension::nsFuzzExtension()
{
  /* member initializers and constructor code */
}

nsFuzzExtension::~nsFuzzExtension()
{
  /* destructor code */
}

/* boolean processRdfaTriples(); */
NS_IMETHODIMP nsFuzzExtension::ProcessRdfaTriples(const char* uri, const char* html, fuzzJSTripleHandlerCallback* callback, PRInt32 *_retval)
{
   nsresult rval = NS_OK;
   
   //printf("URI: %s\n", uri);
   //FILE* hfile = fopen("/tmp/fuzz.html", "w");
   //fprintf(hfile, "%s", html);
   //fclose(hfile);
   
   rdfacontext* context = rdfa_create_context(uri);
   buffer_status* status = (buffer_status*)malloc(sizeof(buffer_status));
   
   // initialize the callback data
   status->buffer = html;
   status->current_offset = 0;
   status->total_length = strlen(html);
   status->javascript_callback = callback;
   context->callback_data = status;
   
   // setup the parser
   rdfa_set_triple_handler(context, &process_triple);
   rdfa_set_buffer_filler(context, &fill_buffer);
   (*_retval) = rdfa_parse(context);

   // free the context
   rdfa_free_context(context);
   
   return rval;
}

/* boolean processRdfaTriples(); */
NS_IMETHODIMP nsFuzzExtension::TidyAndProcessRdfaTriples(const char* uri, const char* html, fuzzJSTripleHandlerCallback* callback, PRInt32 *_retval)
{
   nsresult rval = NS_OK;
   TidyBuffer output;
   TidyBuffer errbuf;
   int rc = -1;
   Bool ok;

   //FILE* hfile = fopen("/tmp/fuzz-untidied.html", "w");
   //fprintf(hfile, "%s", html);
   //fclose(hfile);
   
   TidyDoc tdoc = tidyCreate();                    // Initialize "document"
   tidyBufInit(&output);
   tidyBufInit(&errbuf);
   //printf("Tidying:\n%s\n", html);
   
   ok = tidyOptSetBool(tdoc, TidyXhtmlOut, yes);   // Convert to XHTML
   if(ok)
      rc = tidySetErrorBuffer( tdoc, &errbuf );    // Capture diagnostics
   if(rc >= 0)
      rc = tidyParseString(tdoc, html);            // Parse the input
   if(rc >= 0)
      rc = tidyCleanAndRepair(tdoc);               // Tidy it up!
   if(rc >= 0)
      rc = tidyRunDiagnostics(tdoc);               // Kvetch
   if(rc > 1)                                      // If error, force output.
      rc = (tidyOptSetBool(tdoc, TidyForceOutput, yes) ? rc : -1);
   if(rc >= 0)
      rc = tidySaveBuffer(tdoc, &output);          // Pretty Print
   
   if(rc >= 0)
   {
      //if(rc > 0)
      //{
      //   printf("\nDiagnostics:\n\n%s", errbuf.bp);
      //}
      //printf("\nAnd here is the result:\n\n%s", output.bp);
   }
   else
      printf("A severe error (%d) occurred.\n", rc);

   //FILE* tfile = fopen("/tmp/fuzz-tidied.html", "w");
   //fprintf(tfile, "%s", (const char*)output.bp);
   //fclose(tfile);
   
   (*_retval) = this->ProcessRdfaTriples(
      uri, (const char*)output.bp, callback, _retval);

   tidyBufFree(&output);
   tidyBufFree(&errbuf);
   tidyRelease(tdoc);
   
   return rval;
}

/* 
 * All of the Fuzz components that are a part of the Fuzz
 * extension. This is how the various services are registered in
 * Firefox for the Fuzz Extension.
 */
static const nsModuleComponentInfo gFuzzComponents[] =
{
   {
      FUZZ_CLASSNAME,
      FUZZ_CID,
      FUZZ_CONTRACTID,
      nsFuzzExtensionConstructor
	}
};

// Create the method that will be called to register the extension.
NS_IMPL_NSGETMODULE(nsFuzzExtension, gFuzzComponents)

// This code is needed by Windows to specify an entry-point for the
// DLL
#ifdef WIN32
#include "windows.h"

BOOL APIENTRY DllMain(
   HMODULE hModule, DWORD  ul_reason_for_call, LPVOID lpReserved)
{
    return TRUE;
}
#endif
