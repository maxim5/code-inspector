// ============================================================================
//
// = LIBRARY
//    ulib - c++ library
//
// = FILENAME
//    uhttp.cpp - HTTP utility
//
// = AUTHOR
//    Stefano Casazza
//
// ============================================================================

#include <ulib/base/coder/xml.h>

#include <ulib/url.h>
#include <ulib/date.h>
#include <ulib/db/rdb.h>
#include <ulib/command.h>
#include <ulib/tokenizer.h>
#include <ulib/mime/entity.h>
#include <ulib/utility/uhttp.h>
#include <ulib/mime/multipart.h>
#include <ulib/utility/escape.h>
#include <ulib/utility/base64.h>
#include <ulib/base/coder/url.h>
#include <ulib/utility/dir_walk.h>
#include <ulib/utility/services.h>
#include <ulib/utility/websocket.h>
#include <ulib/net/server/server.h>
#include <ulib/utility/socket_ext.h>

#ifdef USE_LIBMAGIC
#  include <ulib/magic/magic.h>
#endif
#ifdef HAVE_LIBTCC
#  include <libtcc.h>
#endif
#ifndef __MINGW32__
#  include <sys/resource.h>
#endif

#if defined(SYS_INOTIFY_H_EXISTS_AND_WORKS) && defined(U_HTTP_INOTIFY_SUPPORT)
#  include <sys/inotify.h>
#elif defined(LINUX) || defined(__LINUX__) || defined(__linux__)
#  ifdef HAVE_SYS_INOTIFY_H
#  undef HAVE_SYS_INOTIFY_H
#  endif
#  ifndef __MINGW32__
#     include <ulib/replace/inotify-nosys.h>
#  endif
#endif

#define U_FLV_HEAD             "FLV\x1\x1\0\0\0\x9\0\0\0\x9"
#define U_STORAGE_KEYID        "STID"
#define U_TIME_FOR_EXPIRE      (u_now->tv_sec + (365 * U_ONE_DAY_IN_SECOND))
#define U_MIN_SIZE_FOR_DEFLATE 150

int         UHTTP::inotify_wd;
bool        UHTTP::nostat;
bool        UHTTP::bsendfile;
bool        UHTTP::virtual_host;
bool        UHTTP::telnet_enable;
bool        UHTTP::digest_authentication;
bool        UHTTP::enable_caching_by_proxy_servers;
void*       UHTTP::db_session;
UFile*      UHTTP::file;
UFile*      UHTTP::apache_like_log;
UString*    UHTTP::uri;
UString*    UHTTP::alias;
UString*    UHTTP::cbuffer;
UString*    UHTTP::geoip;
UString*    UHTTP::tmpdir;
UString*    UHTTP::keyID;
UString*    UHTTP::qcontent;
UString*    UHTTP::htpasswd;
UString*    UHTTP::htdigest;
UString*    UHTTP::pathname;
UString*    UHTTP::set_cookie;
UString*    UHTTP::request_uri;
UString*    UHTTP::global_alias;
UString*    UHTTP::fcgi_uri_mask;
UString*    UHTTP::scgi_uri_mask;
UString*    UHTTP::cookie_option;
UString*    UHTTP::cache_file_mask;
UString*    UHTTP::cache_file_store;
UString*    UHTTP::uri_protected_mask;
UString*    UHTTP::uri_request_cert_mask;
UString*    UHTTP::maintenance_mode_page;
UString*    UHTTP::string_HTTP_Variables;
UString*    UHTTP::uri_strict_transport_security_mask;
uint32_t    UHTTP::npathinfo;
uint32_t    UHTTP::range_size;
uint32_t    UHTTP::range_start;
uint32_t    UHTTP::sid_counter_gen;
uint32_t    UHTTP::sid_counter_cur;
uint32_t    UHTTP::limit_request_body = U_STRING_LIMIT;
uint32_t    UHTTP::request_read_timeout;
uint32_t    UHTTP::min_size_for_sendfile; // NB: for major size it is better to use sendfile()...
const char* UHTTP::ptrH;
const char* UHTTP::ptrC;
const char* UHTTP::ptrT;
const char* UHTTP::ptrR;
const char* UHTTP::ptrI;
const char* UHTTP::ptrA;
const char* UHTTP::ptrF;
const char* UHTTP::ptrK;
const char* UHTTP::ptrU;
const char* UHTTP::ptrP;
const char* UHTTP::ptrX;
const char* UHTTP::ptrS;

uint32_t                          UHTTP::upload_progress_index;
UDataSession*                     UHTTP::data_session;
UDataSession*                     UHTTP::data_storage;
UMimeMultipart*                   UHTTP::formMulti;
UVector<UString>*                 UHTTP::valias;
UVector<UString>*                 UHTTP::form_name_value;
UVector<UIPAllow*>*               UHTTP::vallow_IP;
UHTTP::upload_progress*           UHTTP::ptr_upload_progress;

         UHTTP::UFileCacheData*   UHTTP::file_data;
UHashMap<UHTTP::UFileCacheData*>* UHTTP::cache_file;
#ifdef USE_PAGE_SPEED
UHTTP::UPageSpeed*                UHTTP::page_speed;
#endif
#ifdef USE_LIBV8
UHTTP::UV8JavaScript*             UHTTP::v8_javascript;
#endif

const UString* UHTTP::str_origin;
const UString* UHTTP::str_frm_body;
const UString* UHTTP::str_indexhtml;
const UString* UHTTP::str_ctype_tsa;
const UString* UHTTP::str_frm_header;
const UString* UHTTP::str_ctype_html;
const UString* UHTTP::str_ctype_soap;
const UString* UHTTP::str_ulib_header;
const UString* UHTTP::str_strict_transport_security;

void UHTTP::str_allocate()
{
   U_TRACE(0+256, "UHTTP::str_allocate()")

   U_INTERNAL_ASSERT_EQUALS(str_indexhtml,0)
   U_INTERNAL_ASSERT_EQUALS(str_ctype_tsa,0)
   U_INTERNAL_ASSERT_EQUALS(str_ctype_html,0)
   U_INTERNAL_ASSERT_EQUALS(str_ctype_soap,0)
   U_INTERNAL_ASSERT_EQUALS(str_frm_header,0)
   U_INTERNAL_ASSERT_EQUALS(str_frm_body,0)
   U_INTERNAL_ASSERT_EQUALS(str_origin,0)
   U_INTERNAL_ASSERT_EQUALS(str_ulib_header,0)
   U_INTERNAL_ASSERT_EQUALS(str_strict_transport_security,0)

   static ustringrep stringrep_storage[] = {
   { U_STRINGREP_FROM_CONSTANT("index.html") },
   { U_STRINGREP_FROM_CONSTANT("application/timestamp-reply\r\n") },
   { U_STRINGREP_FROM_CONSTANT(U_CTYPE_HTML U_CRLF) },
   { U_STRINGREP_FROM_CONSTANT("application/soap+xml; charset=\"utf-8\"\r\n") },
   { U_STRINGREP_FROM_CONSTANT("HTTP/1.%c %d %s\r\n"
                               "Server: ULib\r\n" // ULIB_VERSION "\r\n"
                               "%.*s"
                               "%.*s") },
   { U_STRINGREP_FROM_CONSTANT("<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\r\n"
                               "<html><head>\r\n"
                               "<title>%d %s</title>\r\n"
                               "</head><body>\r\n"
                               "<h1>%s</h1>\r\n"
                               "<p>%s</p>\r\n"
                               "<hr>\r\n"
                               "<address>ULib Server</address>\r\n"
                               "</body></html>\r\n") },
   { U_STRINGREP_FROM_CONSTANT("Origin") },
   { U_STRINGREP_FROM_CONSTANT("X-Powered-By: ULib/" ULIB_VERSION "\r\n") },
   { U_STRINGREP_FROM_CONSTANT("Strict-Transport-Security: max-age=31536000; includeSubDomains\r\n") }
   };

   U_NEW_ULIB_OBJECT(str_indexhtml,                 U_STRING_FROM_STRINGREP_STORAGE(0));
   U_NEW_ULIB_OBJECT(str_ctype_tsa,                 U_STRING_FROM_STRINGREP_STORAGE(1));
   U_NEW_ULIB_OBJECT(str_ctype_html,                U_STRING_FROM_STRINGREP_STORAGE(2));
   U_NEW_ULIB_OBJECT(str_ctype_soap,                U_STRING_FROM_STRINGREP_STORAGE(3));
   U_NEW_ULIB_OBJECT(str_frm_header,                U_STRING_FROM_STRINGREP_STORAGE(4));
   U_NEW_ULIB_OBJECT(str_frm_body,                  U_STRING_FROM_STRINGREP_STORAGE(5));
   U_NEW_ULIB_OBJECT(str_origin,                    U_STRING_FROM_STRINGREP_STORAGE(6));
   U_NEW_ULIB_OBJECT(str_ulib_header,               U_STRING_FROM_STRINGREP_STORAGE(7));
   U_NEW_ULIB_OBJECT(str_strict_transport_security, U_STRING_FROM_STRINGREP_STORAGE(8));
}

// HTML Pagination

uint32_t UHTTP::num_item_tot;
uint32_t UHTTP::num_page_cur;
uint32_t UHTTP::num_page_end;    // modified by getLinkPagination()...
uint32_t UHTTP::num_page_start;  // modified by getLinkPagination()...
uint32_t UHTTP::num_item_for_page;

UString UHTTP::getLinkPagination()
{
   U_TRACE(0, "UHTTP::getLinkPagination()")

   U_INTERNAL_DUMP("num_page_cur = %u num_item_tot = %u num_page_start = %u num_page_end = %u num_item_for_page = %u",
                    num_page_cur,     num_item_tot,     num_page_start,     num_page_end,     num_item_for_page)

   U_INTERNAL_ASSERT_MAJOR(num_item_for_page,0)

   UString link(U_CAPACITY);

   if (num_item_tot <= num_item_for_page)
      {
      num_page_end   =  num_item_tot;
      num_page_start = (num_item_tot > 0);

      (void) link.assign(U_CONSTANT_TO_PARAM("<span class=\"void\">PREV</span><span class=\"void\">NEXT</span>"));
      }
   else
      {
      uint32_t pagina_precedente =  num_page_cur - 1,
               pagina_successiva =  num_page_cur + 1,
               i, tot_pagine     = (num_item_tot / num_item_for_page);

      if ((num_item_tot % num_item_for_page) != 0) ++tot_pagine;

      uint32_t    ultima_pagina =    tot_pagine - 1,
               penultima_pagina = ultima_pagina - 1;

      // link to previous page

      if (num_page_cur == 1)
         {
         num_page_start = 1;

         (void) link.assign(U_CONSTANT_TO_PARAM("<span class=\"void\">PREV</span> "));
         }
      else
         {
         num_page_start = 1 + (pagina_precedente * num_item_for_page);

         link.snprintf("<a href=\"?page=%u\" class=\"pnum\">PREV</a> ", pagina_precedente);
         }

      // we always show the link to the first page

      addLinkPagination(link,1);

      // if the next link is to the second page, add the dots ... or only the missing page

      if (pagina_precedente > 2)
         {
         if (pagina_precedente == 3) addLinkPagination(link,2);
         else                 (void) link.append(U_CONSTANT_TO_PARAM(" ... "));
         }

      // we create links to the current page and those close to it

      for (i = pagina_precedente; (int32_t)i < (int32_t)(pagina_successiva+1); ++i) // NB: we need this (gcc: cannot optimize possibly infinite loops)
         {
         // check if there is among those near the first page (already reported)

         if (i < 2) continue;

         // check if there is among those near the last page (which will show with the next instruction)

         if (i > ultima_pagina) continue;

         addLinkPagination(link,i);
         }

      // if the previous link was not on the penultimate page, add the dots ... or only the missing page

      if (pagina_successiva < ultima_pagina)
         {
         if (pagina_successiva == penultima_pagina) addLinkPagination(link,ultima_pagina);
         else                                (void) link.append(U_CONSTANT_TO_PARAM(" ... "));
         }

      // show the link to the last page if it does not coincide with the first

      if (tot_pagine != 1) addLinkPagination(link,tot_pagine);

      // link to next page

      if (num_page_cur == tot_pagine)
         {
         num_page_end = num_item_tot;

         (void) link.append(U_CONSTANT_TO_PARAM("<span class=\"void\">NEXT</span>"));
         }
      else
         {
         num_page_end = num_page_start + num_item_for_page - 1;

         link.snprintf_add("<a href=\"?page=%u\" class=\"pnum\">NEXT</a>", pagina_successiva);
         }
      }

   U_INTERNAL_DUMP("num_page_cur = %u num_item_tot = %u num_page_start = %u num_page_end = %u num_item_for_page = %u",
                    num_page_cur,     num_item_tot,     num_page_start,     num_page_end,     num_item_for_page)

   U_RETURN_STRING(link);
}

// CACHE FILE SYSTEM

UHTTP::UFileCacheData::UFileCacheData()
{
   U_TRACE_REGISTER_OBJECT(0, UFileCacheData, "")

   U_INTERNAL_ASSERT_POINTER(file)

   ptr        = array = 0;
   size       = 0;
   mode       = 0;
   mtime      = 0;
   link       = false;
   expire     = U_TIME_FOR_EXPIRE;
   mime_index = wd = fd = -1;
}

UHTTP::UFileCacheData::~UFileCacheData()
{
   U_TRACE_UNREGISTER_OBJECT(0, UFileCacheData)

   U_INTERNAL_DUMP("ptr = %p array = %p link = %b", ptr, array, link)

   if (ptr &&
       link == false)
      {
           if (mime_index == U_usp) delete (UServletPage*)ptr;
#  ifdef HAVE_LIBTCC
      else if (mime_index == U_csp) delete (UCServletPage*)ptr;
#  endif
      else if (mime_index == U_cgi)
         {
         U_FREE_TYPE(ptr, UHTTP::ucgi);
         }
      else delete (UString*)ptr;
      }

   if (array) delete array;

#if defined(HAVE_SYS_INOTIFY_H) && defined(U_HTTP_INOTIFY_SUPPORT)
   if (UServer_Base::handler_inotify)
      {
      U_INTERNAL_ASSERT_MAJOR(UServer_Base::handler_inotify->fd,0)

      if (wd != -1 &&
          UServer_Base::isChild() == false)
         {
         (void) U_SYSCALL(inotify_rm_watch, "%d,%d", UServer_Base::handler_inotify->fd, wd);
         }
      }
#endif
}

// INOTIFY FOR CACHE FILE SYSTEM

#if defined(HAVE_SYS_INOTIFY_H) && defined(U_HTTP_INOTIFY_SUPPORT)
U_NO_EXPORT void UHTTP::getInotifyPathDirectory(UStringRep* key, void* value)
{
   U_TRACE(0+256, "UHTTP::getInotifyPathDirectory(%.*S,%p)", U_STRING_TO_TRACE(*key), value)

   file_data = (UFileCacheData*)value;

   U_INTERNAL_ASSERT_POINTER(file_data)

   if (file_data->wd == inotify_wd)
      {
      U_INTERNAL_ASSERT_POINTER(UHashMap<void*>::pkey)

      UHashMap<void*>::pkey->str     = key->str;
      UHashMap<void*>::pkey->_length = key->_length;

      cache_file->stopCallForAllEntry();
      }
}

U_NO_EXPORT void UHTTP::checkInotifyForCache(int wd, char* name, uint32_t len)
{
   U_TRACE(0, "UHTTP::checkInotifyForCache(%d,%.*S,%u)", wd, len, name, len)

   U_INTERNAL_ASSERT_POINTER(cache_file)

   if (wd        != inotify_wd ||
       file_data == 0          ||
       wd        != file_data->wd)
      {
      inotify_wd = wd;

      cache_file->callForAllEntry(getInotifyPathDirectory);
      }

   U_INTERNAL_ASSERT_POINTER(file_data)

   if (file_data->wd != wd) file_data = 0;
   else
      {
      U_INTERNAL_ASSERT_POINTER(UHashMap<void*>::pkey)

      char buffer[U_PATH_MAX];

      UHashMap<void*>::pkey->_length = u__snprintf(buffer, sizeof(buffer), "%.*s/%.*s", U_STRING_TO_TRACE(*UHashMap<void*>::pkey), len, name);
      UHashMap<void*>::pkey->str     = buffer;

      file_data = (*cache_file)[UHashMap<void*>::pkey];
      }
}
#endif

U_NO_EXPORT void UHTTP::in_CREATE()
{
   U_TRACE(0, "UHTTP::in_CREATE()")

   if (file_data == 0)
      {
      U_INTERNAL_ASSERT_POINTER(pathname)
      U_INTERNAL_ASSERT_POINTER(UHashMap<void*>::pkey)

      uint32_t sz = UHashMap<void*>::pkey->size();

      pathname->setBuffer(sz);

      (void) pathname->snprintf("%.*s", sz, UHashMap<void*>::pkey->data());

      checkFileForCache();
      }
}

U_NO_EXPORT void UHTTP::in_DELETE()
{
   U_TRACE(0, "UHTTP::in_DELETE()")

   if (file_data)
      {
      cache_file->eraseAfterFind();

      file_data = 0;
      }
}

#define IN_BUFLEN (1024 * (sizeof(struct inotify_event) + 16))

void UHTTP::in_READ()
{
   U_TRACE(1+256, "UHTTP::in_READ()")

   U_INTERNAL_ASSERT_POINTER(UServer_Base::handler_inotify)

#if defined(HAVE_SYS_INOTIFY_H) && defined(U_HTTP_INOTIFY_SUPPORT)
   /*
   struct inotify_event {
      int wd;           // The watch descriptor
      uint32_t mask;    // Watch mask
      uint32_t cookie;  // A cookie to tie two events together
      uint32_t len;     // The length of the filename found in the name field
      char name[];      // The name of the file, padding to the end with NULs
   }
   */

   union uuinotify_event {
                      char*  p;
      struct inotify_event* ip;
   };

   uint32_t len;
   char buffer[IN_BUFLEN];
   union uuinotify_event event;
   int i = 0, length = U_SYSCALL(read, "%d,%p,%u", UServer_Base::handler_inotify->fd, buffer, IN_BUFLEN);  

   while (i < length)
      {
      event.p = buffer+i;

      if (event.ip->len)
         {
         U_INTERNAL_DUMP("The %s %s(%u) was %s", (event.ip->mask & IN_ISDIR  ? "directory" : "file"), event.ip->name, event.ip->len,
                                                 (event.ip->mask & IN_CREATE ? "created"   :
                                                  event.ip->mask & IN_DELETE ? "deleted"   :
                                                  event.ip->mask & IN_MODIFY ? "modified"  : "???"))

         // NB: The length contains any potential padding that is, the result of strlen() on the name field may be smaller than len...

         for (len = event.ip->len; event.ip->name[len-1] == '\0'; --len) {}

         U_INTERNAL_ASSERT_EQUALS(len, u__strlen(event.ip->name, __PRETTY_FUNCTION__))

         checkInotifyForCache(event.ip->wd, event.ip->name, len);

         if (event.ip->mask & IN_CREATE) in_CREATE();
         else
            {
                 if (event.ip->mask & IN_DELETE) in_DELETE();
            else if (event.ip->mask & IN_MODIFY)
               {
               if (file_data)
                  {
                  // NB: check if we have the content of file in cache...

                  if (isDataFromCache()) file_data->expire = 0; // NB: we delay the renew...
                  else                   renewDataCache();
                  }
               }
            }
         }

      i += sizeof(struct inotify_event) + event.ip->len;
      }
#endif
}

// CSP (C Servlet Page)

#ifdef HAVE_LIBTCC
static char*        get_reply(void)                    { return UClientImage_Base::wbuffer->data(); }
static unsigned int get_reply_capacity(void)           { return UClientImage_Base::wbuffer->capacity(); }
static void         set_reply_capacity(unsigned int n) {        UClientImage_Base::wbuffer->setBuffer(n); }
#  ifdef USE_LIBV8
static char* runv8(const char* jssrc) // compiles and executes javascript and returns the script return value as string
   {
   if (UHTTP::v8_javascript)
      {
      UString x(jssrc);

      UHTTP::v8_javascript->runv8(x);

      return x.c_strdup();
      }

   return 0;
   }
#  endif
#endif

bool UHTTP::UCServletPage::compile(const UString& program)
{
   U_TRACE(1, "UCServletPage::compile(%.*S)", U_STRING_TO_TRACE(program))

   bool result = false;
#ifdef HAVE_LIBTCC
   TCCState* s = (TCCState*) U_SYSCALL_NO_PARAM(tcc_new);

   (void) U_SYSCALL(tcc_set_output_type, "%p,%d", s, TCC_OUTPUT_MEMORY);

   const char* ptr = program.c_str();

   if (U_SYSCALL(tcc_compile_string, "%p,%S", s, ptr) != -1)
      {
      /* we add a symbol that the compiled program can use */

      (void) U_SYSCALL(tcc_add_symbol, "%p,%S,%p", s, "get_reply",          (void*)get_reply);
      (void) U_SYSCALL(tcc_add_symbol, "%p,%S,%p", s, "get_reply_capacity", (void*)get_reply_capacity);
      (void) U_SYSCALL(tcc_add_symbol, "%p,%S,%p", s, "set_reply_capacity", (void*)set_reply_capacity);
#  ifdef USE_LIBV8
      (void) U_SYSCALL(tcc_add_symbol, "%p,%S,%p", s, "runv8",              (void*)runv8);
#  endif

      /* define preprocessor symbol 'sym'. Can put optional value */

      U_SYSCALL_VOID(tcc_define_symbol, "%p,%S,%S", s, "HAVE_CONFIG_H", 0);

      /* You may also open a dll with tcc_add_file() and use symbols from that */

#  ifdef DEBUG
      (void) U_SYSCALL(tcc_add_file, "%p,%S,%d", s, U_PREFIXDIR "/lib/libulib_g.so");
#  else
      (void) U_SYSCALL(tcc_add_file, "%p,%S,%d", s, U_PREFIXDIR "/lib/libulib.so");
#  endif

      int rc;
      UString token;
      uint32_t pos = 0;
      UTokenizer t(program);
      char buffer[U_PATH_MAX];

      while ((pos = U_STRING_FIND(program, pos, "#pragma ")) != U_NOT_FOUND)
         {
         pos += U_CONSTANT_SIZE("#pragma ");

         t.setDistance(pos);

         if (t.next(token, (bool*)0) == false) break;

         if (token == "link ")
            {
            if (t.next(token, (bool*)0) == false) break;

            pos += U_CONSTANT_SIZE("link ");

            if (token.first_char() != '/')
               {
               (void) snprintf(buffer, U_PATH_MAX, "../libraries/%.*s", U_STRING_TO_TRACE(token));

               if (UFile::access(buffer, R_OK))
                  {
                  rc = U_SYSCALL(tcc_add_file, "%p,%S,%d", s, buffer);

                  if (rc != -1) continue;
                  }
               }

            (void) U_SYSCALL(tcc_add_file, "%p,%S,%d", s, token.c_str());
            }
         else if (token == "include ")
            {
            if (t.next(token, (bool*)0) == false) break;

            pos += U_CONSTANT_SIZE("include ");

            if (token.first_char() != '/')
               {
               (void) snprintf(buffer, U_PATH_MAX, "../include/%.*s", U_STRING_TO_TRACE(token));

               if (UFile::access(buffer, R_OK | X_OK))
                  {
                  rc = U_SYSCALL(tcc_add_include_path, "%p,%S,%d", s, buffer);

                  if (rc != -1) continue;
                  }
               }

            (void) U_SYSCALL(tcc_add_include_path, "%p,%S,%d", s, token.c_str());
            }
         }

      size = U_SYSCALL(tcc_relocate, "%p,%p", s, 0);

      if (size > 0)
         {
         relocated = UMemoryPool::_malloc((uint32_t*)&size);

         (void) U_SYSCALL(tcc_relocate, "%p,%p", s, relocated);

         prog_main = (iPFipvc) U_SYSCALL(tcc_get_symbol, "%p,%S", s, "main");

         if (prog_main) result = true;
         }
      }

   U_SYSCALL_VOID(tcc_delete, "%p", s);
#endif

   U_RETURN(result);
}

void UHTTP::ctor()
{
   U_TRACE(1, "UHTTP::ctor()")

               str_allocate();
   UWebSocket::str_allocate();

   if (        Url::str_ftp  == 0)         Url::str_allocate();
   if (UMimeHeader::str_name == 0) UMimeHeader::str_allocate();

   U_INTERNAL_ASSERT_EQUALS(uri,0)
   U_INTERNAL_ASSERT_EQUALS(file,0)
   U_INTERNAL_ASSERT_EQUALS(alias,0)
   U_INTERNAL_ASSERT_EQUALS(geoip,0)
   U_INTERNAL_ASSERT_EQUALS(tmpdir,0)
   U_INTERNAL_ASSERT_EQUALS(cbuffer,0)
   U_INTERNAL_ASSERT_EQUALS(qcontent,0)
   U_INTERNAL_ASSERT_EQUALS(pathname,0)
   U_INTERNAL_ASSERT_EQUALS(formMulti,0)
   U_INTERNAL_ASSERT_EQUALS(set_cookie,0)
   U_INTERNAL_ASSERT_EQUALS(request_uri,0)
   U_INTERNAL_ASSERT_EQUALS(form_name_value,0)

   uri             = U_NEW(UString);
   file            = U_NEW(UFile);
   keyID           = U_NEW(UString);
   alias           = U_NEW(UString);
   geoip           = U_NEW(UString(U_CAPACITY));
   tmpdir          = U_NEW(UString(U_PATH_MAX));
   cbuffer         = U_NEW(UString);
   qcontent        = U_NEW(UString);
   pathname        = U_NEW(UString);
   formMulti       = U_NEW(UMimeMultipart);
   set_cookie      = U_NEW(UString);
   request_uri     = U_NEW(UString);
   form_name_value = U_NEW(UVector<UString>);

   if (cookie_option   == 0) cookie_option   = U_NEW(U_STRING_FROM_CONSTANT("[\"\" 0]"));
   if (cache_file_mask == 0) cache_file_mask = U_NEW(U_STRING_FROM_CONSTANT("*.css|*.js|*.*html|*.png|*.gif|*.jpg|*.shtml"));

   U_INTERNAL_ASSERT_POINTER(USocket::str_host)
   U_INTERNAL_ASSERT_POINTER(USocket::str_range)
   U_INTERNAL_ASSERT_POINTER(USocket::str_accept)
   U_INTERNAL_ASSERT_POINTER(USocket::str_cookie)
   U_INTERNAL_ASSERT_POINTER(USocket::str_connection)
   U_INTERNAL_ASSERT_POINTER(USocket::str_content_type)
   U_INTERNAL_ASSERT_POINTER(USocket::str_content_length)
   U_INTERNAL_ASSERT_POINTER(USocket::str_if_modified_since)

   ptrH =    USocket::str_host->c_pointer(1);              // "Host"
   ptrR =    USocket::str_range->c_pointer(1);             // "Range"
   ptrA =    USocket::str_accept->c_pointer(1);            // "Accept"
   ptrK =    USocket::str_cookie->c_pointer(1);            // "Cookie"
   ptrF =    USocket::str_referer->c_pointer(1);           // "Referer"
   ptrP =    USocket::str_X_Real_IP->c_pointer(1);         // "X-Real-IP"
   ptrU =    USocket::str_user_agent->c_pointer(1);        // "User-Agent"
   ptrC =    USocket::str_connection->c_pointer(1);        // "Connection"
   ptrT =    USocket::str_content_type->c_pointer(1);      // "Content-"
   ptrS = UWebSocket::str_websocket_key->c_pointer(1);     // "Sec-WebSocket-Key"
   ptrX =    USocket::str_X_Forwarded_For->c_pointer(1);   // "X-Forwarded-For"
   ptrI =    USocket::str_if_modified_since->c_pointer(1); // "If-Modified-Since"

#ifdef USE_LIBMAGIC
   (void) UMagic::init();
#endif

#ifdef USE_LIBSSL
   if (UServer_Base::bssl) enable_caching_by_proxy_servers = true;
#endif

#if defined(USE_PAGE_SPEED) || defined(USE_LIBV8)
   char buffer[U_PATH_MAX];
#endif

#ifdef USE_PAGE_SPEED
   U_INTERNAL_ASSERT_EQUALS(page_speed, 0)

   (void) snprintf(buffer, U_PATH_MAX, U_FMT_LIBPATH, U_PATH_CONV(UPlugIn<void*>::plugin_dir), U_CONSTANT_TO_TRACE("mod_pagespeed"));

   page_speed = U_NEW(UHTTP::UPageSpeed);

   if (page_speed->UDynamic::load(buffer))
      {
      page_speed->minify_html  = (vPFpcstr) (*page_speed)["minify_html"];
      page_speed->optimize_gif = (vPFstr)   (*page_speed)["optimize_gif"];
      page_speed->optimize_png = (vPFstr)   (*page_speed)["optimize_png"];
      page_speed->optimize_jpg = (vPFstr)   (*page_speed)["optimize_jpg"];

      U_INTERNAL_ASSERT_POINTER(page_speed->minify_html)
      U_INTERNAL_ASSERT_POINTER(page_speed->optimize_gif)
      U_INTERNAL_ASSERT_POINTER(page_speed->optimize_png)
      U_INTERNAL_ASSERT_POINTER(page_speed->optimize_jpg)

      U_SRV_LOG("Load of plugin 'mod_pagespeed' success");
      }
   else
      {
      U_SRV_LOG("Load of plugin 'mod_pagespeed' FAILED");

      delete page_speed;
             page_speed = 0;
      }

   U_INTERNAL_ASSERT_POINTER(page_speed)
#endif

#ifdef USE_LIBV8
   U_INTERNAL_ASSERT_EQUALS(v8_javascript, 0)

   (void) snprintf(buffer, U_PATH_MAX, U_FMT_LIBPATH, U_PATH_CONV(UPlugIn<void*>::plugin_dir), U_CONSTANT_TO_TRACE("mod_v8"));

   v8_javascript = U_NEW(UHTTP::UV8JavaScript);

   if (v8_javascript->UDynamic::load(buffer))
      {
      v8_javascript->runv8 = (vPFstr) (*v8_javascript)["runv8"];

      U_INTERNAL_ASSERT_POINTER(v8_javascript->runv8)

      U_SRV_LOG("Load of plugin 'mod_v8' success");
      }
   else
      {
      U_SRV_LOG("Load of plugin 'mod_v8' FAILED");

      delete v8_javascript;
             v8_javascript = 0;
      }

   U_INTERNAL_ASSERT_POINTER(v8_javascript)
#endif

   if (virtual_host) U_SRV_LOG("Virtual host service enabled");

   if (min_size_for_sendfile == 0) min_size_for_sendfile = 10 * 1024 * 1024; // 10M

   U_INTERNAL_DUMP("min_size_for_sendfile = %u", min_size_for_sendfile)

   UServices::generateKey(); // For ULIB facility request TODO session cookies... 

   /*
   Set up static environment variables

   server static variable  Description
   -------------------------------------------------------------------------------------------------------------------------------------------
   SERVER_PORT
   SERVER_ADDR
   SERVER_NAME       - Server's hostname, DNS alias, or IP address as it appears in self-referencing URLs.
   DOCUMENT_ROOT     - The root directory of your server.
   SERVER_SOFTWARE   - Name and version of the information server software answering the request (and running the gateway). Format: name/version.
   GATEWAY_INTERFACE - CGI specification revision with which this server complies. Format: CGI/revision.

   Example:
   ----------------------------------------------------------------------------------------------------------------------------
   SERVER_PORT=80
   SERVER_ADDR=127.0.0.1
   SERVER_NAME=localhost
   DOCUMENT_ROOT="/var/www/localhost/htdocs"
   SERVER_SOFTWARE=Apache
   GATEWAY_INTERFACE=CGI/1.1
   ----------------------------------------------------------------------------------------------------------------------------
   */

   U_INTERNAL_ASSERT_POINTER(UServer_Base::senvironment)

   UString name = USocketExt::getNodeName(), ip_server = UServer_Base::getIPAddress();

   UServer_Base::senvironment->snprintf_add("SERVER_NAME=%.*s\n"  // Your server's fully qualified domain name (e.g. www.cgi101.com)
                                            "SERVER_ADDR=%.*s\n"
                                            "SERVER_PORT=%d\n"    // The port number your server is listening on
                                            "DOCUMENT_ROOT=%w\n", // The root directory of your server
                                            U_STRING_TO_TRACE(name),
                                            U_STRING_TO_TRACE(ip_server),
                                            UServer_Base::port);

   (void) UServer_Base::senvironment->append(U_CONSTANT_TO_PARAM(
                                             "GATEWAY_INTERFACE=CGI/1.1\n"
                                             "SERVER_SOFTWARE=" PACKAGE_NAME "/" ULIB_VERSION "\n" // The server software you're using (such as Apache 1.3)
                                             "PATH=/usr/local/bin:/usr/bin:/bin\n"                 // The system path your server is running under
                                             "REDIRECT_STATUS=200\n"));

   U_ASSERT_EQUALS(UServer_Base::senvironment->isBinary(), false)

   setCacheForDocumentRoot();
}

#ifdef DEBUG
U_NO_EXPORT void UHTTP::check_memory(UStringRep* key, void* value)
{
   U_TRACE(0, "UHTTP::check_memory(%.*S,%p)", U_STRING_TO_TRACE(*key), value)

   U_INTERNAL_ASSERT_POINTER(value)

   UFileCacheData* cptr = (UFileCacheData*)value;

   if (cptr->array) (void) cptr->array->check_memory();
}

bool UHTTP::cache_file_check_memory()
{
   U_TRACE(0, "UHTTP::cache_file_check_memory()")

   U_INTERNAL_ASSERT_POINTER(cache_file)

   cache_file->callForAllEntry(check_memory);

   U_RETURN(true);
}
#endif

void UHTTP::checkFileForCache()
{
   U_TRACE(0+256, "UHTTP::checkFileForCache()")

   U_INTERNAL_ASSERT_POINTER(pathname)

   file->setPath(*pathname);

   // NB: file->stat() get also the size of file...

   if (file->stat()) manageDataForCache();
}

void UHTTP::setCacheForDocumentRoot()
{
   U_TRACE(1, "UHTTP::setCacheForDocumentRoot()")

#if defined(HAVE_SYS_INOTIFY_H) && defined(U_HTTP_INOTIFY_SUPPORT)
   if (UServer_Base::handler_inotify)
      {
      // INIT INOTIFY FOR DOCUMENT ROOT CACHE

#  ifdef HAVE_INOTIFY_INIT1
      UServer_Base::handler_inotify->fd = U_SYSCALL(inotify_init1, "%d", IN_NONBLOCK | IN_CLOEXEC);

      if (UServer_Base::handler_inotify->fd != -1 || errno != ENOSYS) goto next;
#  endif

      UServer_Base::handler_inotify->fd = U_SYSCALL_NO_PARAM(inotify_init);

      (void) U_SYSCALL(fcntl, "%d,%d,%d", UServer_Base::handler_inotify->fd, F_SETFL, O_NONBLOCK | O_CLOEXEC);
next:
      if (UServer_Base::handler_inotify->fd != -1)
         {
         U_SRV_LOG("Inode based directory notification enabled");
         }
      else
         {
         UServer_Base::handler_inotify = 0;

         U_WARNING("Inode based directory notification failed");
         }
      }
#else
   UServer_Base::handler_inotify = 0;
#endif

   U_INTERNAL_ASSERT_EQUALS(cache_file, 0)

   cache_file = U_NEW(UHashMap<UFileCacheData*>);

   if (cache_file_store)
      {
      UString content = UFile::contentOf(*cache_file_store);

      if (content.empty()) goto load;

      if (u_endsWith(U_STRING_TO_PARAM(*cache_file_store), U_CONSTANT_TO_PARAM(".gz"))) content = UStringExt::gunzip(content);

      cache_file->allocate();

      UString2Object(U_STRING_TO_PARAM(content), *cache_file);

      U_ASSERT_MAJOR(cache_file->size(), 0)

      nostat = u_startsWith(U_STRING_TO_PARAM(*cache_file_store), U_CONSTANT_TO_PARAM("nostat"));

      U_SRV_LOG("Loaded cache file store: %.*S - nostat %s enabled", U_STRING_TO_TRACE(*cache_file_store), nostat ? "is" : "NOT");
      }
   else
      {
load:
      if (U_STRNEQ(cache_file_mask->data(), "_off_")) cache_file->allocate();
      else
         {
         UString item;
         UDirWalk dirwalk(0); // U_CONSTANT_TO_PARAM("[cu]sp"));
                              // u_setPfnMatch(U_FNMATCH, FNM_INVERT);

         UVector<UString> vec(4000);
         UDirWalk::setRecurseSubDirs();

         uint32_t i, n = dirwalk.walk(vec);

         cache_file->allocate(n + (15 * (n / 100)) + 32);

         for (i = 0; i < n; ++i)
            {
            item = vec[i];

            U_INTERNAL_ASSERT_POINTER(pathname)

            (void) pathname->replace(item);

            checkFileForCache();
            }
         }
      }

   // manage favicon...

   file_data = (*cache_file)["favicon.ico"];

   if (file_data &&
       file_data->array == 0)
      {
      (void) pathname->replace(U_CONSTANT_TO_PARAM("favicon.ico"));

      file->setPath(*pathname);

      U_INTERNAL_ASSERT(file->stat())

      UString content = file->getContent();

      u_mime_index = U_ico;

      putDataInCache(getHeaderMimeType(0, U_CTYPE_ICO, 0, U_TIME_FOR_EXPIRE), content);

      U_INTERNAL_ASSERT_POINTER(file_data->array)

      U_ASSERT(file_data->array->check_memory())
      }

   // manage global alias...

   if (global_alias &&
       virtual_host == false)
      {
      file_data = (*cache_file)[*global_alias];

      if (file_data &&
          file_data->array == 0)
         {
         (void) pathname->replace(*global_alias);

         file->setPath(*pathname);

         U_INTERNAL_ASSERT(file->stat())

         UString content = file->getContent();

         u_mime_index = U_ssi;

         putDataInCache(getHeaderMimeType(0, U_CTYPE_HTML, 0, 0), content);

         U_INTERNAL_ASSERT_POINTER(file_data->array)
         U_INTERNAL_ASSERT_EQUALS( file_data->array->size(), 2)

         U_ASSERT(file_data->array->check_memory())
         }
      }

   // set fd_max limit...

   uint32_t sz = cache_file->size();

   U_INTERNAL_DUMP("cache size = %u", sz)

#ifndef __MINGW32__
   uint32_t rlim = (sz + UNotifier::max_connection + 100);

   U_INTERNAL_DUMP("rlim = %u", rlim)

   if (rlim > 1024)
      {
      struct rlimit nofile = {
         rlim, /* Soft limit */
         rlim  /* Hard limit (ceiling for rlim_cur) */
      };

      if (U_SYSCALL(setrlimit, "%d,%p", RLIMIT_NOFILE, &nofile) == 0)
         {
         U_SRV_LOG("Updated program fd_max: %u", rlim);
         }
      }
#endif

   // manage authorization data...

   file_data = (*cache_file)[".htpasswd"];

   if (file_data)
      {
      U_INTERNAL_ASSERT_EQUALS(file_data->array, 0)

      htpasswd = U_NEW(UString(UFile::contentOf(".htpasswd")));

      U_SRV_LOG("File data users permission: '.htpasswd' loaded");
      }

   file_data = (*cache_file)[".htdigest"];

   if (file_data)
      {
      U_INTERNAL_ASSERT_EQUALS(file_data->array, 0)

      htdigest = U_NEW(UString(UFile::contentOf(".htdigest")));

      U_SRV_LOG("File data users permission: '.htdigest' loaded");
      }
}

void UHTTP::dtor()
{
   U_TRACE(0, "UHTTP::dtor()")

   if (valias)                             delete valias;
   if (global_alias)                       delete global_alias;
   if (cookie_option)                      delete cookie_option;
   if (cache_file_mask)                    delete cache_file_mask;
   if (cache_file_store)                   delete cache_file_store;
   if (uri_protected_mask)                 delete uri_protected_mask;
   if (uri_request_cert_mask)              delete uri_request_cert_mask;
   if (maintenance_mode_page)              delete maintenance_mode_page;
   if (string_HTTP_Variables)              delete string_HTTP_Variables;
   if (uri_strict_transport_security_mask) delete uri_strict_transport_security_mask;

   if (file)
      {
      // CACHE DOCUMENT ROOT

      U_ASSERT(cache_file_check_memory())

      /*
      if (cache_file_store)
         {
         char buffer[2 * 1024 * 1024];

         (void) UFile::writeToTmpl("/tmp/doc_root_cache", buffer, UObject2String(*cache_file, buffer, sizeof(buffer)));
         }
      */

      file_data = 0;

             cache_file->clear();
             cache_file->deallocate();
      delete cache_file;

      delete uri;
      delete file;
      delete keyID;
      delete alias;
      delete geoip;
      delete tmpdir;
      delete cbuffer;
      delete qcontent;
      delete pathname;
      delete formMulti;
      delete set_cookie;
      delete request_uri;
      delete form_name_value;

      if (htpasswd)     delete htpasswd;
      if (htdigest)     delete htdigest;
      if (vallow_IP)    delete vallow_IP;
      if (vRewriteRule) delete vRewriteRule;

      if (db_session) deleteSession();

#  ifdef USE_PAGE_SPEED
      if (page_speed) delete page_speed;
#  endif
#  ifdef USE_LIBV8
      if (v8_javascript) delete v8_javascript;
#  endif

#  if defined(HAVE_SYS_INOTIFY_H) && defined(U_HTTP_INOTIFY_SUPPORT)
      if (UServer_Base::handler_inotify)
         {
         // inotify: Inode based directory notification...

         U_INTERNAL_ASSERT_MAJOR(UServer_Base::handler_inotify->fd,0)

         (void) U_SYSCALL(close, "%d", UServer_Base::handler_inotify->fd);

         UServer_Base::handler_inotify = 0;
         }
#  endif
      }
}

UString UHTTP::getDocumentName()
{
   U_TRACE(0, "UHTTP::getDocumentName()")

   U_INTERNAL_ASSERT_POINTER(file)

   UString document = UStringExt::basename(file->getPath());

   U_RETURN_STRING(document);
}

UString UHTTP::getDirectoryURI()
{
   U_TRACE(0, "UHTTP::getDirectoryURI()")

   U_INTERNAL_ASSERT_POINTER(file)

   UString directory = UStringExt::dirname(file->getPath());

   U_RETURN_STRING(directory);
}

UString UHTTP::getRequestURI()
{
   U_TRACE(0, "UHTTP::getRequestURI()")

   U_INTERNAL_ASSERT_POINTER(uri)
   U_INTERNAL_ASSERT_POINTER(request_uri)

   static uint32_t old_counter;

   if (old_counter != UClientImage_Base::counter)
      {
       old_counter  = UClientImage_Base::counter;

      // NB: there may be an ALIAS...

      if (request_uri->empty()) (void) uri->assign(U_HTTP_URI_TO_PARAM);
      else                            *uri = *request_uri;
      }

   U_RETURN_STRING(*uri);
}

UString UHTTP::getRequestURIWithQuery()
{
   U_TRACE(0, "UHTTP::getRequestURIWithQuery()")

   UString result;

   // NB: there may be an ALIAS...

   if (request_uri->empty())
      {
      if (u_http_info.query_len == 0) result = getRequestURI();
      else                     (void) result.assign(U_HTTP_URI_QUERY_TO_PARAM);
      }
   else
      {
      result = getRequestURI();

      if (u_http_info.query_len)
         {
         result.push_back('?');

         (void) result.append(U_HTTP_QUERY_TO_PARAM);
         }
      }

   U_RETURN_STRING(result);
}

uint32_t UHTTP::getUserAgent()
{
   U_TRACE(0, "UHTTP::getUserAgent()")

   static uint32_t agent;
   static uint32_t old_counter;

   if (old_counter != UClientImage_Base::counter)
      {
       old_counter  = UClientImage_Base::counter;

      agent = (u_http_info.user_agent_len ? u_cdb_hash((unsigned char*)U_HTTP_USER_AGENT_TO_PARAM, false) : 0);
      }

   U_RETURN(agent);
}

__pure bool UHTTP::isMobile()
{
   U_TRACE(0, "UHTTP::isMobile()")

   if (u_http_info.user_agent_len &&
       u_dosmatch_with_OR(U_HTTP_USER_AGENT_TO_PARAM,
                          U_CONSTANT_TO_PARAM("*android*|"
                                              "*iphone*|*ipod*|"
                                              "*windows ce*|*windows phone*|"
                                              "*blackberry*|*palm*|*opera mini*|"
                                              "*webkit*series60*|*webkit*symbian*"), FNM_IGNORECASE))
      {
      U_RETURN(true);
      }

   U_RETURN(false);
}

void UHTTP::writeApacheLikeLog(bool bprepare)
{
   U_TRACE(0, "UHTTP::writeApacheLikeLog(%b)", bprepare)

   static char buffer1[64];
   static char buffer2[16];

   static iovec iov[7] = {
      { (caddr_t) buffer1, 0 },
      { (caddr_t)       0, 0 }, // request
      { (caddr_t) buffer2, 0 }, // response_code, body_len
      { (caddr_t)       0, 0 }, // referer
      { (caddr_t) U_CONSTANT_TO_PARAM("\" \"") },
      { (caddr_t)       0, 0 }, // agent
      { (caddr_t) U_CONSTANT_TO_PARAM("\"\n") }
   };

   /* Example
   ------------------------------------------------------------------------------------------------------------------------------------------------- 
    10.10.25.2 - - [21/May/2012:16:29:41 +0200] "GET / HTTP/1.1" 200 598 "-" "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/536.5 (KHTML, like Gecko)"
    10.10.25.2 - - [21/May/2012:16:29:41 +0200] "GET /unirel_logo.gif HTTP/1.1" 200 3414 "http://www.unirel.com/" "Mozilla/5.0 (X11; Linux x86_64)"
   ------------------------------------------------------------------------------------------------------------------------------------------------- 
   */

   if (bprepare)
      {
      uint32_t request_len;
      const char* request = UClientImage_Base::request->data();

      if (u_http_info.startHeader)
         {
         U_INTERNAL_DUMP("u_http_info.startHeader = %u", u_http_info.startHeader)

         request_len = u_http_info.startHeader;

         if (u__isspace(request[request_len]) == false &&
             u__isspace(request[request_len-1]))
            {
            while (u__isspace(request[--request_len])) {}

            ++request_len;
            }
         }
      else
         {
         request_len = U_STRING_FIND(*UClientImage_Base::request, 0, "HTTP/");

         U_INTERNAL_DUMP("request_len = %u", request_len)

         if (request_len != U_NOT_FOUND) request_len += U_CONSTANT_SIZE("HTTP/1.1");
         else
            {
            request     = "-";
            request_len = 1;
            }
         }

      U_INTERNAL_DUMP("request(%u) = %.*S", request_len, request_len, request)

      while (u__isspace(*request))
         {
         ++request;
         --request_len;
         }

      iov[1].iov_base = (caddr_t) request;
      iov[1].iov_len  = U_min(1000, request_len);

      if (u_http_info.referer_len)
         {
         iov[3].iov_base = (caddr_t) u_http_info.referer;
         iov[3].iov_len  = U_min(1000, u_http_info.referer_len);
         }
      else
         {
         iov[3].iov_base = (caddr_t) "-";
         iov[3].iov_len  = 1;
         }

      if (u_http_info.user_agent_len)
         {
         iov[5].iov_base = (caddr_t) u_http_info.user_agent;
         iov[5].iov_len  = U_min(1000, u_http_info.user_agent_len);
         }
      else
         {
         iov[5].iov_base = (caddr_t) "-";
         iov[5].iov_len  = 1;
         }
      }
   else
      {
      int fd            = apache_like_log->getFd();
      uint32_t body_len = UClientImage_Base::body->size();

      iov[0].iov_len = u__snprintf(buffer1, sizeof(buffer1), "%s - - [%11D] \"", UServer_Base::client_address);

      iov[2].iov_len = (body_len == 0 ? u__snprintf(buffer2, sizeof(buffer2), "\" %u - \"",  u_http_info.nResponseCode)
                                      : u__snprintf(buffer2, sizeof(buffer2), "\" %u %u \"", u_http_info.nResponseCode, body_len));

      if (u_http_info.nResponseCode == HTTP_NO_RESPONSE)
         {
         iov[1].iov_base = iov[3].iov_base = iov[5].iov_base = (caddr_t) "-";
         iov[1].iov_len  = iov[3].iov_len  = iov[5].iov_len  = 1;
         }

      (void) U_SYSCALL(writev, "%d,%p,%d", fd, iov, 7);
      }
}

/* read HTTP message
======================================================================================
Read the request line and attached headers. A typical http request will take the form:
======================================================================================
GET / HTTP/1.1
Host: 127.0.0.1
User-Agent: Mozilla/5.0 (compatible; Konqueror/3.1; Linux; it, en_US, en)
Accept-Encoding: gzip, deflate
Accept-Charset: iso-8859-1, utf-8;q=0.5, *;q=0.5
Accept-Language: it, en
Connection: Keep-Alive
<empty line>
======================================================================================
Read the result line and attached headers. A typical http response will take the form:
======================================================================================
HTTP/1.1 200 OK
Date: Wed, 25 Oct 2000 16:54:02 GMT
Age: 57718
Server: Apache/1.3b5
Last-Modified: Sat, 23 Sep 2000 15:00:57 GMT
Accept-Ranges: bytes
Etag: "122b12-2d8c-39ccc5a9"
Content-Type: text/html
Content-Length: 11660
<empty line>
.......<the data>
====================================================================================
*/

__pure int UHTTP::isValidRequest(const char* ptr)
{
   U_TRACE(0, "UHTTP::isValidRequest(%.*S)", 30, ptr)

   while (u__isspace(*ptr)) ++ptr; // skip space...

   unsigned char c = u__toupper(ptr[0]);

   if (c != 'G' && // GET
       c != 'P' && // POST/PUT
       c != 'D' && // DELETE
       c != 'H' && // HEAD
       c != 'C' && // COPY
       c != 'O')   // OPTIONS
      {
      U_RETURN(0);
      }

   if (c == 'G') // GET
      {
      if (U_STRNCASECMP(ptr, "GET ")) U_RETURN(false);

      U_RETURN(HTTP_GET);
      }

   if (c == 'P') // POST/PUT
      {
      if (U_STRNCASECMP(ptr, "POST ") == 0) U_RETURN(HTTP_POST);
      if (U_STRNCASECMP(ptr, "PUT ")  == 0) U_RETURN(HTTP_PUT);

      U_RETURN(0);
      }

   if (c == 'D') // DELETE
      {
      if (U_STRNCASECMP(ptr, "DELETE ")) U_RETURN(0);

      U_RETURN(HTTP_DELETE);
      }

   if (c == 'H') // HEAD
      {
      if (U_STRNCASECMP(ptr, "HEAD ")) U_RETURN(0);

      U_RETURN(HTTP_HEAD);
      }

   if (c == 'C') // COPY
      {
      if (U_STRNCASECMP(ptr, "COPY ")) U_RETURN(0);

      U_RETURN(HTTP_COPY);
      }

   // OPTIONS

   if (U_STRNCASECMP(ptr, "OPTIONS ")) U_RETURN(0);

   U_RETURN(HTTP_OPTIONS);
}

bool UHTTP::scanfHeader(const char* ptr, uint32_t size)
{
   U_TRACE(0, "UHTTP::scanfHeader(%.*S,%u)", size, ptr, size)

   /**
    * Check if HTTP response or request
    *
    * The default is GET for input requests and POST for output requests
    *
    * Other possible alternatives are:
    *
    *  - PUT
    *  - HEAD
    *  - DELETE
    *  - COPY
    *  - OPTIONS
    *
    *  ---- NOT implemented -----
    *
    *  - PATCH
    *  - CONNECT
    *  - TRACE (because can send client cookie information, dangerous...)
    *
    *  --------------------------
    *
    * See http://ietf.org/rfc/rfc2616.txt for further information about HTTP request methods
    **/

   unsigned char c   = *ptr;
   const char* start =  ptr;

   if (c != 'G')
      {
      // RFC 2616 4.1 "servers SHOULD ignore any empty line(s) received where a Request-Line is expected"

      if (u__isspace(c)) while (u__isspace((c = *++ptr))) {}

      // DELETE
      // GET
      // HEAD or response
      // POST/PUT
      // COPY
      // OPTIONS

      static const unsigned char is_req[] = {
         0,   0,   0,   0,   0,   0,   0,   0,
         0,   0,   0,   0,   0,   0,   0,   0,
         0,   0,   0,   0,   0,   0,   0,   0,
         0,   0,   0,   0,   0,   0,   0,   0, // 24      25      26      27      28      29      30      31 
         0,   0,   0,   0,   0,   0,   0,   0, // ' ',    '!',    '\"',   '#',    '$',    '%',    '&',    '\'',
         0,   0,   0,   0,   0,   0,   0,   0, // '(',    ')',    '*',    '+',    ',',    '-',    '.',    '/',
         0,   0,   0,   0,   0,   0,   0,   0, // '0',    '1',    '2',    '3',    '4',    '5',    '6',    '7',
         0,   0,   0,   0,   0,   0,   0,   0, // '8',    '9',    ':',    ';',    '<',    '=',    '>',    '?',
         0,   0,   0, 'C', 'D',   0,   0, 'G', // '@',    'A',    'B',    'C',    'D',    'E',    'F',    'G',
       'H',   0,   0,   0,   0,   0,   0, 'O', // 'H',    'I',    'J',    'K',    'L',    'M',    'N',    'O',
       'P',   0,   0,   0,   0,   0,   0,   0, // 'P',    'Q',    'R',    'S',    'T',    'U',    'V',    'W',
         0,   0,   0,   0,   0,   0,   0,   0, // 'X',    'Y',    'Z',    '[',    '\\',   ']',    '^',    '_',
         0,   0,   0, 'C', 'D',   0,   0, 'G', // '`',    'a',    'b',    'c',    'd',    'e',    'f',    'g',
       'H',   0,   0,   0,   0,   0,   0, 'O', // 'h',    'i',    'j',    'k',    'l',    'm',    'n',    'o',
       'P',   0,   0,   0,   0,   0,   0,   0, // 'p',    'q',    'r',    's',    't',    'u',    'v',    'w',
         0,   0,   0,   0,   0,   0,   0,   0  // 'x',    'y',    'z',    '{',    '|',    '}',    '~',    '\177'
      };

      if ((c = is_req[c]) == 0) U_RETURN(false);
      }

   // try to parse the request line: GET / HTTP/1.n

   u_http_info.method = ptr;

   const char* endptr;

   switch (c)
      {
      case 'G': // GET
         {
         U_http_method_len  = 3;
         U_http_method_type = HTTP_GET;

      // U_INTERNAL_ASSERT_EQUALS(U_STRNCASECMP(u_http_info.method, "GET "), 0)
         }
      break;

      case 'P': // POST/PUT
         {
         if (u__toupper(ptr[1]) == 'O')
            {
            U_http_method_len  = 4;
            U_http_method_type = HTTP_POST;

         // U_INTERNAL_ASSERT_EQUALS(U_STRNCASECMP(u_http_info.method, "POST "), 0)
            }
         else
            {
            U_http_method_len  = 3;
            U_http_method_type = HTTP_PUT;

         // U_INTERNAL_ASSERT_EQUALS(U_STRNCASECMP(u_http_info.method, "PUT "), 0)
            }
         }
      break;

      case 'H': // HEAD or response
         {
         if (ptr[1] == 'T')
            {
            // try to parse the response line: HTTP/1.n nnn <ssss>

            if (U_STRNCMP(ptr, "HTTP/1.")) U_RETURN(false);

            ptr += U_CONSTANT_SIZE("HTTP/1.") + 2;

            u_http_info.nResponseCode = strtol(ptr, (char**)&ptr, 10);

            U_INTERNAL_DUMP("u_http_info.nResponseCode = %d", u_http_info.nResponseCode)

            if (U_IS_HTTP_VALID_RESPONSE(u_http_info.nResponseCode)) goto is_response;

            U_RETURN(false);
            }

         U_http_method_len  = 4;
         U_http_method_type = HTTP_HEAD;

      // U_INTERNAL_ASSERT_EQUALS(U_STRNCASECMP(u_http_info.method, "HEAD "), 0)
         }
      break;

      case 'D': // DELETE
         {
         U_http_method_len  = 6;
         U_http_method_type = HTTP_DELETE;

      // U_INTERNAL_ASSERT_EQUALS(U_STRNCASECMP(u_http_info.method, "DELETE "), 0)
         }
      break;

      case 'C': // COPY
         {
         if (u__toupper(ptr[2]) != 'P') U_RETURN(false);

         U_http_method_len  = 4;
         U_http_method_type = HTTP_COPY;

      // U_INTERNAL_ASSERT_EQUALS(U_STRNCASECMP(u_http_info.method, "COPY "), 0)
         }
      break;

      case 'O': // OPTIONS
         {
         U_http_method_len  = 7;
         U_http_method_type = HTTP_OPTIONS;

      // U_INTERNAL_ASSERT_EQUALS(U_STRNCASECMP(u_http_info.method, "OPTIONS "), 0)
         }
      break;

      default: U_RETURN(false);
      }

   U_INTERNAL_DUMP("method = %.*S method_type = %C", U_HTTP_METHOD_TO_TRACE, U_http_method_type)

   U_INTERNAL_ASSERT_MAJOR(U_http_method_len,0)

   ptr += U_http_method_len;

   U_INTERNAL_DUMP("c (after method) = %C", *ptr)

   if (u__isspace((*ptr)) == false) U_RETURN(false);

   while (u__isspace(*++ptr)) {} // RFC 2616 19.3 "[servers] SHOULD accept any amount of SP or HT characters between [Request-Line] fields"

   u_http_info.uri   = ptr;
   u_http_info.query = 0;

   endptr = start + size;

   while (true)
      {
      c = (*(unsigned char*)ptr);

      if (c == ' ') break;

      /* check uri for invalid characters (NB: '\n' can be present with openssl base64) */

      if (u__iscntrl(c))
         {
         u_http_info.uri_len = 0;

         if (ptr >= endptr) goto end;

         U_WARNING("invalid character %C in URI %.*S", c, ptr - u_http_info.uri, u_http_info.uri);

         U_RETURN(false);
         }

      if (c == '?' &&
          u_http_info.uri_len == 0) // NB: we consider valid only the first '?' encountered...
         {
         u_http_info.uri_len =   ptr - u_http_info.uri;
         u_http_info.query   = ++ptr;

         continue;
         }

      ++ptr;
      }

   // NB: there are case of requests fragmented (maybe because of VPN tunnel)
   //     for example something like: GET /info?Mac=00%3A40%3A63%3Afb%3A42%3A1c&ip=172.16.93.235&gateway=172.16.93.254%3A5280&ap=ap%4010.8.0.9

   if (ptr >= endptr) goto end;

   if (u_http_info.query == 0) u_http_info.uri_len = ptr - u_http_info.uri;
   else
      {
      u_http_info.query_len = ptr - u_http_info.query;

      U_INTERNAL_DUMP("query = %.*S", U_HTTP_QUERY_TO_TRACE)

      if (u_http_info.query_len == U_CONSTANT_SIZE("_nav_") &&
          U_STRNEQ(u_http_info.query, "_nav_"))
         {
         U_http_is_navigation = true;
         }

      U_INTERNAL_DUMP("U_http_is_navigation = %b", U_http_is_navigation)
      }

   U_INTERNAL_DUMP("URI = %.*S", U_HTTP_URI_TO_TRACE)

   U_INTERNAL_ASSERT_MINOR(ptr, endptr)

   while (u__isspace(*++ptr)) {} // RFC 2616 19.3 "[servers] SHOULD accept any amount of SP or HT characters between [Request-Line] fields"

   if (U_STRNCMP(ptr, "HTTP/1.")) U_RETURN(false);

   ptr += U_CONSTANT_SIZE("HTTP/1.");

   U_http_version = ptr[0];

   U_INTERNAL_DUMP("U_http_version = %C", U_http_version)

is_response:
   while (u__islterm((c = *++ptr)) == false) {} 

   if (c == '\r')
      {
      u_line_terminator     = U_CRLF;
      u_line_terminator_len = 2;
      }
   else
      {
      U_INTERNAL_ASSERT_EQUALS(c, '\n')

      u_line_terminator     = U_LF;
      u_line_terminator_len = 1;
      }

end:
   u_http_info.startHeader += (ptr - start);

   U_INTERNAL_DUMP("u_http_info.startHeader(%u) = %.*S", u_http_info.startHeader, 20, ptr)

   U_RETURN(true);
}

bool UHTTP::findEndHeader(const UString& buffer)
{
   U_TRACE(0, "UHTTP::findEndHeader(%.*S)", U_STRING_TO_TRACE(buffer))

   U_INTERNAL_DUMP("u_http_info.startHeader = %u", u_http_info.startHeader)

   const char* ptr = buffer.c_pointer(u_http_info.startHeader);

   u_http_info.endHeader = u_findEndHeader(ptr, buffer.remain(ptr));

   if (u_http_info.endHeader != U_NOT_FOUND)
      {
      // NB: endHeader comprende anche la blank line...

      u_http_info.szHeader   = u_http_info.endHeader - (u_line_terminator_len * 2);
      u_http_info.endHeader += u_http_info.startHeader;

      U_INTERNAL_DUMP("szHeader = %u endHeader(%u) = %.*S", u_http_info.szHeader, u_http_info.endHeader, 20, buffer.c_pointer(u_http_info.endHeader))

      U_RETURN(true);
      }

   u_http_info.endHeader = 0;

   U_RETURN(false);
}

U_NO_EXPORT void UHTTP::manageBufferResize(const char* rpointer1, const char* rpointer2)
{
   U_TRACE(0, "UHTTP::manageBufferResize(%p)", rpointer1, rpointer2)

   U_INTERNAL_ASSERT_DIFFERS(rpointer1, rpointer2)

   ptrdiff_t diff = (rpointer2 - rpointer1);

                                        u_http_info.method          += diff;
                                        u_http_info.uri             += diff;
   if (u_http_info.query_len)           u_http_info.query           += diff;
   if (U_http_host_len)                 u_http_info.host            += diff;
   if (U_http_range_len)                u_http_info.range           += diff;
   if (u_http_info.cookie_len)          u_http_info.cookie          += diff;
   if (U_http_accept_len)               u_http_info.accept          += diff;
   if (u_http_info.referer_len)         u_http_info.referer         += diff;
   if (U_http_ip_client_len)            u_http_info.ip_client       += diff;
   if (u_http_info.websocket)           u_http_info.websocket       += diff;
   if (u_http_info.user_agent_len)      u_http_info.user_agent      += diff;
   if (U_http_content_type_len)         u_http_info.content_type    += diff;
   if (U_http_accept_language_len)      u_http_info.accept_language += diff;

   U_INTERNAL_DUMP("method          = %.*S", U_HTTP_METHOD_TO_TRACE)
   U_INTERNAL_DUMP("uri             = %.*S", U_HTTP_URI_TO_TRACE)
   U_INTERNAL_DUMP("query           = %.*S", U_HTTP_QUERY_TO_TRACE)
   U_INTERNAL_DUMP("host            = %.*S", U_HTTP_HOST_TO_TRACE)
   U_INTERNAL_DUMP("vhost           = %.*S", U_HTTP_VHOST_TO_TRACE)
   U_INTERNAL_DUMP("range           = %.*S", U_HTTP_RANGE_TO_TRACE)
   U_INTERNAL_DUMP("cookie          = %.*S", U_HTTP_COOKIE_TO_TRACE)
   U_INTERNAL_DUMP("accept          = %.*S", U_HTTP_ACCEPT_TO_TRACE)
   U_INTERNAL_DUMP("referer         = %.*S", U_HTTP_REFERER_TO_TRACE)
   U_INTERNAL_DUMP("ip_client       = %.*S", U_HTTP_IP_CLIENT_TO_TRACE)
   U_INTERNAL_DUMP("websocket       = %.*S", U_HTTP_WEBSOCKET_TO_TRACE)
   U_INTERNAL_DUMP("user_agent      = %.*S", U_HTTP_USER_AGENT_TO_TRACE)
   U_INTERNAL_DUMP("ctype           = %.*S", U_HTTP_CTYPE_TO_TRACE)
   U_INTERNAL_DUMP("accept_language = %.*S", U_HTTP_ACCEPT_LANGUAGE_TO_TRACE)
}

bool UHTTP::readHeader(USocket* s, UString& buffer)
{
   U_TRACE(0, "UHTTP::readHeader(%p,%.*S)", s, U_STRING_TO_TRACE(buffer))

   // u_http_info.method
   // u_http_info.uri
   // ....
   // u_http_info.if_modified_since;
   // ....

   U_HTTP_INFO_RESET(0);

   uint32_t sz     = buffer.size();
   const char* ptr = buffer.data(); // NB: it is possible a resize of the buffer string...

   U_INTERNAL_DUMP("sz = %u", sz)

   if ( sz < 18 && // 18 -> "GET / HTTP/1.0\r\n\r\n"
       (sz <  4 || (*(uint32_t*)(buffer.c_pointer(sz - 4)) != (*(uint32_t*)U_CRLF2))))
      {
      if (buffer.empty()  == false &&
          buffer.isText() == false)
         {
         setNoResponse();

         U_RETURN(false);
         }

read:
      if (USocketExt::read(s, buffer, 18, UServer_Base::timeoutMS, request_read_timeout) == false)
         {
         if (s->isTimeout())
            {
            U_http_is_connection_close = U_YES;
            u_http_info.nResponseCode  = HTTP_CLIENT_TIMEOUT;

            setResponse(0, 0);
            }

         U_RETURN(false);
         }

      sz = buffer.size();

      U_INTERNAL_DUMP("sz = %u", sz)

      if (ptr != buffer.data())
         {
         manageBufferResize(ptr, buffer.data());

         ptr = buffer.data();
         }
      }

start:
   if (U_http_method_type == 0)
      {
      // NB: u_http_info.startHeader is needed for loop...

      uint32_t sz1     = sz;
      const char* ptr1 = ptr;

      if (u_http_info.startHeader)
         {
         ptr1 = buffer.c_pointer(u_http_info.startHeader);
         sz1  = buffer.remain(ptr1);
         }

      if (scanfHeader(ptr1, sz1) == false) U_RETURN(false);

      // check if HTTP response line: HTTP/1.n 100 Continue

      U_INTERNAL_DUMP("u_http_info.nResponseCode = %d", u_http_info.nResponseCode)

      if (u_http_info.nResponseCode == HTTP_CONTINUE)
         {
         /*
         --------------------------------------------------------------------------------------------------------
         During the course of an HTTP 1.1 client sending a request to a server, the server might respond with
         an interim "100 Continue" response. This means the server has received the first part of the request,
         and can be used to aid communication over slow links. In any case, all HTT 1.1 clients must handle the
         100 response correctly (perhaps by just ignoring it). The "100 Continue" response is structured like
         any HTTP response, i.e. consists of a status line, optional headers, and a blank line. Unlike other
         responses, it is always followed by another complete, final response. Example:
         --------------------------------------------------------------------------------------------------------
         HTTP/1.0 100 Continue
         [blank line here]
         HTTP/1.0 200 OK
         Date: Fri, 31 Dec 1999 23:59:59 GMT
         Content-Type: text/plain
         Content-Length: 42
         some-footer: some-value
         another-footer: another-value
         [blank line here]
         abcdefghijklmnoprstuvwxyz1234567890abcdef
         --------------------------------------------------------------------------------------------------------
         To handle this, a simple HTTP 1.1 client might read one response from the socket;
         if the status code is 100, discard the first response and read the next one instead.
         --------------------------------------------------------------------------------------------------------
         */

         U_ASSERT(buffer.isEndHeader(u_http_info.startHeader))

         U_http_method_type        = 0;
         u_http_info.startHeader  += u_line_terminator_len * 2;
         u_http_info.nResponseCode = 0;

         if (sz <= u_http_info.startHeader) goto read;
                                            goto start;
         }
      }

   U_INTERNAL_DUMP("sz = %u u_http_info.startHeader = %u", sz, u_http_info.startHeader)

   // NB: there are case of requests fragmented (maybe because of VPN tunnel)
   //     for example something like: GET /info?Mac=00%3A40%3A63%3Afb%3A42%3A1c&ip=172.16.93.235&gateway=172.16.93.254%3A5280&ap=ap%4010.8.0.9

   if (u_http_info.startHeader >= sz)
      {
      U_http_method_type      = 0;
      u_http_info.startHeader = 0;

      goto read;
      }

   // NB: endHeader comprende anche la blank line...

   if (buffer.isEndHeader(u_http_info.startHeader))
      {
      u_http_info.endHeader = u_http_info.startHeader + (u_line_terminator_len * 2);

      U_INTERNAL_DUMP("u_http_info.endHeader = %u", u_http_info.endHeader)

      U_RETURN(true);
      }

   if (telnet_enable &&
       findEndHeader(buffer) == false)
      {
      const char* terminator;
      uint32_t terminator_len;

      if (u_line_terminator_len == 1)
         {
         terminator     = U_LF2;
         terminator_len = U_CONSTANT_SIZE(U_LF2);
         }
      else
         {
         terminator     = U_CRLF2;
         terminator_len = U_CONSTANT_SIZE(U_CRLF2);
         }

      if (USocketExt::readWhileNotToken(s, buffer,
                                        terminator, terminator_len,
                                        16, UServer_Base::timeoutMS, request_read_timeout) == U_NOT_FOUND)
         {
         U_http_is_connection_close = U_YES;
         u_http_info.nResponseCode  = HTTP_PRECON_FAILED;

         setResponse(0, 0);

         U_RETURN(false);
         }

      if (ptr != buffer.data()) manageBufferResize(ptr, buffer.data());
      }

   u_http_info.startHeader += u_line_terminator_len;
   u_http_info.szHeader     = buffer.size() - u_http_info.startHeader;

   U_INTERNAL_DUMP("u_http_info.szHeader = %u u_http_info.startHeader(%u) = %.*S",
                    u_http_info.szHeader, u_http_info.startHeader, 20, buffer.c_pointer(u_http_info.startHeader))

   U_RETURN(true);
}

U_NO_EXPORT bool UHTTP::isRequestTooLarge(UString& buffer)
{
   U_TRACE(0, "UHTTP::isRequestTooLarge(%.*S)", U_STRING_TO_TRACE(buffer))

   U_INTERNAL_DUMP("limit_request_body = %u", limit_request_body)

   U_INTERNAL_ASSERT_MAJOR(limit_request_body,0)

   if (                u_http_info.clength > limit_request_body     ||
       (buffer.reserve(u_http_info.clength + u_http_info.endHeader) &&
        buffer.isNull()))
      {
      U_RETURN(true);
      }

   U_RETURN(false);
}

__pure const char* UHTTP::getHeaderValuePtr(const UString& request, const UString& name, bool nocase)
{
   U_TRACE(0, "UHTTP::getHeaderValuePtr(%.*S,%.*S,%b)", U_STRING_TO_TRACE(request), U_STRING_TO_TRACE(name), nocase)

   if (u_http_info.szHeader) return UStringExt::getValueFromName(request, u_http_info.startHeader, u_http_info.szHeader, name, nocase);

   U_RETURN((const char*)0);
}

__pure const char* UHTTP::getHeaderValuePtr(const UString& name, bool nocase) { return getHeaderValuePtr(*UClientImage_Base::request, name, nocase); }

bool UHTTP::readBody(USocket* s, UString* pbuffer, UString& body)
{
   U_TRACE(0, "UHTTP::readBody(%p,%.*S,%.*S)", s, U_STRING_TO_TRACE(*pbuffer), U_STRING_TO_TRACE(body))

   U_INTERNAL_ASSERT(s->isConnected())
   U_INTERNAL_ASSERT_EQUALS((bool)body, false)

   // NB: check if request includes an entity-body (as indicated by the presence of Content-Length or Transfer-Encoding)

   if (u_http_info.clength)
      {
      uint32_t body_byte_read = (pbuffer->size() - u_http_info.endHeader);

      U_INTERNAL_DUMP("pbuffer->size() = %u body_byte_read = %u Content-Length = %u", pbuffer->size(), body_byte_read, u_http_info.clength)

      if (u_http_info.clength > body_byte_read)
         {
         if (isRequestTooLarge(*pbuffer))
            {
            u_http_info.nResponseCode  = HTTP_ENTITY_TOO_LARGE;
            U_http_is_connection_close = U_YES;

            setResponse(0, 0);

            U_RETURN(false);
            }

         // NB: check for 'Expect: 100-continue' (as curl does)...

         if (body_byte_read == 0                                                                                            &&
             pbuffer->find(*USocket::str_expect_100_continue, u_http_info.startHeader, u_http_info.szHeader) != U_NOT_FOUND &&
             USocketExt::write(s, U_CONSTANT_TO_PARAM("HTTP/1.1 100 Continue\r\n\r\n"), UServer_Base::timeoutMS) == false)
            {
            U_INTERNAL_ASSERT_EQUALS(U_http_version, '1')

            U_RETURN(false);
            }

#     ifdef U_HTTP_UPLOAD_PROGRESS_SUPPORT
         if (USocketExt::byte_read_hook)
            {
            (void) initUploadProgress(u_http_info.clength);

            if (body_byte_read) updateUploadProgress(body_byte_read);
            }
#     endif

         // NB: wait for other data...

         if (USocketExt::read(s, *pbuffer, u_http_info.clength - body_byte_read, UServer_Base::timeoutMS, request_read_timeout) == false)
            {
            u_http_info.nResponseCode  = HTTP_CLIENT_TIMEOUT;
            U_http_is_connection_close = U_YES;

            setResponse(0, 0);

            U_RETURN(false);
            }

#     ifdef U_HTTP_UPLOAD_PROGRESS_SUPPORT
         if (USocketExt::byte_read_hook) updateUploadProgress(u_http_info.clength); // done...
#     endif
         }
      }
   else
      {
      U_INTERNAL_DUMP("U_http_chunked = %b", U_http_chunked)

      if (U_http_chunked == false)
         {
         const char* chunk_ptr = getHeaderValuePtr(*pbuffer, *USocket::str_Transfer_Encoding, true);

         if (chunk_ptr)
            {
            if (USocket::str_chunked->equal(chunk_ptr, U_CONSTANT_SIZE("chunked")) == false)
               {
               setBadRequest();

               U_RETURN(false);
               }

            U_http_chunked = true;
            }
         }

      if (U_http_chunked)
         {
               char* out;
         const char* inp;
         const char* chunk_terminator;
         uint32_t count, chunkSize, chunk_terminator_len;
         /* ----------------------------------------------------------------------------------------------------------
         If a server wants to start sending a response before knowing its total length (like with long script output),
         it might use the simple chunked transfer-encoding, which breaks the complete response into smaller chunks and
         sends them in series. You can identify such a response because it contains the "Transfer-Encoding: chunked"
         header. All HTTP 1.1 clients must be able to receive chunked messages. A chunked message body contains a
         series of chunks, followed by a line with a single "0" (zero), followed by optional footers (just like headers),
         and a blank line. Each chunk consists of two parts: a line with the size of the chunk data, in hex, possibly
         followed by a semicolon and extra parameters you can ignore (none are currently standard), and ending with
         CRLF. the data itself, followed by CRLF. An example:

         HTTP/1.1 200 OK
         Date: Fri, 31 Dec 1999 23:59:59 GMT
         Content-Type: text/plain
         Transfer-Encoding: chunked
         [blank line here]
         1a; ignore-stuff-here
         abcdefghijklmnopqrstuvwxyz
         10
         1234567890abcdef
         0
         some-footer: some-value
         another-footer: another-value
         [blank line here]

         Note the blank line after the last footer. The length of the text data is 42 bytes (1a + 10, in hex),
         and the data itself is abcdefghijklmnopqrstuvwxyz1234567890abcdef. The footers should be treated like
         headers, as if they were at the top of the response. The chunks can contain any binary data, and may
         be much larger than the examples here. The size-line parameters are rarely used, but you should at
         least ignore them correctly. Footers are also rare, but might be appropriate for things like checksums
         or digital signatures
         -------------------------------------------------------------------------------------------------------- */

         if (u_line_terminator_len == 1)
            {
            chunk_terminator     = U_LF2;
            chunk_terminator_len = U_CONSTANT_SIZE(U_LF2);
            }
         else
            {
            chunk_terminator     = U_CRLF2;
            chunk_terminator_len = U_CONSTANT_SIZE(U_CRLF2);
            }

                                   count = pbuffer->find(chunk_terminator, u_http_info.endHeader, chunk_terminator_len);

         if (count == U_NOT_FOUND) count = USocketExt::readWhileNotToken(s, *pbuffer,
                                                                         chunk_terminator, chunk_terminator_len,
                                                                         16 * 1024, UServer_Base::timeoutMS, request_read_timeout);

         if (count == U_NOT_FOUND)
            {
            if (s->isOpen()) setBadRequest();
            else
               {
               u_http_info.nResponseCode  = HTTP_CLIENT_TIMEOUT;
               U_http_is_connection_close = U_YES;

               setResponse(0, 0);
               }

            U_RETURN(false);
            }

         count += chunk_terminator_len; // NB: the message include also the blank line...

         U_INTERNAL_DUMP("count = %u u_http_info.endHeader = %u", count, u_http_info.endHeader)

         if (count <= u_http_info.endHeader)
            {
            setBadRequest();

            U_RETURN(false);
            }

         u_http_info.clength = (count - u_http_info.endHeader);

         U_INTERNAL_DUMP("u_http_info.clength = %u", u_http_info.clength)

         body.setBuffer(u_http_info.clength);

         inp = pbuffer->c_pointer(u_http_info.endHeader);
         out = body.data();

         while (true)
            {
            // Decode the hexadecimal chunk size into an understandable number

            chunkSize = strtol(inp, 0, 16);

            U_INTERNAL_DUMP("chunkSize = %u inp[0] = %C", chunkSize, inp[0])

            // The last chunk is followed by zero or more trailers, followed by a blank line

            if (chunkSize == 0)
               {
               body.size_adjust(body.distance(out));

               U_INTERNAL_DUMP("body = %.*S", U_STRING_TO_TRACE(body))

               break;
               }

            U_INTERNAL_ASSERT(u__isxdigit(*inp))

            while (*inp++ != '\n') {} // discard the rest of the line

            U__MEMCPY(out, inp, chunkSize);

            inp += chunkSize + u_line_terminator_len;
            out += chunkSize;

            U_INTERNAL_ASSERT(inp <= (pbuffer->c_pointer(count)))
            }

         U_RETURN(true);
         }

      if (U_http_version == '1')
         {
         // HTTP/1.1 compliance: no missing Content-Length on POST requests

         u_http_info.nResponseCode  = HTTP_LENGTH_REQUIRED;
         U_http_is_connection_close = U_YES;

         setResponse(0, 0);

         U_RETURN(false);
         }

      U_ASSERT_EQUALS(UClientImage_Base::isPipeline(), false)

      if (u_http_info.szHeader == 0)
         {
         setBadRequest();

         U_RETURN(false);
         }

      (void) USocketExt::read(s, *pbuffer, 256 * 1024, UServer_Base::timeoutMS, request_read_timeout); // wait for other data (max 256k)...

      u_http_info.clength = (pbuffer->size() - u_http_info.endHeader);

      U_INTERNAL_DUMP("u_http_info.clength = %u", u_http_info.clength)

      if (u_http_info.clength == 0) U_RETURN(true);
      }

   body = pbuffer->substr(u_http_info.endHeader, u_http_info.clength);

   U_RETURN(true);
}

bool UHTTP::checkRequestForHeader(const UString& request)
{
   U_TRACE(0, "UHTTP::checkRequestForHeader(%.*S)", U_STRING_TO_TRACE(request))

   // --------------------------------
   // check in header request for:
   // --------------------------------
   // "Host: ..."
   // "Range: ..."
   // "Accept: ..."
   // "Cookie: ..."
   // "Referer: ..."
   // "X-Real-IP: ..."
   // "User-Agent: ..."
   // "Connection: ..."
   // "Content-Type: ..."
   // "Content-Length: ..."
   // "X-Forwarded-For: ..."
   // "Accept-Encoding: ..."
   // "Accept-Language: ..."
   // "If-Modified-Since: ..."
   // "Sec-WebSocket-Key: ..."
   // --------------------------------

   U_INTERNAL_ASSERT(request)
   U_INTERNAL_ASSERT_DIFFERS(ptrH, 0)
   U_INTERNAL_ASSERT_DIFFERS(ptrR, 0)
   U_INTERNAL_ASSERT_DIFFERS(ptrC, 0)
   U_INTERNAL_ASSERT_DIFFERS(ptrT, 0)
   U_INTERNAL_ASSERT_DIFFERS(ptrA, 0)
   U_INTERNAL_ASSERT_DIFFERS(ptrI, 0)
   U_INTERNAL_ASSERT_DIFFERS(ptrF, 0)
   U_INTERNAL_ASSERT_DIFFERS(ptrK, 0)
   U_INTERNAL_ASSERT_DIFFERS(ptrU, 0)
   U_INTERNAL_ASSERT_DIFFERS(ptrP, 0)
   U_INTERNAL_ASSERT_DIFFERS(ptrX, 0)
   U_INTERNAL_ASSERT_DIFFERS(ptrS, 0)
   U_INTERNAL_ASSERT_MAJOR(u_http_info.szHeader, 0)

   static const unsigned char ctable1[] = {
      0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0, // 24      25      26      27      28      29      30      31
      0,   0,   0,   0,   0,   0,   0,   0, // ' ',    '!',    '\"',   '#',    '$',    '%',    '&',    '\'',
      0,   0,   0,   0,   0,   0,   0,   0, // '(',    ')',    '*',    '+',    ',',    '-',    '.',    '/',
      0,   0,   0,   0,   0,   0,   0,   0, // '0',    '1',    '2',    '3',    '4',    '5',    '6',    '7',
      0,   0,   0,   0,   0,   0,   0,   0, // '8',    '9',    ':',    ';',    '<',    '=',    '>',    '?',
      0, 'A',   0, 'C',   0,   0,   0,   0, // '@',    'A',    'B',    'C',    'D',    'E',    'F',    'G',
    'H', 'I',   0,   0,   0,   0,   0,   0, // 'H',    'I',    'J',    'K',    'L',    'M',    'N',    'O',
      0,   0, 'R', 'S',   0, 'U',   0,   0, // 'P',    'Q',    'R',    'S',    'T',    'U',    'V',    'W',
    'X',   0,   0,   0,   0,   0,   0,   0, // 'X',    'Y',    'Z',    '[',    '\\',   ']',    '^',    '_',
      0, 'A',   0, 'C',   0,   0,   0,   0, // '`',    'a',    'b',    'c',    'd',    'e',    'f',    'g',
    'H', 'I',   0,   0,   0,   0,   0,   0, // 'h',    'i',    'j',    'k',    'l',    'm',    'n',    'o',
      0,   0, 'R', 'S',   0, 'U',   0,   0, // 'p',    'q',    'r',    's',    't',    'u',    'v',    'w',
    'X',   0,   0,   0,   0,   0,   0,   0  // 'x',    'y',    'z',    '{',    '|',    '}',    '~',    '\177'
   };

   static const unsigned char ctable2[] = {
      0,   0,   0,   0,   0,   0,   0,   0,
      0,   1,   0,   0,   0,   0,   0,   0, // '\b', '\t',
      0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0, // 24      25      26      27      28      29      30      31
      1,   0,   0,   0,   0,   0,   0,   0, // ' ',    '!',    '\"',   '#',    '$',    '%',    '&',    '\'',
      0,   0,   0,   0,   0,   0,   0,   0, // '(',    ')',    '*',    '+',    ',',    '-',    '.',    '/',
      0,   0,   0,   0,   0,   0,   0,   0, // '0',    '1',    '2',    '3',    '4',    '5',    '6',    '7',
      0,   0,   1,   0,   0,   0,   0,   0, // '8',    '9',    ':',    ';',    '<',    '=',    '>',    '?',
      0,   0,   0,   0,   0,   0,   0,   0, // '@',    'A',    'B',    'C',    'D',    'E',    'F',    'G',
      0,   0,   0,   0,   0,   0,   0,   0, // 'H',    'I',    'J',    'K',    'L',    'M',    'N',    'O',
      0,   0,   0,   0,   0,   0,   0,   0, // 'P',    'Q',    'R',    'S',    'T',    'U',    'V',    'W',
      0,   0,   0,   0,   0,   0,   0,   0, // 'X',    'Y',    'Z',    '[',    '\\',   ']',    '^',    '_',
      0,   0,   0,   0,   0,   0,   0,   0, // '`',    'a',    'b',    'c',    'd',    'e',    'f',    'g',
      0,   0,   0,   0,   0,   0,   0,   0, // 'h',    'i',    'j',    'k',    'l',    'm',    'n',    'o',
      0,   0,   0,   0,   0,   0,   0,   0, // 'p',    'q',    'r',    's',    't',    'u',    'v',    'w',
      0,   0,   0,   0,   0,   0,   0,   0  // 'x',    'y',    'z',    '{',    '|',    '}',    '~',    '\177'
   };

   bool result;
   const char* p;
   const char* p1;
   const char* p2;
   const char* p3;
   unsigned char c, c1;
   const char* ptr = request.data();
   uint32_t pos1, pos2, n, char_r = (u_line_terminator_len == 2),
            end = (u_http_info.endHeader ? (result = true, u_http_info.endHeader - u_line_terminator_len) : (result = false, request.size()));

   U_INTERNAL_DUMP("end = %u result = %b", end, result)

   for (pos1 = pos2 = u_http_info.startHeader; pos1 < end; pos1 = pos2 + 1)
      {
   // U_INTERNAL_DUMP("pos1 = %.*S", 20, request.c_pointer(pos1))

      c = *(p = (ptr + pos1));

      U_INTERNAL_DUMP("c = %C ctable1[%d] = %C", c, c, ctable1[c])

      if ((c = ctable1[c])               &&
              (ctable2[(int)p[(n =  4)]] || // "Host:"
               ctable2[(int)p[(n =  5)]] || // "Range:"
               ctable2[(int)p[(n =  6)]] || // "Cookie|Accept:"
               ctable2[(int)p[(n =  7)]] || // "Referer|Upgrade:"
               ctable2[(int)p[(n =  9)]] || // "X-Real-IP:"
               ctable2[(int)p[(n = 10)]] || // "Connection|User-Agent:"
               ctable2[(int)p[(n = 12)]] || // "Content-Type:"
               ctable2[(int)p[(n = 14)]] || // "Content-Length:"
               ctable2[(int)p[(n = 15)]] || // "Accept-Encoding/Language|X-Forwarded-For:"
               ctable2[(int)p[(n = 17)]]))  // "If-Modified-Since|Sec-WebSocket-Key:"
         {
         pos1 += n;

         while (u__isspace(ptr[pos1])) ++pos1;

         U_INTERNAL_DUMP("n = %u ptr[pos1] = %C", n, ptr[pos1])

         if (ptr[pos1] != ':')
            {
            pos1 -= n; // NB: we can have too much advanced...

            goto next;
            }

         do { ++pos1; } while (u__isspace(ptr[pos1]));

         if (pos1 >= end) U_RETURN(false); // NB: we can have too much advanced...

         pos2 = pos1;

         while (pos2 < end && ptr[pos2] != '\n') ++pos2;

      // U_INTERNAL_DUMP("pos2 = %.*S", 20, request.c_pointer(pos2))

         ++p;

         switch (c)
            {
            case 'C':
               {
               if (memcmp(ptrT, p, 7) == 0) // 7 -> sizeof("ontent-")
                  {
                  p += 8;
                  c1 = u__toupper(p[-1]);

                  if (c1 == 'T' &&
                      U_MEMCMP(p, "ype") == 0)
                     {
                     U_http_content_type_len  = pos2 - pos1 - char_r;
                     u_http_info.content_type = ptr + (ptrdiff_t)pos1;

                     U_INTERNAL_DUMP("Content-Type: = %.*S", U_HTTP_CTYPE_TO_TRACE)
                     }
                  else if (c1 == 'L' &&
                           U_MEMCMP(p, "ength") == 0)
                     {
                     u_http_info.clength = (uint32_t) strtoul(ptr + pos1, 0, 0);

                     U_INTERNAL_DUMP("Content-Length: = %.*S u_http_info.clength = %u", 10, ptr + pos1, u_http_info.clength)
                     }
                  }
               else if (memcmp(ptrC, p, 9) == 0) // 9 -> sizeof("onnection")
                  {
                  p = ptr + pos1;

                  U_INTERNAL_DUMP("Connection: = %.*S", pos2 - pos1 - char_r, p)

                  c1 = u__toupper(p[0]);

                  if (c1 == 'C')
                     {
                     if (U_STRNCASECMP(p+1, "lose") == 0)
                        {
                        U_http_is_connection_close = U_YES;

                        U_INTERNAL_DUMP("U_http_is_connection_close = %d", U_http_is_connection_close)
                        }
                     }
                  else if (c1 == 'K')
                     {
                     if (U_STRNCASECMP(p+1, "eep-alive") == 0)
                        {
                        U_http_keep_alive = '1';

                        U_INTERNAL_DUMP("U_http_keep_alive = %C", U_http_keep_alive)
                        }
                     }

                  else if (c1 == 'U')
                     {
                     if (U_STRNCASECMP(p+1, "pgrade") == 0)
                        {
                        U_http_upgrade = '1';

                        U_INTERNAL_DUMP("U_http_upgrade = %C", U_http_upgrade)
                        }
                     }
                  }
               else if (memcmp(ptrK, p, 5) == 0) // 5 -> sizeof("ookie")
                  {
                  u_http_info.cookie     = ptr + (ptrdiff_t)pos1;
                  u_http_info.cookie_len = pos2 - pos1 - char_r;

                  U_INTERNAL_DUMP("Cookie: = %.*S", U_HTTP_COOKIE_TO_TRACE)
                  }
               }
            break;

            case 'A':
               {
               if (memcmp(ptrA, p, 5) == 0) // 5 -> sizeof("ccept")
                  {
                  if (p[5] == '-')
                     {
                     p += 7;
                     c1 = u__toupper(p[-1]);

                     if (c1 == 'E' &&
                         U_MEMCMP(p, "ncoding") == 0)
                        {
                        p  = ptr + pos1;
                        p1 = p + 4;

                        U_INTERNAL_DUMP("Accept-Encoding: = %.*S", pos2 - pos1 - char_r, p)

                        if ((U_STRNEQ(p, "gzip") && U_STRNCMP(p1, ";q=0")) ||
                            u_find(p1, 30, U_CONSTANT_TO_PARAM("gzip")) != 0)
                           {
                           U_http_is_accept_gzip = '1';

                           U_INTERNAL_DUMP("U_http_is_accept_gzip = %C", U_http_is_accept_gzip)

                           U_ASSERT(u_find(p, 30, U_CONSTANT_TO_PARAM("gzip")))
                           }
                        }
                     else if (c1 == 'L' &&
                              U_MEMCMP(p, "anguage") == 0)
                        {
                        u_http_info.accept_language = ptr + (ptrdiff_t)pos1;
                        U_http_accept_language_len  = pos2 - pos1 - char_r;

                        U_INTERNAL_DUMP("Accept-Language: = %.*S", U_HTTP_ACCEPT_LANGUAGE_TO_TRACE)
                        }
                     }
                  else
                     {
                     u_http_info.accept = ptr + (ptrdiff_t)pos1;
                     U_http_accept_len  = pos2 - pos1 - char_r;

                     U_INTERNAL_DUMP("Accept: = %.*S", U_HTTP_ACCEPT_TO_TRACE)
                     }
                  }
               }
            break;

            case 'R':
               {
               if (memcmp(ptrF, p, 6) == 0) // 6 -> sizeof("eferer")
                  {
                  u_http_info.referer     = ptr + (ptrdiff_t)pos1;
                  u_http_info.referer_len = pos2 - pos1 - char_r;

                  U_INTERNAL_DUMP("Referer: = %.*S", U_HTTP_REFERER_TO_TRACE)
                  }
               else if (memcmp(ptrR, p, 4) == 0) // 4 -> sizeof("ange")
                  {
                  if (U_STRNEQ(ptr + pos1, "bytes="))
                     {
                     u_http_info.range = ptr + (ptrdiff_t)pos1 + U_CONSTANT_SIZE("bytes=");
                     U_http_range_len  = pos2 - pos1 - char_r  - U_CONSTANT_SIZE("bytes=");

                     U_INTERNAL_DUMP("Range = %.*S", U_HTTP_RANGE_TO_TRACE)
                     }
                  }
               }
            break;

            case 'X':
               {
               if (p[0] == '-')
                  {
                  c1 = u__toupper(p[1]);

                  // check of CLIENT-IP, WEBPROXY-REMOTE-ADDR, FORWARDED miss...

                  if (c1 == 'F') // "X-Forwarded-For"
                     {
                     if (u__toupper(p[11]) == 'F'      &&
                         memcmp(ptrX+2,  p+2,  9) == 0 && // 9 -> sizeof("orwarded-")
                         memcmp(ptrX+12, p+12, 2) == 0)   // 2 -> sizeof("or")
                        {
                        u_http_info.ip_client = ptr + (ptrdiff_t)pos1;
                        U_http_ip_client_len  = pos2 - pos1 - char_r;

                        U_INTERNAL_DUMP("X-Forwarded-For: = %.*S", U_HTTP_IP_CLIENT_TO_TRACE)
                        }
                     }
                  else if (c1 == 'R') // "X-Real-IP"
                     {
                     if (u__toupper(p[6]) == 'I' &&
                         u__toupper(p[7]) == 'P' &&
                         memcmp(ptrP+2, p+2, 4) == 0) // 4 -> sizeof("eal-")
                        {
                        u_http_info.ip_client = ptr + (ptrdiff_t)pos1;
                        U_http_ip_client_len  = pos2 - pos1 - char_r;

                        U_INTERNAL_DUMP("X-Real-IP: = %.*S", U_HTTP_IP_CLIENT_TO_TRACE)
                        }
                     }
                  else if (c1 == 'H') // "X-Http-X-Forwarded-For"
                     {
                     if (u__toupper(p[2])  == 'T'      &&
                         u__toupper(p[3])  == 'T'      &&
                         u__toupper(p[4])  == 'P'      &&
                         u__toupper(p[18]) == 'F'      &&
                         memcmp(ptrX+9,  p+9,  9) == 0 && // 9 -> sizeof("orwarded-")
                         memcmp(ptrX+19, p+19, 2) == 0)   // 2 -> sizeof("or")
                        {
                        u_http_info.ip_client = ptr + (ptrdiff_t)pos1;
                        U_http_ip_client_len  = pos2 - pos1 - char_r;

                        U_INTERNAL_DUMP("X-Http-X-Forwarded-For: = %.*S", U_HTTP_IP_CLIENT_TO_TRACE)
                        }
                     }

                  if (U_http_ip_client_len)
                     {
                     n  = 0;
                     p2 = u_http_info.ip_client;

                     do {
                        c = p2[n];

                        if (u__isspace(c) || c == ',') break;
                        }
                     while (++n < (uint32_t)U_http_ip_client_len);

                     U_INTERNAL_DUMP("ip_client = %.*S", n, u_http_info.ip_client)

                     if (u_isIPAddr(UClientImage_Base::bIPv6, u_http_info.ip_client, n))
                        {
                        U_INTERNAL_ASSERT_MINOR(n, U_INET_ADDRSTRLEN)

                        U__MEMCPY(UServer_Base::client_address, u_http_info.ip_client, n);

                        UServer_Base::client_address[n] = '\0';

                        U_INTERNAL_DUMP("UServer_Base::client_address = %S", UServer_Base::client_address)
                        }
                     }
                  }
               }
            break;

            case 'U':
               {
               if (u__toupper(p[4]) == 'A'      &&
                   memcmp(ptrU,   p,   4) == 0  && // 4 -> sizeof("ser-")
                   memcmp(ptrU+5, p+5, 4) == 0)    // 4 -> sizeof(     "gent")
                  {
                  u_http_info.user_agent     = ptr + (ptrdiff_t)pos1;
                  u_http_info.user_agent_len = pos2 - pos1 - char_r;

                  U_INTERNAL_DUMP("User-Agent: = %.*S", U_HTTP_USER_AGENT_TO_TRACE)
                  }
               else if (U_STRNEQ(p, "pgrade"))
                  {
                  U_http_upgrade = '1';

                  U_INTERNAL_DUMP("U_http_upgrade = %C", U_http_upgrade)

                  p = ptr + pos1;

                  U_INTERNAL_DUMP("Upgrade: = %.*S", pos2 - pos1 - char_r, p)

                  if (U_STRNCASECMP(p, "websocket") == 0)
                     {
                     U_http_websocket = '1';

                     U_INTERNAL_DUMP("U_http_websocket = %C", U_http_websocket)
                     }
                  }
               }
            break;

            case 'I': // If-Modified-Since
               {
               if (u__toupper(p[2])  == 'M'       &&
                   u__toupper(p[11]) == 'S'       &&
                   memcmp(ptrI,    p,    2) == 0  && // 2 -> sizeof("f-")
                   memcmp(ptrI+3,  p+3,  8) == 0  && // 8 -> sizeof(   "odified-")
                   memcmp(ptrI+12, p+12, 4) == 0)    // 4 -> sizeof(            "ince")
                  {
                  u_http_info.if_modified_since = UTimeDate::getSecondFromTime(ptr + pos1, true);

                  U_INTERNAL_DUMP("If-Modified-Since = %ld", u_http_info.if_modified_since)
                  }
               }
            break;

            case 'H':
               {
               if (memcmp(ptrH, p, 3) == 0)
                  {
                  u_http_info.host = ptr + (ptrdiff_t)pos1;
                  U_http_host_len  =
                  U_http_host_vlen = pos2 - pos1 - char_r;

                  p2 = p1 = u_http_info.host;

                  U_INTERNAL_DUMP("U_http_host_len  = %u U_HTTP_HOST  = %.*S", U_http_host_len, U_HTTP_HOST_TO_TRACE)

                  // Host: hostname[:port]

                  for (p3 = p2 + U_http_host_len; p2 < p3; ++p2)  
                     {
                     if (*p2 == ':')
                        {
                        U_http_host_vlen = p2 - p1;

                        break;
                        }
                     }

                  U_INTERNAL_DUMP("U_http_host_vlen = %u U_HTTP_VHOST = %.*S", U_http_host_vlen, U_HTTP_VHOST_TO_TRACE)
                  }
               }
            break;

            case 'S': // Sec-WebSocket-Key
               {
               if (u__toupper(p[3])  == 'W'      &&
                   u__toupper(p[6])  == 'S'      &&
                   u__toupper(p[13]) == 'K'      &&
                   memcmp(ptrS,    p,    3) == 0 && // 3 -> sizeof("ec-")
                   memcmp(ptrS+4,  p+4,  2) == 0 && // 2 -> sizeof(     "eb")
                   memcmp(ptrS+7,  p+7,  6) == 0 && // 6 -> sizeof(        "ocket-")
                   memcmp(ptrS+14, p+14, 2) == 0)   // 2 -> sizeof(               "ey")
                  {
                  u_http_info.websocket = ptr + (ptrdiff_t)pos1;
                  U_http_websocket_len  = pos2 - pos1 - char_r;

                  U_INTERNAL_DUMP("Sec-WebSocket-Key: = %.*S", U_HTTP_WEBSOCKET_TO_TRACE)
                  }
               }
            break;
            }
         }
next:
      pos2 = pos1;

      while (ptr[pos2] != '\n')
         {
         if (++pos2 >= end) U_RETURN(false); // NB: we can have too much advanced...
         }

      U_INTERNAL_DUMP("pos2 = %.*S", 10, request.c_pointer(pos2))

      if (u_http_info.endHeader == 0)
         {
         c = (p1 = (ptr + pos2))[1];

         U_INTERNAL_DUMP("c (after newline) = %C", c)

         // \n\n     (U_LF2)
         // \r\n\r\n (U_CRLF2)

         if (u__islterm(c))
            {
            if (p1[-1] == '\r' &&
                p1[ 2] == '\n')
               {
               u_http_info.szHeader  =  pos2 - 1 - u_http_info.startHeader;
               u_http_info.endHeader = (pos2 + 3);

               U_INTERNAL_DUMP("szHeader = %u endHeader(%u) = %.*S", u_http_info.szHeader,u_http_info.endHeader,
                                                                     20,request.c_pointer(u_http_info.endHeader))

               if (u_http_info.endHeader <= end) U_RETURN(true);
               }
            else if (u_line_terminator_len == 1)
               {
               U_ASSERT(request.isEndHeader(pos2))

               u_http_info.szHeader  =  pos2 - u_http_info.startHeader;
               u_http_info.endHeader = (pos2 + 2);

               U_INTERNAL_DUMP("szHeader = %u endHeader(%u) = %.*S", u_http_info.szHeader,u_http_info.endHeader,20,request.c_pointer(u_http_info.endHeader))

               U_RETURN(true);
               }

            U_RETURN(false);
            }
         }
      }

   U_RETURN(result);
}

// inlining failed in call to ...: call is unlikely and code size would grow

#ifdef U_HTTP_CACHE_REQUEST
void UHTTP::clearRequestCache()
{
   U_TRACE(0, "UHTTP::clearRequestCache()")

   U_INTERNAL_DUMP("cbuffer(%u) = %.*S", cbuffer->size(), U_STRING_TO_TRACE(*cbuffer))

   cbuffer->clear();

   U_INTERNAL_DUMP("UServer_Base::sfd = %d", UServer_Base::sfd)

   if (UServer_Base::sfd) UServer_Base::sfd = 0;
}

int UHTTP::checkRequestCache()
{
   U_TRACE(0, "UHTTP::checkRequestCache()")

   U_INTERNAL_ASSERT_EQUALS(cbuffer->isNull(), false)

   uint32_t end    = u_http_info.startHeader - 2;
   const char* ptr = UClientImage_Base::rbuffer->data();

   if (cbuffer->compare(0U, end, ptr, end) == 0)
      {
      int result;

      if ((end = UClientImage_Base::rbuffer->size()) != cbuffer->size())
         {
         const char* p;
         unsigned char c;
         bool http_gzip = false;
         char http_keep_alive = '\0';

         for (uint32_t pos = u_http_info.startHeader; pos < end; ++pos)
            {
            U_INTERNAL_DUMP("pos = %.*S", 20, UClientImage_Base::rbuffer->c_pointer(pos))

            c = *(p = (ptr + pos));

            U_INTERNAL_DUMP("c = %C", c)

            if (c == '\r') goto end;

            if (c == 'C')
               {
               if (memcmp(ptrC, p+1, 9) == 0) // 9 -> sizeof("onnection")
                  {
                  p += 11;

                  U_INTERNAL_DUMP("Connection: = %.*S", 13, p-1)

                  for (uint32_t i = 0; i < 4; ++i)
                     {
                     if (u__toupper(p[i]) == 'K')
                        {
                        U_INTERNAL_ASSERT_EQUALS(U_STRNCASECMP(p+i, "keep-alive"), 0);

                        http_keep_alive = '1';

                        U_INTERNAL_DUMP("http_keep_alive = %C", http_keep_alive)

                        break;
                        }
                     }

                  if (U_http_is_accept_gzip != '2' || http_gzip) goto end;

                  pos = (p - ptr);
                  }

               goto next;
               }

            if (U_http_is_accept_gzip != '2' || http_gzip) goto next;

            if (c == 'A'                    &&
                memcmp(ptrA, p+1, 5) ==  0  && // 5 -> sizeof("ccept")
                             p[6]    == '-' &&
                    U_MEMCMP(p+7, "Encoding") == 0)
               {
               p += 16;

               U_INTERNAL_DUMP("Accept-Encoding: = %.*S", 20, p-1)

               p = (const char*) u_find(p, 30, U_CONSTANT_TO_PARAM("gzip"));

               if (p && U_STRNCMP(p+4, ";q=0"))
                  {
                  http_gzip = true;

                  U_INTERNAL_DUMP("http_gzip = %b", http_gzip)
                  }
               }
next:
            do { ++pos; } while (pos < end && ptr[pos] != '\n');
            }
end:
         U_INTERNAL_DUMP("U_http_version = %C U_http_keep_alive = %C U_http_is_connection_close = %d U_http_is_accept_gzip = %C",
                          U_http_version,     U_http_keep_alive,     U_http_is_connection_close,     U_http_is_accept_gzip)

         if (http_keep_alive != U_http_keep_alive ||
             (http_gzip == false && U_http_is_accept_gzip == '2'))
            {
            U_RETURN(U_PLUGIN_HANDLER_FINISHED);
            }

         (void) cbuffer->_assign(UClientImage_Base::rbuffer->rep);
         }

                                               result  = U_PLUGIN_HANDLER_AGAIN;
      if (U_http_is_connection_close == U_YES) result |= U_PLUGIN_HANDLER_ERROR;

      U_INTERNAL_DUMP("UServer_Base::sfd = %d", UServer_Base::sfd)

      if (UServer_Base::sfd)
         {
         U_ASSERT(UClientImage_Base::body->isNull())
         U_INTERNAL_ASSERT_MAJOR(UServer_Base::count,0)

         U_INTERNAL_DUMP("UServer_Base::pClientImage->sfd = %d", UServer_Base::pClientImage->sfd)

         U_INTERNAL_ASSERT_EQUALS(UServer_Base::pClientImage->sfd, 0)

         UServer_Base::pClientImage->sfd    = UServer_Base::sfd;
         UServer_Base::pClientImage->start  = UServer_Base::start;
         UServer_Base::pClientImage->count  = UServer_Base::count;
         UServer_Base::pClientImage->bclose = UServer_Base::bclose;
         }

      U_RETURN(result);
      }

   U_RETURN(U_PLUGIN_HANDLER_FINISHED);
}

void UHTTP::manageRequestCache()
{
   U_TRACE(1, "UHTTP::manageRequestCache()")

   if (isGETorHEAD()                                &&
       U_IS_HTTP_SUCCESS(u_http_info.nResponseCode) &&
       U_http_no_cache                 == false     &&
       UClientImage_Base::isPipeline() == false)
      {
      U_INTERNAL_ASSERT(cbuffer->isNull())

      (void) cbuffer->_assign(UClientImage_Base::rbuffer->rep);

      U_INTERNAL_DUMP("cbuffer(%u) = %.*S", cbuffer->size(), U_STRING_TO_TRACE(*cbuffer))

      U_gettimeofday; // NB: optimization if it is enough a time resolution of one second...

      U_INTERNAL_DUMP("expire        = %ld", UServer_Base::expire)
      U_INTERNAL_DUMP("u_now->tv_sec = %ld", u_now->tv_sec)

      UServer_Base::expire = u_now->tv_sec + 1;

      if (file_data)
         {
         U_INTERNAL_DUMP("bsendfile = %b", bsendfile)

         if (bsendfile)
            {
            U_INTERNAL_DUMP("UServer_Base::pClientImage->sfd   = %d", UServer_Base::pClientImage->sfd)
            U_INTERNAL_DUMP("UServer_Base::pClientImage->state = %d", UServer_Base::pClientImage->state)

            U_INTERNAL_ASSERT_EQUALS(UServer_Base::sfd, 0)

            UServer_Base::sfd = file_data->fd;
            }
         }
      }
}
#endif

/*
 * There are four parts to an HTTP request:
 *
 * The Request Line: the method, the URL, the version of the protocol
 *
 * The Request Headers [OPTIONAL]: a series of lines (one per) in the format of name, colon(:), and the value of the header.
 *
 * A Blank Line: required, worth mentioning by itself.
 *
 * The Request Body [OPTIONAL]: Used in POST requests to send content to the server.
 */

int UHTTP::manageRequest()
{
   U_TRACE(0, "UHTTP::manageRequest()")

   int result = U_PLUGIN_HANDLER_ERROR;

#ifdef U_HTTP_CACHE_REQUEST
   U_INTERNAL_DUMP("cbuffer(%u) = %.*S",                    cbuffer->size(), U_STRING_TO_TRACE(*cbuffer))
   U_INTERNAL_DUMP("rbuffer(%u) = %.*S", UClientImage_Base::rbuffer->size(), U_STRING_TO_TRACE(*UClientImage_Base::rbuffer))

   if (cbuffer->isNull() == false)
      {
      U_gettimeofday; // NB: optimization if it is enough a time resolution of one second...

      U_INTERNAL_DUMP("expire        = %ld", UServer_Base::expire)
      U_INTERNAL_DUMP("u_now->tv_sec = %ld", u_now->tv_sec)

      if (UServer_Base::expire >= u_now->tv_sec)
         {
         result = checkRequestCache();

         if (result != U_PLUGIN_HANDLER_FINISHED)
            {
            if (apache_like_log) writeApacheLikeLog(false);

            U_RETURN(result);
            }
         }

      clearRequestCache();
      }

   if (UClientImage_Base::isPipeline() == false) UClientImage_Base::initAfterGenericRead();
#endif

   if (readHeader(UServer_Base::pClientImage->socket, *UClientImage_Base::request) &&
       (u_http_info.szHeader == 0                                                  ||
        (isValidRequest()                                                          &&
         checkRequestForHeader(*UClientImage_Base::request))))
      {
      bool request_buffer_resize      = false;
      UClientImage_Base::size_request = u_http_info.endHeader;

      U_INTERNAL_DUMP("u_http_info.clength = %u", u_http_info.clength)

      if (u_http_info.clength == 0 &&
          isPOST()            == false)
         {
         result = U_PLUGIN_HANDLER_FINISHED;
         }
      else
         {
         const char* ptr = UClientImage_Base::request->data(); // NB: it is possible a resize of the request string...

         result = (readBody(UServer_Base::pClientImage->socket, UClientImage_Base::request, *UClientImage_Base::body)
                        ? U_PLUGIN_HANDLER_FINISHED
                        : U_PLUGIN_HANDLER_ERROR);

         if (result == U_PLUGIN_HANDLER_FINISHED)
            {
            if (ptr != UClientImage_Base::request->data())
               {
               request_buffer_resize = true;

               manageBufferResize(ptr, UClientImage_Base::request->data());
               }

            UClientImage_Base::size_request += u_http_info.clength;
            }
         else if (U_http_is_connection_close == U_YES ||
                  UServer_Base::pClientImage->socket->isClosed())
            {
            // NB: we don't need to creat a pipeline (we are going to close the connection or it is already closed)...

            UClientImage_Base::size_request = UClientImage_Base::request->size();
            }
         }

      UClientImage_Base::manageRequestSize(request_buffer_resize);

      if (result == U_PLUGIN_HANDLER_FINISHED)
         {
         if (U_http_host_len)
            {
            // NB: as protection from DNS rebinding attack web servers can reject HTTP requests with an unrecognized Host header...

            if (UServer_Base::public_address &&
                ((U_http_host_len - U_http_host_vlen) > (1 + 5) || // NB: ':' + 0-65536
                 u_isHostName(U_HTTP_VHOST_TO_PARAM) == false))
               {
               result = U_PLUGIN_HANDLER_ERROR;
               }
            }
         else if (U_http_version == '1')
            {
            // HTTP 1.1 want header "Host: " ...

            result = U_PLUGIN_HANDLER_ERROR;
            }
         }
      }

#ifdef DEBUG
   if (UServer_Base::pClientImage->socket->isClosed())
      {
      U_INTERNAL_ASSERT_EQUALS(result, U_PLUGIN_HANDLER_ERROR)
      }
#endif

   if (result == U_PLUGIN_HANDLER_ERROR)
      {
      if (UClientImage_Base::write_off == false)
         {
         if (UServer_Base::pClientImage->socket->isClosed()) UClientImage_Base::write_off = true;
         else
            {
            U_http_is_connection_close = U_YES;

            if (UClientImage_Base::wbuffer->empty())
               {
               // HTTP/1.1 compliance:
               // -----------------------------------------------------
               // Sends 501 for request-method != (GET|POST|HEAD)
               // Sends 505 for protocol != HTTP/1.[0-1]
               // Sends 400 for broken Request-Line
               // Sends 411 for missing Content-Length on POST requests

                    if (U_http_method_type == 0)                        u_http_info.nResponseCode = HTTP_NOT_IMPLEMENTED;
               else if (U_http_version     == 0 && u_http_info.uri_len) u_http_info.nResponseCode = HTTP_VERSION;
               else
                  {
                  setBadRequest();

                  U_RETURN(U_PLUGIN_HANDLER_ERROR);
                  }

               setResponse(0, 0);
               }
            }
         }

      U_RETURN(U_PLUGIN_HANDLER_ERROR);
      }

   // check the HTTP message

   U_ASSERT(isRequestNotFound())
   U_INTERNAL_ASSERT(*UClientImage_Base::request)

   const char* ptr;
   uint32_t len1, len2;

   // reset

   file_data    = 0;
   bsendfile    = false;
   u_mime_index = -1;

   // reset alias

   if (alias->empty() == false)
      {
            alias->clear();
      request_uri->clear();
      }

   // manage alias uri

   if (maintenance_mode_page &&
       U_HTTP_URI_STRNEQ("favicon.ico") == false)
      {
      (void) request_uri->assign(U_HTTP_URI_TO_PARAM);

      (void) alias->append(*maintenance_mode_page);

      goto next1;
      }

   // manage virtual host

   if (virtual_host &&
       U_http_host_vlen)
      {
      // Host: hostname[:port]

      alias->setBuffer(1 + U_http_host_vlen + u_http_info.uri_len);

      alias->snprintf("/%.*s", U_HTTP_VHOST_TO_TRACE);
      }

   if (valias)
      {
      int flag;
      UString mask;

      // Ex: /admin /admin.html

      for (int32_t i = 0, n = valias->size(); i < n; i += 2)
         {
         flag = 0;
         mask = (*valias)[i];

         ptr  = mask.data();
         len1 = mask.size();

         if (ptr[0] == '!')
            {
            ++ptr;
            --len1;

            flag = FNM_INVERT;
            }

         if (U_HTTP_URI_DOSMATCH(ptr, len1, flag))
            {
            // NB: this is exclusive with SSI alias...

            (void) request_uri->assign(U_HTTP_URI_TO_PARAM);

            (void) alias->append((*valias)[i+1]);

            break;
            }
         }
      }

   // manage global alias

   if (global_alias          &&
       request_uri->isNull() &&
       u_getsuffix(U_HTTP_URI_TO_PARAM) == 0)
      {
      (void) request_uri->assign(U_HTTP_URI_TO_PARAM);

      (void) alias->append(*global_alias);
      }

   if (alias->empty() == false)
      {
      U_INTERNAL_ASSERT_EQUALS(alias->first_char(), '/')

      if (request_uri->isNull())
         {
         U_INTERNAL_ASSERT(virtual_host)

         (void) request_uri->assign(U_HTTP_URI_TO_PARAM);

         (void) alias->append(U_HTTP_URI_TO_PARAM);
         }
next1:
      u_http_info.uri     = alias->data();
      u_http_info.uri_len = alias->size();

      U_SRV_LOG("ALIAS: URI request changed to: %.*s", U_HTTP_URI_TO_TRACE);
      }

   // ...process the HTTP message

   U_INTERNAL_DUMP("method = %.*S method_type = %C URI = %.*S", U_HTTP_METHOD_TO_TRACE, U_http_method_type, U_HTTP_URI_TO_TRACE)

   pathname->setBuffer(u_cwd_len + u_http_info.uri_len);

   U_INTERNAL_DUMP("u_cwd(%u) = %.*S", u_cwd_len, u_cwd_len, u_cwd)

   pathname->snprintf("%w%.*s", U_HTTP_URI_TO_TRACE);

   U_INTERNAL_ASSERT_DIFFERS(pathname->size(), u_cwd_len)

   checkPath();

   if (isRequestNotFound())
      {
      ptr  = pathname->c_pointer(u_cwd_len);
      len1 = pathname->size() -  u_cwd_len;

      // NB: if status is 'file not found' and we have virtual host
      //     we check if it is present as shared file (without the virtual host prefix...)

      if (virtual_host &&
          U_http_host_vlen)
         {
         len2 = u_http_info.uri_len - U_http_host_vlen - 1;

         (void) U_SYSCALL(memmove, "%p,%p,%u", (void*)ptr, ptr + 1 + U_http_host_vlen, len2);

         if (checkPath(len2)) goto next2;
         }

      // URI request can be URL encoded...

      len2 = u_url_decode(U_HTTP_URI_TO_PARAM, (unsigned char*)ptr, true);

      if (len1 != len2 &&
          checkPath(len2))
          {
          goto next2;
          }
      }

   // NB: apply rewrite rule if requested and if status is 'file forbidden or not exist'...

   if (vRewriteRule &&
       U_http_request_check <= U_HTTP_REQUEST_IS_FORBIDDEN)
      {
      processRewriteRule();
      }

next2:
   // in general at this point, after checkPath(), we can have as status:
   // ------------------------------------------------------------------
   // 1) file is forbidden (not in DOC_ROOT)
   // 2) DOC_ROOT dir need to be processed (and it can be forbidden...)
   // 3) file is in FILE CACHE with/without content (stat() cache...)
   // 4) file do not exist

   if (apache_like_log) writeApacheLikeLog(true);

   U_INTERNAL_DUMP("file_data = %p U_http_request_check = %C u_http_info.flag = %.8S", file_data, U_http_request_check, u_http_info.flag)

   if (isRequestInFileCache()) // 3
      {
      U_INTERNAL_ASSERT_POINTER(file_data)

      // NB: check for dynamic page (servlet)...

      u_mime_index = file_data->mime_index;

      U_INTERNAL_DUMP("u_mime_index = %C U_http_is_navigation = %b", u_mime_index, U_http_is_navigation)

      if ((u_is_usp()                ||
           u__isdigit(u_mime_index)) &&
          U_http_is_navigation == false)
         {
         (void) runDynamicPage(0);

         keyID->clear();

         goto next3;
         }

      u_http_info.nResponseCode = HTTP_OK;

      // NB: check if we can service the content of file directly from cache...

      if (isDataFromCache() &&
          isGETorHEAD() &&
          (checkGetRequestIfModified() == false || processFileCache()))
         {
next3:
         setRequestProcessed();

         goto check;
         }

      // NB: ...if not, set status to 'file exist and need to be processed'...

      setRequestNeedProcessing();
      }

   if (isRequestAlreadyProcessed())
      {
check:
      U_INTERNAL_DUMP("u_is_ssi() = %b u_http_info.nResponseCode = %d", u_is_ssi(), u_http_info.nResponseCode)

      if (u_is_ssi() &&
          u_http_info.nResponseCode != HTTP_NO_CONTENT)
         {
         setRequestNeedProcessing();

         goto next4;
         }

#  ifdef U_HTTP_CACHE_REQUEST
      manageRequestCache();
#  endif
      }

   U_DUMP("isRequestNotFound() = %b U_http_upgrade = %C U_http_websocket = %C", isRequestNotFound(), U_http_upgrade, U_http_websocket)

   if (isRequestNotFound() == false || // 4
       U_http_upgrade)
      {
next4:
      // check if the uri use HTTP Strict Transport Security to force client to use secure connections only

      if (uri_strict_transport_security_mask                                                                                 &&
#     ifdef USE_LIBSSL
          UServer_Base::bssl == false                                                                                        &&
#     endif
          (u_dosmatch_with_OR(U_HTTP_URI_TO_PARAM,              U_STRING_TO_PARAM(*uri_strict_transport_security_mask), 0)   ||
           (request_uri->empty() == false                                                                                    &&
            u_dosmatch_with_OR(U_STRING_TO_PARAM(*request_uri), U_STRING_TO_PARAM(*uri_strict_transport_security_mask), 0))))
         {
         // we are in cleartext at the moment, prevent further execution and output

         char redirect_url[32 * 1024];

         UString _uri = getRequestURIWithQuery(), ip_server = UServer_Base::getIPAddress();

         setRedirectResponse(2, *str_strict_transport_security, (const char*)redirect_url,
                                    u__snprintf(redirect_url, sizeof(redirect_url), "%s:/%.*s%.*s",
                                       U_http_websocket ? "wss" : "https", U_STRING_TO_TRACE(ip_server), U_STRING_TO_TRACE(_uri)));

         U_SRV_LOG("URI_STRICT_TRANSPORT_SECURITY: request redirected to %S", redirect_url);

         setRequestProcessed();

         U_RETURN(U_PLUGIN_HANDLER_FINISHED);
         }

      // check if the uri need a certificate

#  ifdef USE_LIBSSL
      if (UServer_Base::bssl                                                                                    &&
          uri_request_cert_mask                                                                                 &&
          (u_dosmatch_with_OR(U_HTTP_URI_TO_PARAM,              U_STRING_TO_PARAM(*uri_request_cert_mask), 0)   ||
           (request_uri->empty() == false                                                                       &&
            u_dosmatch_with_OR(U_STRING_TO_PARAM(*request_uri), U_STRING_TO_PARAM(*uri_request_cert_mask), 0))) &&
          ((UServer<USSLSocket>*)UServer_Base::pthis)->askForClientCertificate() == false)
         {
         U_SRV_LOG("URI_REQUEST_CERT: request denied by mandatory certificate from client");

         setForbidden();

         U_RETURN(U_PLUGIN_HANDLER_ERROR);
         }
#  endif

      // check if the uri is protected

      if (uri_protected_mask                                                                                 &&
          (u_dosmatch_with_OR(U_HTTP_URI_TO_PARAM,              U_STRING_TO_PARAM(*uri_protected_mask), 0)   ||
           (request_uri->empty() == false                                                                    &&
            u_dosmatch_with_OR(U_STRING_TO_PARAM(*request_uri), U_STRING_TO_PARAM(*uri_protected_mask), 0))) &&
          checkUriProtected() == false)
         {
         U_RETURN(U_PLUGIN_HANDLER_ERROR);
         }

      if (U_http_upgrade)
         {
         if (U_http_websocket)
            {
                 if (U_http_websocket_len == 0) U_http_websocket = 0;
            else if (UWebSocket::sendAccept() == false)
               {
               setBadRequest();

               U_RETURN(U_PLUGIN_HANDLER_ERROR);
               }
            }
         }
      }

   U_RETURN(U_PLUGIN_HANDLER_FINISHED);
}

/*
Set-Cookie: NAME=VALUE; expires=DATE; path=PATH; domain=DOMAIN_NAME; secure

NAME=VALUE
------------------------------------------------------------------------------------------------------------------------------------
This string is a sequence of characters excluding semi-colon, comma and white space. If there is a need to place such data
in the name or value, some encoding method such as URL style %XX encoding is recommended, though no encoding is defined or required.
This is the only required attribute on the Set-Cookie header.
------------------------------------------------------------------------------------------------------------------------------------

expires=DATE
------------------------------------------------------------------------------------------------------------------------------------
The expires attribute specifies a date string that defines the valid life time of that cookie. Once the expiration date has been
reached, the cookie will no longer be stored or given out.
The date string is formatted as: Wdy, DD-Mon-YYYY HH:MM:SS GMT
expires is an optional attribute. If not specified, the cookie will expire when the user's session ends.

Note: There is a bug in Netscape Navigator version 1.1 and earlier. Only cookies whose path attribute is set explicitly to "/" will
be properly saved between sessions if they have an expires attribute.
------------------------------------------------------------------------------------------------------------------------------------

domain=DOMAIN_NAME
------------------------------------------------------------------------------------------------------------------------------------
When searching the cookie list for valid cookies, a comparison of the domain attributes of the cookie is made with the Internet
domain name of the host from which the URL will be fetched. If there is a tail match, then the cookie will go through path matching
to see if it should be sent. "Tail matching" means that domain attribute is matched against the tail of the fully qualified domain
name of the host. A domain attribute of "acme.com" would match host names "anvil.acme.com" as well as "shipping.crate.acme.com".

Only hosts within the specified domain can set a cookie for a domain and domains must have at least two (2) or three (3) periods in
them to prevent domains of the form: ".com", ".edu", and "va.us". Any domain that fails within one of the seven special top level
domains listed below only require two periods. Any other domain requires at least three. The seven special top level domains are:
"COM", "EDU", "NET", "ORG", "GOV", "MIL", and "INT".

The default value of domain is the host name of the server which generated the cookie response.
------------------------------------------------------------------------------------------------------------------------------------

path=PATH
------------------------------------------------------------------------------------------------------------------------------------
The path attribute is used to specify the subset of URLs in a domain for which the cookie is valid. If a cookie has already passed
domain matching, then the pathname component of the URL is compared with the path attribute, and if there is a match, the cookie is
considered valid and is sent along with the URL request. The path "/foo" would match "/foobar" and "/foo/bar.html". The path "/" is
the most general path.

If the path is not specified, it as assumed to be the same path as the document being described by the header which contains the
cookie.

secure
------------------------------------------------------------------------------------------------------------------------------------
If a cookie is marked secure, it will only be transmitted if the communications channel with the host is a secure one. Currently
this means that secure cookies will only be sent to HTTPS (HTTP over SSL) servers.

If secure is not specified, a cookie is considered safe to be sent in the clear over unsecured channels. 
------------------------------------------------------------------------------------------------------------------------------------

HttpOnly cookies are a Microsoft extension to the cookie standard. The idea is that cookies marked as httpOnly cannot be accessed
from JavaScript. This was implemented to stop cookie stealing through XSS vulnerabilities. This is unlike many people believe not
a way to stop XSS vulnerabilities, but a way to stop one of the possible attacks (cookie stealing) that are possible through XSS.
*/

void UHTTP::setCookie(const UString& param)
{
   U_TRACE(0, "UHTTP::setCookie(%.*S)", U_STRING_TO_TRACE(param))

   time_t expire;
   uint32_t n_hours;
   UVector<UString> vec(param);
   UString item, cookie(U_CAPACITY);

   // param: "[ data expire path domain secure HttpOnly ]"
   // -----------------------------------------------------------------------------------------------------------------------------------
   // string -- key_id or data to put in cookie    -- must
   // int    -- lifetime of the cookie in HOURS    -- must (0 -> valid until browser exit)
   // string -- path where the cookie can be used  --  opt
   // string -- domain which can read the cookie   --  opt
   // bool   -- secure mode                        --  opt
   // bool   -- only allow HTTP usage              --  opt
   // -----------------------------------------------------------------------------------------------------------------------------------
   // RET: Set-Cookie: ulib.s<counter>=data&expire&HMAC-MD5(data&expire); expires=expire(GMT); path=path; domain=domain; secure; HttpOnly

   for (uint32_t i = 0, n = vec.size(); i < n; ++i)
      {
      item = vec[i];

      switch (i)
         {
         case 0:
            {
            // string -- key_id or data to put in cookie -- must

            if (item.empty())
               {
               U_INTERNAL_ASSERT_EQUALS((bool)*keyID, false)

               keyID->setBuffer(100U);

               keyID->snprintf("%s_%u_%P_%u", UServer_Base::client_address, getUserAgent(), ++sid_counter_gen);

               item = *keyID;
               }

            // int -- lifetime of the cookie in HOURS -- must (0 -> valid until browser exit)

            n_hours = (++i < n ? vec[i].strtol() : 0);
            expire  = (n_hours ? u_now->tv_sec + (n_hours * 60L * 60L) : 0L);

            cookie.snprintf("ulib.s%u=", sid_counter_gen);

            (void) cookie.append(UServices::generateToken(item, expire)); // HMAC-MD5(data&expire)

            if (n_hours) cookie.snprintf_add("; expires=%#12D", expire);
            }
         break;

         case 2:
            {
            // string -- path where the cookie can be used -- opt

            if (item.empty() == false) cookie.snprintf_add("; path=%.*s", U_STRING_TO_TRACE(item));
            }
         break;

         case 3:
            {
            // string -- domain which can read the cookie -- opt

            if (item.empty() == false) cookie.snprintf_add("; domain=%.*s", U_STRING_TO_TRACE(item));
            }
         break;

         case 4:
            {
            // bool -- secure mode -- opt

            if (item.strtob()) (void) cookie.append(U_CONSTANT_TO_PARAM("; secure"));
            }
         break;

         case 5:
            {
            // bool -- only allow HTTP usage -- opt

            if (item.strtob()) (void) cookie.append(U_CONSTANT_TO_PARAM("; HttpOnly"));
            }
         break;
         }
      }

   U_SRV_LOG("Create new session ulib.s%u", sid_counter_gen);

   addSetCookie(cookie);
}

// HTTP session

void UHTTP::addSetCookie(const UString& cookie)
{
   U_TRACE(0, "UHTTP::addSetCookie(%.*S)", U_STRING_TO_TRACE(cookie))

   U_INTERNAL_ASSERT(cookie)

   if (set_cookie->empty() == false) (void) set_cookie->append(U_CONSTANT_TO_PARAM("\r\n"));
                                     (void) set_cookie->append(U_CONSTANT_TO_PARAM("Set-Cookie: "));
                                     (void) set_cookie->append(cookie);

   U_INTERNAL_DUMP("set_cookie = %.*S", U_STRING_TO_TRACE(*set_cookie))
}

void UHTTP::removeDataSession()
{
   U_TRACE(0, "UHTTP::removeDataSession()")

   if (keyID->empty() == false ||
       (u_http_info.cookie_len &&
       getDataSession(0,0)))
      {
      removeDataSession(*keyID);

      keyID->clear();
      }
}

void UHTTP::setSessionCookie(UString* param)
{
   U_TRACE(0, "UHTTP::setSessionCookie(%p)", param)

   U_INTERNAL_DUMP("keyID = %.*S", U_STRING_TO_TRACE(*keyID))

   if (param)
      {
      removeDataSession();

      setCookie(*param);
      }
   else if (keyID->empty()) setCookie(*cookie_option);
}

bool UHTTP::getCookie(UString* cookie)
{
   U_TRACE(0, "UHTTP::getCookie(%p)", cookie)

   U_INTERNAL_ASSERT_MAJOR(u_http_info.cookie_len, 0)

   U_INTERNAL_DUMP("keyID = %.*S", U_STRING_TO_TRACE(*keyID))

   U_INTERNAL_ASSERT_EQUALS((bool)*keyID, false)

   char* ptr;
   bool check;
   time_t expire;
   const char* start;
   uint32_t len, agent;
   UString cookies(u_http_info.cookie, u_http_info.cookie_len), item, value, token;
   UVector<UString> cookie_list; // NB: must be here to avoid DEAD OF SOURCE STRING WITH CHILD ALIVE...

   for (uint32_t i = 0, n = cookie_list.split(cookies, ';'); i < n; ++i)
      {
      item = cookie_list[i];

      item.trim();

      U_INTERNAL_DUMP("cookie[%u] = %.*S", i, U_STRING_TO_TRACE(item))

      start = item.data();

      if (U_STRNEQ(start, "ulib.s"))
         {
         sid_counter_cur = strtol(start + U_CONSTANT_SIZE("ulib.s"), &ptr, 0);

         U_INTERNAL_DUMP("ptr[0] = %C", ptr[0])

         U_INTERNAL_ASSERT_EQUALS(ptr[0], '=')

         len = item.size() - (++ptr - start);

         (void) value.assign(ptr, len);

         check = false;

         /* XSRF (cross-site request forgery) is a problem that can be solved by using Crumbs.
          * Crumbs is a large alphanumeric string you pass between each on your site, and it is
          * a timestamp + a HMAC-MD5 encoded result of your IP address and browser user agent and a
          * pre-defined key known only to the web server. So, the application checks the crumb
          * value on every page, and the crumb value has a specific expiration time and also is
          * based on IP and browser, so it is difficult to forge. If the crumb value is wrong,
          * then it just prevents the user from viewing that page
          */

         token.setBuffer(100U);

         if (UServices::getTokenData(token, value, expire))
            {
            if (token.first_char() == '$') check = true; // session shared... (ex. chat)
            else
               {
               // HTTP Session Hijacking mitigation: IP_USER-AGENT_PID_COUNTER

               len = u__strlen(UServer_Base::client_address, __PRETTY_FUNCTION__);

               if (token.compare(0U, len, UServer_Base::client_address, len) == 0) // IP
                  {
                  agent = strtol(token.c_pointer(len+1), &ptr, 0);

                  U_INTERNAL_DUMP("ptr[0] = %C", ptr[0])

                  U_INTERNAL_ASSERT_EQUALS(ptr[0], '_')

                  if (agent == getUserAgent()) // USER-AGENT
                     {
                     if (UServer_Base::preforked_num_kids ||
                         memcmp(ptr+1, u_pid_str, u_pid_str_len) == 0) // PID
                        {
                        do { ++ptr; } while (*ptr != '_');

                        check = (sid_counter_cur == (uint32_t)strtol(ptr+1, 0, 0)); // COUNTER
                        }
                     }
                  }
               }
            }

         checkDataSession(token, expire, check);
         }
      else if (cookie)
         {
         if (cookie->empty() == false) (void) cookie->append(U_CONSTANT_TO_PARAM("; "));
                                       (void) cookie->append(item);
         }
      }

   if (keyID->empty() == false) U_RETURN(true);

   U_RETURN(false);
}

bool UHTTP::initSession(const char* location, uint32_t size)
{
   U_TRACE(0, "UHTTP::initSession(%S,%u)", location, size)

   U_INTERNAL_ASSERT_EQUALS(db_session, 0)

   if (UServer_Base::preforked_num_kids == 0)
      {
      db_session = U_NEW(UHashMap<UDataSession*>);

      ((UHashMap<UDataSession*>*)db_session)->allocate(U_GET_NEXT_PRIME_NUMBER(size));
      }
   else
      {
      // NB: the old sessions are automatically invalid because UServer generate the crypto key at startup...

      UString pathdb(U_CAPACITY);

      pathdb.snprintf("%s%s", (location[0] == '/' ? "" : U_LIBEXECDIR "/"), location);

      db_session = U_NEW(URDB(pathdb, false));

      bool btruncate = (UServer_Base::pid == 0); // NB: no truncate, if we have started as replacement of child preforked...

      if (((URDB*)db_session)->open(size, btruncate, true) == false)
         {
         U_SRV_LOG("DB initialization of http session failed...");

         delete (URDB*)db_session;
                       db_session = 0;

         U_RETURN(false);
         }

      U_INTERNAL_ASSERT_POINTER(U_LOCK_HTTP_SESSION)

      ((URDB*)db_session)->setShared(U_LOCK_HTTP_SESSION);
      }

   U_SRV_LOG("DB initialization of http session %s success", location);

   if (data_session == 0) data_session = U_NEW(UDataSession);
   if (data_storage == 0) data_storage = U_NEW(UDataSession);

   U_RETURN(true);
}

U_NO_EXPORT void UHTTP::deleteSession()
{
   U_TRACE(0, "UHTTP::deleteSession()")

   U_INTERNAL_ASSERT_POINTER(db_session)
   U_INTERNAL_ASSERT_POINTER(data_session)
   U_INTERNAL_ASSERT_POINTER(data_storage)

   delete data_session;
   delete data_storage;

   if (UServer_Base::preforked_num_kids == 0)
      {
      ((UHashMap<UDataSession*>*)db_session)->clear();
      ((UHashMap<UDataSession*>*)db_session)->deallocate();

      delete (UHashMap<UDataSession*>*)db_session;
      }
   else
      {
      ((URDB*)db_session)->close();

      delete (URDB*)db_session;
      }
}

U_NO_EXPORT void UHTTP::removeDataSession(const UString& token)
{
   U_TRACE(0, "UHTTP::removeDataSession(%.*S)", U_STRING_TO_TRACE(token))

   U_INTERNAL_ASSERT(token)

   UString cookie(100U);

   cookie.snprintf("ulib.s%u=; expires=%#12D", sid_counter_cur, u_now->tv_sec - U_ONE_DAY_IN_SECOND);

   addSetCookie(cookie);

   U_SRV_LOG("Delete session ulib.s%u keyID=%.*S", sid_counter_cur, U_STRING_TO_TRACE(token));

   if (db_session)
      {
#  ifdef U_HTTP_CACHE_REQUEST
      U_http_no_cache = true;
#  endif

      if (UServer_Base::preforked_num_kids == 0) (void) ((UHashMap<UDataSession*>*)db_session)->erase(token);
      else
         {
         int result = ((URDB*)db_session)->remove(token);

         if (result) U_SRV_LOG("Remove of session data on db failed with error %d", result);
         }
      }
}

U_NO_EXPORT void UHTTP::checkDataSession(const UString& token, time_t expire, bool checked)
{
   U_TRACE(0, "UHTTP::checkDataSession(%.*S,%ld,%b)", U_STRING_TO_TRACE(token), expire, checked)

   if (checked)
      {
      U_INTERNAL_ASSERT(token)

      if (keyID->empty() == false) // NB: previous valid cookie...
         {
         data_session->clear();

         goto remove;
         }

      if (db_session)
         {
         U_INTERNAL_ASSERT_POINTER(data_session)

         if (UServer_Base::preforked_num_kids == 0)
            {
            UDataSession* data = (*(UHashMap<UDataSession*>*)db_session)[token];

            if (data) data_session->fromDataSession(*data);
            }
         else
            {
            UString data = (*(URDB*)db_session)[token];

            if (data.empty() == false)
               {
               data_session->fromString(data);

               U_INTERNAL_DUMP("data                     = %.*S", U_STRING_TO_TRACE(data))
               U_DUMP(         "data_session->toString() = %.*S", U_STRING_TO_TRACE(data_session->toString()))

               U_ASSERT_EQUALS(data, data_session->toString())
               }
            }

         if (expire == 0 && // 0 -> valid until browser exit
             (data_session->last_access - data_session->creation) > U_ONE_DAY_IN_SECOND)
            {
            data_session->clear();

            goto remove;
            }
         }

      U_INTERNAL_ASSERT_EQUALS((bool)*keyID, false)

      *keyID = token;

      U_SRV_LOG("Found session ulib.s%u", sid_counter_cur);
      }
   else
      {
remove:
      removeDataSession(token);
      }
}

bool UHTTP::getDataSession(uint32_t index, UString* value)
{
   U_TRACE(0, "UHTTP::getDataSession(%u,%p)", index, value)

   if (db_session ||
       initSession("session.http", 1024 * 1024))
      {
      U_INTERNAL_ASSERT_POINTER(db_session)
      U_INTERNAL_ASSERT_POINTER(data_session)

      U_INTERNAL_DUMP("keyID = %.*S", U_STRING_TO_TRACE(*keyID))

      if (keyID->empty() == false) goto next;

      data_session->clear();

      if (u_http_info.cookie_len &&
          getCookie(0))
         {
         U_INTERNAL_ASSERT(*keyID)
next:
         if (value &&
             data_session->getValue(index, *value) == false)
            {
            U_RETURN(false);
            }

         U_RETURN(true);
         }
      }

   U_RETURN(false);
}

bool UHTTP::getDataStorage(uint32_t index, UString* value)
{
   U_TRACE(0, "UHTTP::getDataStorage(%u,%p)", index, value)

   if (db_session ||
       initSession("session.http", 1024 * 1024))
      {
      U_INTERNAL_ASSERT_POINTER(db_session)
      U_INTERNAL_ASSERT_POINTER(data_storage)
      U_INTERNAL_ASSERT_POINTER(UHashMap<void*>::pkey)

      data_storage->clear();

      UHashMap<void*>::pkey->str     =                 U_STORAGE_KEYID;
      UHashMap<void*>::pkey->_length = U_CONSTANT_SIZE(U_STORAGE_KEYID);

      if (UServer_Base::preforked_num_kids == 0)
         {
         UDataSession* data = (*(UHashMap<UDataSession*>*)db_session)[UHashMap<void*>::pkey];

         if (data) data_storage->fromDataSession(*data);
         }
      else
         {
         UString data = (*(URDB*)db_session)[UHashMap<void*>::pkey];

         if (data.empty() == false)
            {
            data_storage->fromString(data);

            U_INTERNAL_DUMP("data                     = %.*S", U_STRING_TO_TRACE(data))
            U_DUMP(         "data_storage->toString() = %.*S", U_STRING_TO_TRACE(data_storage->toString()))

            U_ASSERT_EQUALS(data, data_storage->toString())
            }
         }

      if (value &&
          data_storage->getValue(index, *value) == false)
         {
         U_RETURN(false);
         }

      U_RETURN(true);
      }

   U_RETURN(false);
}

void UHTTP::putDataSession(uint32_t index, const char* value, uint32_t size)
{
   U_TRACE(0, "UHTTP::putDataSession(%u,%.*S,%u)", index, size, value, size)

   U_INTERNAL_ASSERT_POINTER(db_session)
   U_INTERNAL_ASSERT_POINTER(data_session)

   U_INTERNAL_DUMP("keyID = %.*S", U_STRING_TO_TRACE(*keyID))

   U_INTERNAL_ASSERT(*keyID)

#ifdef U_HTTP_CACHE_REQUEST
   U_http_no_cache = true;
#endif

   if (size)
      {
      UString _value((void*)value, size);

      data_session->putValue(index, _value);
      }

   if (UServer_Base::preforked_num_kids == 0)
      {
      UDataSession* data = (*(UHashMap<UDataSession*>*)db_session)[*keyID];

      if (data == 0)
         {
         data = data_session->toDataSession();

         U_INTERNAL_ASSERT_POINTER(data)

         ((UHashMap<UDataSession*>*)db_session)->insertAfterFind(*keyID, data);
         }
      }
   else
      {
      UString data = data_session->toString();

      U_INTERNAL_ASSERT(data)

      int result = ((URDB*)db_session)->store(*keyID, data, RDB_REPLACE);

      if (result) U_SRV_LOG("Store of session data on db failed with error %d", result);
      }
}

void UHTTP::putDataStorage(uint32_t index, const char* value, uint32_t size)
{
   U_TRACE(0, "UHTTP::putDataStorage(%u,%.*S,%u)", index, size, value, size)

   U_INTERNAL_ASSERT_POINTER(db_session)
   U_INTERNAL_ASSERT_POINTER(data_storage)
   U_INTERNAL_ASSERT_POINTER(UHashMap<void*>::pkey)

#ifdef U_HTTP_CACHE_REQUEST
   U_http_no_cache = true;
#endif

   if (size)
      {
      UString _value((void*)value, size);

      data_storage->putValue(index, _value);
      }

   UHashMap<void*>::pkey->str     =                 U_STORAGE_KEYID;
   UHashMap<void*>::pkey->_length = U_CONSTANT_SIZE(U_STORAGE_KEYID);

   if (UServer_Base::preforked_num_kids == 0)
      {
      UDataSession* data = (*(UHashMap<UDataSession*>*)db_session)[UHashMap<void*>::pkey];

      if (data == 0)
         {
         data = data_storage->toDataSession();

         U_INTERNAL_ASSERT_POINTER(data)

         UStringRep* rep = U_NEW(UStringRep(U_CONSTANT_TO_PARAM(U_STORAGE_KEYID)));

         ((UHashMap<UDataSession*>*)db_session)->insertAfterFind(rep, data);
         }
      }
   else
      {
      UString data = data_storage->toString();

      U_INTERNAL_ASSERT(data)

      int result = ((URDB*)db_session)->store(UHashMap<void*>::pkey, data, RDB_REPLACE);

      if (result) U_SRV_LOG("Store of data on db failed with error %d", result);
      }
}

const char* UHTTP::getStatusDescription(uint32_t nResponseCode)
{
   U_TRACE(0, "UHTTP::getStatusDescription(%u)", nResponseCode)

   const char* descr;

   switch (nResponseCode)
      {
      // 1xx indicates an informational message only
      case HTTP_CONTINUE:                        descr = "Continue";                        break;
      case HTTP_SWITCH_PROT:                     descr = "Switching Protocol";              break;
   // case 102:                                  descr = "HTTP Processing";                 break;

      // 2xx indicates success of some kind
      case HTTP_OK:                              descr = "OK";                              break;
      case HTTP_CREATED:                         descr = "Created";                         break;
      case HTTP_ACCEPTED:                        descr = "Accepted";                        break;
      case HTTP_NOT_AUTHORITATIVE:               descr = "Non-Authoritative Information";   break;
      case HTTP_NO_CONTENT:                      descr = "No Content";                      break;
      case HTTP_RESET:                           descr = "Reset Content";                   break;
      case HTTP_PARTIAL:                         descr = "Partial Content";                 break;
   // case 207:                                  descr = "Webdav Multi-status";             break;

      // 3xx Redirection - Further action must be taken in order to complete the request
      case HTTP_MULT_CHOICE:                     descr = "Multiple Choices";                break;
      case HTTP_MOVED_PERM:                      descr = "Moved Permanently";               break;
      case HTTP_MOVED_TEMP:                      descr = "Moved Temporarily";               break;
   // case HTTP_FOUND:                           descr = "Found [Elsewhere]";               break;
      case HTTP_SEE_OTHER:                       descr = "See Other";                       break;
      case HTTP_NOT_MODIFIED:                    descr = "Not Modified";                    break;
      case HTTP_USE_PROXY:                       descr = "Use Proxy";                       break;
      case HTTP_TEMP_REDIR:                      descr = "Temporary Redirect";              break;

      // 4xx indicates an error on the client's part
      case HTTP_BAD_REQUEST:                     descr = "Bad Request";                     break;
      case HTTP_UNAUTHORIZED:                    descr = "Authorization Required";          break;
      case HTTP_PAYMENT_REQUIRED:                descr = "Payment Required";                break;
      case HTTP_FORBIDDEN:                       descr = "Forbidden";                       break;
      case HTTP_NOT_FOUND:                       descr = "Not Found";                       break;
      case HTTP_BAD_METHOD:                      descr = "Method Not Allowed";              break;
      case HTTP_NOT_ACCEPTABLE:                  descr = "Not Acceptable";                  break;
      case HTTP_PROXY_AUTH:                      descr = "Proxy Authentication Required";   break;
      case HTTP_CLIENT_TIMEOUT:                  descr = "Request Time-out";                break;
      case HTTP_CONFLICT:                        descr = "Conflict";                        break;
      case HTTP_GONE:                            descr = "Gone";                            break;
      case HTTP_LENGTH_REQUIRED:                 descr = "Length Required";                 break;
      case HTTP_PRECON_FAILED:                   descr = "Precondition Failed";             break;
      case HTTP_ENTITY_TOO_LARGE:                descr = "Request Entity Too Large";        break;
      case HTTP_REQ_TOO_LONG:                    descr = "Request-URI Too Long";            break;
      case HTTP_UNSUPPORTED_TYPE:                descr = "Unsupported Media Type";          break;
      case HTTP_REQ_RANGE_NOT_OK:                descr = "Requested Range not satisfiable"; break;
      case HTTP_EXPECTATION_FAILED:              descr = "Expectation Failed";              break;
      case HTTP_UNPROCESSABLE_ENTITY:            descr = "Unprocessable Entity";            break;
   // case 423:                                  descr = "Locked";                          break;
   // case 424:                                  descr = "Failed Dependency";               break;
   // case 425:                                  descr = "No Matching Vhost";               break;
   // case 426:                                  descr = "Upgrade Required";                break;
      case HTTP_PRECONDITION_REQUIRED:           descr = "Precondition required";           break;
      case HTTP_TOO_MANY_REQUESTS:               descr = "Too many requests";               break;
      case HTTP_REQUEST_HEADER_FIELDS_TOO_LARGE: descr = "Request_header_fields_too_large"; break;
      case HTTP_NO_RESPONSE:                     descr = "No Response";                     break;
   // case 449:                                  descr = "Retry With Appropriate Action";   break;

      // 5xx indicates an error on the server's part
      case HTTP_INTERNAL_ERROR:                  descr = "Internal Server Error";           break;
      case HTTP_NOT_IMPLEMENTED:                 descr = "Not Implemented";                 break;
      case HTTP_BAD_GATEWAY:                     descr = "Bad Gateway";                     break;
      case HTTP_UNAVAILABLE:                     descr = "Service Unavailable";             break;
      case HTTP_GATEWAY_TIMEOUT:                 descr = "Gateway Time-out";                break;
      case HTTP_VERSION:                         descr = "HTTP Version Not Supported";      break;
   // case 506:                                  descr = "Variant also varies";             break;
   // case 507:                                  descr = "Insufficient Storage";            break;
   // case 510:                                  descr = "Not Extended";                    break;
      case HTTP_NETWORK_AUTHENTICATION_REQUIRED: descr = "Network authentication required"; break;

      default:                                   descr = "Code unknown";                    break;
      }

   U_RETURN(descr);
}

const char* UHTTP::getStatus()
{
   U_TRACE(0, "UHTTP::getStatus()")

   U_INTERNAL_ASSERT_EQUALS(u_buffer_len, 0)

   (void) sprintf(u_buffer, "(%d, %s)", u_http_info.nResponseCode, getStatusDescription(u_http_info.nResponseCode));

   U_RETURN(u_buffer);
}

U_NO_EXPORT UString UHTTP::getHTMLDirectoryList()
{
   U_TRACE(0, "UHTTP::getHTMLDirectoryList()")

   U_INTERNAL_ASSERT(file->pathname.isNullTerminated())

   bool is_dir;
   uint32_t i, pos, n;
   UDirWalk dirwalk(0);
   UVector<UString> vec(2048);
   UString buffer(4000U), item, size, entry(4000U), value_encoded(U_CAPACITY);

   int32_t len     = file->getPathRelativLen();
   const char* ptr = file->getPathRelativ();
   bool broot      = (len == 1 && ptr[0] == '/');

   if (broot)
      {
      (void) buffer.assign(U_CONSTANT_TO_PARAM(
         "<html><head><title>Index of /</title></head>"
         "<body><h1>Index of directory: /</h1><hr>"
         "<table><tr>"
         "<td></td>"
         "<td></td>"
         "</tr>"));
      }
   else
      {
      buffer.snprintf(
         "<html><head><title>Index of %s</title></head>"
         "<body><h1>Index of directory: %s</h1><hr>"
         "<table><tr>"
         "<td><a href=\"/%s/..?_nav_\"><img width=\"20\" height=\"21\" align=\"absbottom\" border=\"0\" src=\"/icons/menu.png\"> Up one level</a></td>"
         "<td></td>"
         "<td></td>"
         "</tr>", ptr, ptr, ptr);

      if (dirwalk.setDirectory(ptr) == false) goto end;
      }

   n = dirwalk.walk(vec);

   if (n > 1) vec.sort();

   pos = buffer.size();

   for (i = 0; i < n; ++i)
      {
      item      = vec[i];
      file_data = (*cache_file)[item];

      if (file_data == 0) continue; // NB: this can happen (servlet for example...)

      is_dir = S_ISDIR(file_data->mode);

      Url::encode(item, value_encoded);

      size = UStringExt::numberToString(file_data->size, true);

      entry.snprintf(
         "<tr>"
            "<td><strong>"
               "<a href=\"/%.*s?_nav_\"><img width=\"20\" height=\"21\" align=\"absbottom\" border=\"0\" src=\"/icons/%s\"> %.*s</a>"
            "</strong></td>"
            "<td align=\"right\" valign=\"bottom\">%.*s</td>"
            "<td align=\"right\" valign=\"bottom\">%#3D</td>"
         "</tr>",
         U_STRING_TO_TRACE(value_encoded), (is_dir ? "menu.png" : "gopher-unknown.gif"), U_STRING_TO_TRACE(item),
         U_STRING_TO_TRACE(size),
         file_data->mtime);

      if (is_dir)
         {
         (void) buffer.insert(pos, entry);

         pos += entry.size();
         }
      else
         {
         (void) buffer.append(entry);
         }
      }

end:
   (void) buffer.append(U_CONSTANT_TO_PARAM("</table><hr><address>ULib Server</address></body></html>"));

   U_INTERNAL_DUMP("buffer(%u) = %.*S", buffer.size(), U_STRING_TO_TRACE(buffer))

   U_RETURN_STRING(buffer);
}

void UHTTP::resetForm(bool brmdir)
{
   U_TRACE(0, "UHTTP::resetForm(%b)", brmdir)

   U_INTERNAL_ASSERT_POINTER(tmpdir)
   U_INTERNAL_ASSERT_POINTER(qcontent)
   U_INTERNAL_ASSERT_POINTER(formMulti)
   U_INTERNAL_ASSERT_POINTER(form_name_value)

   form_name_value->clear();

   if (qcontent->empty() == false) qcontent->clear();
   else
      {
      // clean temporary directory, if any...

      if (tmpdir->empty() == false)
         {
         if (brmdir) (void) UFile::rmdir(tmpdir->data(), true);

         tmpdir->setEmpty();
         }

      if (formMulti->isEmpty() == false) formMulti->clear();
      }
}

// retrieve information on specific HTML form elements
// (such as checkboxes, radio buttons, and text fields, or uploaded files)

void UHTTP::getFormValue(UString& buffer, uint32_t n)
{
   U_TRACE(0, "UHTTP::getFormValue(%.*S,%u)", U_STRING_TO_TRACE(buffer), n)

   U_INTERNAL_ASSERT_POINTER(form_name_value)

   if (n >= form_name_value->size()) buffer.clear();
   else                       (void) buffer.replace((*form_name_value)[n]);
}

void UHTTP::getFormValue(UString& buffer, const char* name, uint32_t len)
{
   U_TRACE(0, "UHTTP::getFormValue(%.*S,%.*S,%u)", U_STRING_TO_TRACE(buffer), len, name, len)

   U_INTERNAL_ASSERT_POINTER(form_name_value)

   uint32_t index = form_name_value->find(name, len);

   if (index == U_NOT_FOUND) buffer.clear();
   else               (void) buffer.replace((*form_name_value)[index+1]);
}

UString UHTTP::getFormValue(const char* name, uint32_t len, uint32_t start, uint32_t end)
{
   U_TRACE(0, "UHTTP::getFormValue(%.*S,%u,%u,%u)", len, name, len, start, end)

   U_INTERNAL_ASSERT(start <= end)
   U_INTERNAL_ASSERT_POINTER(form_name_value)

   UString buffer;
   uint32_t index = form_name_value->findRange(name, len, start, end);

   if (index != U_NOT_FOUND) (void) buffer.replace((*form_name_value)[index+1]);

   U_RETURN_STRING(buffer);
}

void UHTTP::getFormValue(UString& buffer, const char* name, uint32_t len, uint32_t start, uint32_t pos, uint32_t end)
{
   U_TRACE(0, "UHTTP::getFormValue(%.*S,%.*S,%u,%u,%u,%u)", U_STRING_TO_TRACE(buffer), len, name, len, start, pos, end)

   U_INTERNAL_ASSERT(start <= end)
   U_INTERNAL_ASSERT_POINTER(form_name_value)

   if (pos >= end) buffer.clear();
   else
      {
      UStringRep* r = form_name_value->UVector<UStringRep*>::at(pos-1);

      if (r->equal(name, len) == false) getFormValue(buffer, name, len);
      else
         {
         (void) buffer.replace((*form_name_value)[pos]);

         U_ASSERT_EQUALS(buffer, getFormValue(name, len, start, end))
         }
      }
}

uint32_t UHTTP::processForm()
{
   U_TRACE(0, "UHTTP::processForm()")

   U_ASSERT_EQUALS(form_name_value->size(), 0)

   uint32_t len = 0;

   if (isGETorHEAD())
      {
      *qcontent = UString(U_HTTP_QUERY_TO_PARAM);

      goto get_name_value;
      }

   // ------------------------------------------------------------------------
   // POST
   // ------------------------------------------------------------------------
   // Content-Type: application/x-www-form-urlencoded OR multipart/form-data...
   // ------------------------------------------------------------------------

   U_ASSERT(isPOST())

   if (U_HTTP_CTYPE_STRNEQ("application/x-www-form-urlencoded"))
      {
      *qcontent = *UClientImage_Base::body;

      goto get_name_value;
      }

   // multipart/form-data (FILE UPLOAD)

   U_INTERNAL_ASSERT(U_HTTP_CTYPE_STRNEQ("multipart/form-data"))

   {
   UString boundary, tmp = UClientImage_Base::body->substr(2);
   UTokenizer(tmp, u_line_terminator).next(boundary, (bool*)0);

   formMulti->setBoundary(boundary);
   formMulti->setContent(*UClientImage_Base::body);

   if (formMulti->parse() == false) U_RETURN(0);
   }

   // create temporary directory with files uploaded...

   {
   tmpdir->snprintf("%s/formXXXXXX", u_tmpdir);

   if (UFile::mkdtemp(*tmpdir) == false) U_RETURN(0);

   UMimeEntity* item;
   uint32_t sz = tmpdir->size();
   const char* ptr = tmpdir->data();
   UString content, name, filename, basename;

   for (uint32_t i = 0, j = formMulti->getNumBodyPart(); i < j; ++i)
      {
      item    = (*formMulti)[i];
      content = item->getContent();

      // Content-Disposition: form-data; name="input_file"; filename="/tmp/4dcd39e8-2a84-4242-b7bc-ca74922d26e1"

      if (UMimeHeader::getNames(item->getContentDisposition(), name, filename))
         {
         // NB: we can't reuse the same string (filename) to avoid DEAD OF SOURCE STRING WITH CHILD ALIVE...

         basename = UStringExt::basename(filename);

         len = basename.size();

         pathname->setBuffer(sz + 1 + len);

         pathname->snprintf("%.*s/%.*s", sz, ptr, len, basename.data());

         (void) UFile::writeTo(*pathname, content);

         content = *pathname;
         }

      form_name_value->push_back(name);
      form_name_value->push_back(content);
      }

   len = form_name_value->size();

   U_RETURN(len);
   }

get_name_value:

   if (qcontent->empty() == false)
      {
      len = UStringExt::getNameValueFromData(*qcontent, *form_name_value, U_CONSTANT_TO_PARAM("&"));

      U_ASSERT_EQUALS(len, form_name_value->size())
      }

   U_RETURN(len);
}

// set HTTP main error message
// --------------------------------------------------------------------------------------------------------------------------------------

UString UHTTP::getHeaderForResponse(const UString& content, bool connection_close)
{
   U_TRACE(0, "UHTTP::getHeaderForResponse(%.*S,%b)", U_STRING_TO_TRACE(content), connection_close)

   U_INTERNAL_ASSERT_MAJOR(u_http_info.nResponseCode,0)
   
   // NB: All 1xx (informational), 204 (no content), and 304 (not modified) responses MUST not include a body...

   U_INTERNAL_DUMP("u_http_info.nResponseCode = %d", u_http_info.nResponseCode)

#ifdef DEBUG
   if ((u_http_info.nResponseCode >= 100  &&
        u_http_info.nResponseCode <  200) ||
        u_http_info.nResponseCode == 304)
      {
      U_INTERNAL_ASSERT_EQUALS((bool)content, false)
      }
#endif

   uint32_t sz     = 0;
   const char* ptr = 0;

   if (u_http_info.nResponseCode == HTTP_NOT_IMPLEMENTED ||
       u_http_info.nResponseCode == HTTP_OPTIONS_RESPONSE)
      {
      ptr =                 "Allow: GET, HEAD, POST, PUT, DELETE, COPY, OPTIONS\r\nContent-Length: 0\r\n\r\n";
      sz  = U_CONSTANT_SIZE("Allow: GET, HEAD, POST, PUT, DELETE, COPY, OPTIONS\r\nContent-Length: 0\r\n\r\n");

      if (u_http_info.nResponseCode == HTTP_OPTIONS_RESPONSE) u_http_info.nResponseCode = HTTP_OK;
      }
   else
      {
      // ...all other responses must include an entity body or a Content-Length header field defined with a value of zero (0)

      if ((sz = content.size())) ptr = content.data();
      else
         {
         ptr =                 "Content-Length: 0\r\n\r\n";
         sz  = U_CONSTANT_SIZE("Content-Length: 0\r\n\r\n");

         if (u_http_info.nResponseCode == HTTP_OK) u_http_info.nResponseCode = HTTP_NO_CONTENT;
         }
      }

   U_INTERNAL_DUMP("U_http_version = %C U_http_keep_alive = %C U_http_is_connection_close = %d",
                    U_http_version,     U_http_keep_alive,     U_http_is_connection_close)

   // HTTP/1.1 compliance: Sends Date on every requests...

   UString ext(200U);

   if (U_http_version == '1') ext.snprintf_add("Date: %12D\r\n", 0);
   else
      {
      // HTTP/1.0 compliance: if not Keep-Alive we force close...

      if (U_http_keep_alive == '\0') U_http_is_connection_close = U_YES;
      }

   if (u_http_info.nResponseCode >= 300) (void) ext.append(U_CONSTANT_TO_PARAM("Cache-Control: no-cache\r\n"));

   if (connection_close == false)
      {
      if (U_http_is_connection_close == U_YES)
         {
         if (UClientImage_Base::isPipeline() == false) (void) ext.append(U_CONSTANT_TO_PARAM("Connection: close\r\n"));
         }
      else
         {
         if (U_http_keep_alive == '1') (void) ext.append(U_CONSTANT_TO_PARAM("Connection: Keep-Alive\r\n"));

         if (U_http_version == '0')
            {
            /*
            Keep-Alive Timeout

            Description: Specifies the maximum idle time between requests from a Keep-Alive connection. If no new request is received during
                         this period of time, the connection will be closed.

            Syntax:      Integer number

            Tips: [Security & Performance] We recommend you to set the value just long enough to handle all requests for a single page view.
                  It is unnecessary to keep connection alive for an extended period of time. A smaller value can reduce idle connections, increase
                  capacity to service more users and guard against DoS attacks. 2-5 seconds is a reasonable range for most applications.
            */

            if (UServer_Base::getReqTimeout())
               {
               // ...to indicate that the session is being kept alive for a maximum of x requests and a per-request timeout of x seconds

               ext.snprintf_add("Keep-Alive: max=%u, timeout=%d\r\n", UNotifier::max_connection - UNotifier::min_connection, UServer_Base::getReqTimeout());
               }
            }
         }
      }

   UString tmp(300U + ext.size() + sz);

   tmp.snprintf(str_frm_header->data(),
                (U_http_version ? U_http_version : '0'),
                u_http_info.nResponseCode, getStatusDescription(u_http_info.nResponseCode),
                U_STRING_TO_TRACE(ext),
                sz, ptr);

   U_INTERNAL_DUMP("tmp(%u) = %.*S", tmp.size(), U_STRING_TO_TRACE(tmp))

   U_RETURN_STRING(tmp);
}

void UHTTP::setNoResponse()
{
   U_TRACE(0, "UHTTP::setNoResponse()")

   UClientImage_Base::write_off = true;

   u_http_info.nResponseCode = HTTP_NO_RESPONSE; 

   UClientImage_Base::body->clear();
   UClientImage_Base::wbuffer->clear();

   if (apache_like_log) writeApacheLikeLog(false);
}

void UHTTP::setResponse(const UString* content_type, const UString* body)
{
   U_TRACE(0, "UHTTP::setResponse(%p,%p)", content_type, body)

   U_INTERNAL_ASSERT_MAJOR(u_http_info.nResponseCode,0)

   UString tmp(U_CAPACITY);

   if (content_type)
      {
      U_INTERNAL_ASSERT(u_endsWith(U_STRING_TO_PARAM(*content_type), U_CONSTANT_TO_PARAM(U_CRLF)))

      if (body) tmp.snprintf("Content-Length: %u\r\n", body->size());

      (void) tmp.append(U_CONSTANT_TO_PARAM("Content-Type: "));
      (void) tmp.append(*content_type);
      (void) tmp.append(U_CONSTANT_TO_PARAM(U_CRLF));
      }

   if (body) *UClientImage_Base::body = *body;
   else       UClientImage_Base::body->clear(); // clean body to avoid writev() in response...

   *UClientImage_Base::wbuffer = getHeaderForResponse(tmp, false);

   U_INTERNAL_DUMP("UClientImage_Base::wbuffer(%u) = %.*S", UClientImage_Base::wbuffer->size(), U_STRING_TO_TRACE(*UClientImage_Base::wbuffer))
   U_INTERNAL_DUMP("UClientImage_Base::body(%u)    = %.*S", UClientImage_Base::body->size(),    U_STRING_TO_TRACE(*UClientImage_Base::body))
}

/* http://sebastians-pamphlets.com/the-anatomy-of-http-redirects-301-302-307/
 * ------------------------------------------------------------------------------------------------------------------
 * HTTP/1.0
 * ------------------------------------------------------------------------------------------------------------------
 * 302 Moved Temporarily
 *
 * The requested resource resides temporarily under a different URL. Since the redirection may be altered on occasion,
 * the client should continue to use the Request-URI for future requests. The URL must be given by the Location field
 * in the response. Unless it was a HEAD request, the Entity-Body of the response should contain a short note with a
 * hyperlink to the new URI(s).
 * ------------------------------------------------------------------------------------------------------------------
 * HTTP/1.1
 * ------------------------------------------------------------------------------------------------------------------
 * 302 Found [Elsewhere]
 *
 * The requested resource resides temporarily under a different URI. Since the redirection might be altered on occasion,
 * the client SHOULD continue to use the Request-URI for future requests. This response is only cacheable if indicated
 * by a Cache-Control or Expires header field. The temporary URI SHOULD be given by the Location field in the response.
 * Unless the request method was HEAD, the entity of the response SHOULD contain a short hypertext note with a hyperlink
 * to the new URI(s).
 *
 * 307 Temporary Redirect
 *
 * The requested resource resides temporarily under a different URI. Since the redirection MAY be altered on occasion,
 * the client SHOULD continue to use the Request-URI for future requests. This response is only cacheable if indicated by
 * a Cache-Control or Expires header field. The temporary URI SHOULD be given by the Location field in the response. Unless
 * the request method was HEAD, the entity of the response SHOULD contain a short hypertext note with a hyperlink to the new
 * URI(s), since many pre-HTTP/1.1 user agents do not understand the 307 status. Therefore, the note SHOULD contain the
 * information necessary for a user to repeat the original request on the new URI.
 */

void UHTTP::setRedirectResponse(int mode, const UString& ext, const char* ptr_location, uint32_t len_location)
{
   U_TRACE(0, "UHTTP::setRedirectResponse(%d,%.*S,%.*S,%u)", mode, U_STRING_TO_TRACE(ext), len_location, ptr_location, len_location)

   U_ASSERT_EQUALS(u_find(ptr_location,len_location,"\n",1),0)

   // NB: firefox ask confirmation to user with response 307 (HTTP_TEMP_REDIR)...

   u_http_info.nResponseCode = (mode == 1 ? HTTP_NETWORK_AUTHENTICATION_REQUIRED : HTTP_MOVED_TEMP);

   U_INTERNAL_DUMP("U_http_is_connection_close = %d", U_http_is_connection_close)

   U_http_is_connection_close = U_YES;

   UClientImage_Base::resetPipeline(); // NB: because we close the connection we don't need to process other request in pipeline... (ex. nodog)

   UString tmp(U_CAPACITY);

   (void) tmp.assign(U_CONSTANT_TO_PARAM(U_CTYPE_HTML "\r\n"));
   (void) tmp.append(mode == 1 ? "Refresh: 1; url=" : "Location: ");
   (void) tmp.append(ptr_location, len_location);
   (void) tmp.append(U_CONSTANT_TO_PARAM("\r\n"));

   if (ext.empty() == false)
      {
      U_INTERNAL_ASSERT(u_endsWith(U_STRING_TO_PARAM(ext), U_CONSTANT_TO_PARAM(U_CRLF)))

      (void) tmp.append(ext);
      }

   if (mode == 2) setResponse(&tmp, 0);
   else
      {
      U_INTERNAL_ASSERT(str_frm_body->isNullTerminated())

      UString msg(100U + len_location), body(500U + len_location);

      const char* status = getStatusDescription(u_http_info.nResponseCode);

      msg.snprintf(mode == 1 ? "You need to <a href=\"%.*s\">authenticate with the local network</a> in order to get access"
                             : "The document has moved <a href=\"%.*s\">here</a>",
                   len_location, ptr_location);

      body.snprintf(str_frm_body->data(),
                    u_http_info.nResponseCode, status,
                    status,
                    msg.data());

      setResponse(&tmp, &body);
      }
}

UString UHTTP::getUrlEncodedForResponse(const char* format)
{
   U_TRACE(0, "UHTTP::getUrlEncodedForResponse(%S)", format)

   UString result(800U);
   uint32_t sz = U_HTTP_URI_QUERY_LEN;

   if (sz == 0) (void) result.assign(U_CONSTANT_TO_PARAM("Your browser sent a request that this server could not understand"));
   else
      {
      char output[500U];

      UString _uri = getRequestURIWithQuery();

      // NB: encoding to avoid cross-site scripting (XSS)...

      sz = u_xml_encode((const unsigned char*)_uri.data(), U_min(100, _uri.size()), (unsigned char*)output);

      result.snprintf(format, sz, output);
      }

   U_RETURN_STRING(result);
}

void UHTTP::setBadRequest()
{
   U_TRACE(0, "UHTTP::setBadRequest()")

   UString body(1000U);

   U_http_is_connection_close = U_YES;
   u_http_info.nResponseCode  = HTTP_BAD_REQUEST;

   UFileCacheData* ptr_file_data = getFileInCache(U_CONSTANT_TO_PARAM("ErrorDocument/400.html"));

   if (ptr_file_data &&
       ptr_file_data->array != 0)
      {
      body = (*ptr_file_data->array)[0];
      }
   else
      {
      const char* status = getStatusDescription(HTTP_BAD_REQUEST);

      UString msg = getUrlEncodedForResponse("Your requested URL %.*S was a request that this server could not understand");

      U_INTERNAL_ASSERT(str_frm_body->isNullTerminated())

      body.snprintf(str_frm_body->data(),
                    HTTP_BAD_REQUEST, status,
                    status,
                    msg.data());
      }

   setResponse(str_ctype_html, &body);
}

void UHTTP::setUnAuthorized()
{
   U_TRACE(0, "UHTTP::setUnAuthorized()")

   UString body(1000U);

   u_http_info.nResponseCode = HTTP_UNAUTHORIZED;

   UFileCacheData* ptr_file_data = getFileInCache(U_CONSTANT_TO_PARAM("ErrorDocument/401.html"));

   if (ptr_file_data &&
       ptr_file_data->array != 0)
      {
      body = (*ptr_file_data->array)[0];
      }
   else
      {
      const char* status = getStatusDescription(HTTP_UNAUTHORIZED);

      U_INTERNAL_ASSERT(str_frm_body->isNullTerminated())

      body.snprintf(str_frm_body->data(),
                     HTTP_UNAUTHORIZED, status,
                     status,
                     "This server could not verify that you are authorized to access the document requested. Either you supplied the "
                     "wrong credentials (e.g., bad password), or your browser doesn't understand how to supply the credentials required");
      }

   UString ext(100U);

   (void) ext.assign(U_CONSTANT_TO_PARAM(U_CTYPE_HTML "\r\nWWW-Authenticate: "));

   if (digest_authentication)        ext.snprintf_add("Digest qop=\"auth\", nonce=\"%ld\", algorithm=MD5,", u_now->tv_sec);
   else                       (void) ext.append(U_CONSTANT_TO_PARAM("Basic"));

   (void) ext.append(U_CONSTANT_TO_PARAM(" realm=\"" U_HTTP_REALM "\"\r\n"));

   setResponse(&ext, &body);
}

void UHTTP::setForbidden()
{
   U_TRACE(0, "UHTTP::setForbidden()")

   UString body(1000U);

   u_http_info.nResponseCode = HTTP_FORBIDDEN;

   UFileCacheData* ptr_file_data = getFileInCache(U_CONSTANT_TO_PARAM("ErrorDocument/403.html"));

   if (ptr_file_data &&
       ptr_file_data->array != 0)
      {
      body = (*ptr_file_data->array)[0];
      }
   else
      {
      const char* status = getStatusDescription(HTTP_FORBIDDEN);

      UString msg = getUrlEncodedForResponse("You don't have permission to access %.*S on this server");

      U_INTERNAL_ASSERT(str_frm_body->isNullTerminated())

      body.snprintf(str_frm_body->data(),
                     HTTP_FORBIDDEN, status,
                     status,
                     msg.data());
      }

   setResponse(str_ctype_html, &body);
}

void UHTTP::setNotFound()
{
   U_TRACE(0, "UHTTP::setNotFound()")

   UString body(1000U);

   u_http_info.nResponseCode = HTTP_NOT_FOUND;

   UFileCacheData* ptr_file_data = getFileInCache(U_CONSTANT_TO_PARAM("ErrorDocument/404.html"));

   if (ptr_file_data &&
       ptr_file_data->array != 0)
      {
      body = (*ptr_file_data->array)[0];
      }
   else
      {
      const char* status = getStatusDescription(HTTP_NOT_FOUND);

      UString msg = getUrlEncodedForResponse("The requested URL %.*S was not found on this server");

      U_INTERNAL_ASSERT(str_frm_body->isNullTerminated())

      body.snprintf(str_frm_body->data(),
                     HTTP_NOT_FOUND, status,
                     status,
                     msg.data());
      }

   setResponse(str_ctype_html, &body);
}

void UHTTP::setBadMethod()
{
   U_TRACE(0, "UHTTP::setBadMethod()")

   UString body(1000U);

   u_http_info.nResponseCode = HTTP_BAD_METHOD;

   UFileCacheData* ptr_file_data = getFileInCache(U_CONSTANT_TO_PARAM("ErrorDocument/405.html"));

   if (ptr_file_data &&
       ptr_file_data->array != 0)
      {
      body = (*ptr_file_data->array)[0];
      }
   else
      {
      char format[100];

      (void) u__snprintf(format, sizeof(format), "The requested method %.*s is not allowed for the URL %%.*S", U_HTTP_METHOD_TO_TRACE);

      UString msg = getUrlEncodedForResponse(format);

      const char* status = getStatusDescription(HTTP_BAD_METHOD);

      U_INTERNAL_ASSERT(str_frm_body->isNullTerminated())

      body.snprintf(str_frm_body->data(),
                    HTTP_BAD_METHOD, status,
                    status,
                    msg.data());
      }

   setResponse(str_ctype_html, &body);
}

void UHTTP::setInternalError()
{
   U_TRACE(0, "UHTTP::setInternalError()")

   U_http_is_connection_close = U_YES;
   u_http_info.nResponseCode  = HTTP_INTERNAL_ERROR;

   UString body(2000U);

   UFileCacheData* ptr_file_data = getFileInCache(U_CONSTANT_TO_PARAM("ErrorDocument/500.html"));

   if (ptr_file_data &&
       ptr_file_data->array != 0)
      {
      body = (*ptr_file_data->array)[0];
      }
   else
      {
      const char* status = getStatusDescription(HTTP_INTERNAL_ERROR);

      U_INTERNAL_ASSERT(str_frm_body->isNullTerminated())

      body.snprintf(str_frm_body->data(),
                    HTTP_INTERNAL_ERROR, status,
                    status,
                    "The server encountered an internal error or misconfiguration "
                    "and was unable to complete your request. Please contact the server "
                    "administrator, and inform them of the time the error occurred, and "
                    "anything you might have done that may have caused the error. More "
                    "information about this error may be available in the server error log");
      }

   setResponse(str_ctype_html, &body);
}

void UHTTP::setServiceUnavailable()
{
   U_TRACE(0, "UHTTP::setServiceUnavailable()")

   U_http_is_connection_close = U_YES;
   u_http_info.nResponseCode  = HTTP_UNAVAILABLE;

   UString body(500U);

   const char* status = getStatusDescription(HTTP_UNAVAILABLE);

   U_INTERNAL_ASSERT(str_frm_body->isNullTerminated())

   body.snprintf(str_frm_body->data(),
                  HTTP_UNAVAILABLE, status,
                  status,
                  "Sorry, the service you requested is not available at this moment. "
                  "Please contact the server administrator and inform them about this");

   setResponse(str_ctype_html, &body);
}

void UHTTP::setCgiResponse(bool header_content_type, bool bcompress, bool connection_close)
{
   U_TRACE(0, "UHTTP::setCgiResponse(%b,%b,%b)", header_content_type, bcompress, connection_close)

   U_INTERNAL_DUMP("u_http_info.clength = %u", u_http_info.clength)

   U_INTERNAL_ASSERT(*UClientImage_Base::wbuffer)
   U_INTERNAL_ASSERT_MAJOR(u_http_info.clength, 0)

   const char* ptr;
   uint32_t sz, endHeader = UClientImage_Base::wbuffer->size() - u_http_info.clength;

   U_INTERNAL_DUMP("endHeader = %u", endHeader)

   UString tmp(U_CAPACITY), content = UClientImage_Base::wbuffer->substr(endHeader);

   if (bcompress)
      {
#  ifdef USE_PAGE_SPEED
      page_speed->minify_html("UHTTP::setCgiResponse()", content);
#  endif

      content = UStringExt::deflate(content, true);

      (void) tmp.append(U_CONSTANT_TO_PARAM("Content-Encoding: gzip\r\n"));
      }

   u_http_info.clength = content.size();

   if (endHeader)
      {
      // NB: endHeader comprende anche la blank line...

      ptr = UClientImage_Base::wbuffer->data();
      sz  = endHeader;
      }
   else
      {
      ptr =                 U_CRLF;
      sz  = U_CONSTANT_SIZE(U_CRLF);
      }

   if (header_content_type == false) (void) tmp.append(U_CONSTANT_TO_PARAM("Content-Type: " U_CTYPE_HTML "\r\n"));

   uint32_t sz_cookie = set_cookie->size();

   if (sz_cookie)
      {
      (void) tmp.append(set_cookie->data(), sz_cookie);
      (void) tmp.append(U_CONSTANT_TO_PARAM("\r\n"));

      set_cookie->setEmpty();
      }

   tmp.snprintf_add("Content-Length: %u\r\n"
                    "%.*s",
                    u_http_info.clength,
                    sz, ptr);

   if (bcompress) *UClientImage_Base::wbuffer = content;
   else            UClientImage_Base::wbuffer->erase(0, endHeader);

   *UClientImage_Base::body    = *UClientImage_Base::wbuffer;
   *UClientImage_Base::wbuffer = getHeaderForResponse(tmp, connection_close);
}

// --------------------------------------------------------------------------------------------------------------------------------------

U_NO_EXPORT bool UHTTP::processAuthorization()
{
   U_TRACE(0, "UHTTP::processAuthorization()")

   U_INTERNAL_ASSERT(*UClientImage_Base::request)

   bool result = false;

   const char* ptr = getHeaderValuePtr(*USocket::str_authorization, false);

   if (ptr == 0) U_RETURN(false);

   if (digest_authentication)
      {
      if (U_STRNCMP(ptr, "Digest ")) U_RETURN(false);

      ptr += U_CONSTANT_SIZE("Digest ");
      }
   else
      {
      if (U_STRNCMP(ptr, "Basic "))  U_RETURN(false);

      ptr += U_CONSTANT_SIZE("Basic ");
      }

   UString content, tmp = UClientImage_Base::request->substr(UClientImage_Base::request->distance(ptr));

   UTokenizer t(tmp, u_line_terminator);

   if (t.next(content, (bool*)0) == false) U_RETURN(false);

   UString user(100U);

   if (digest_authentication)
      {
      /*
       * A user requests page protected by digest auth:
       *
       * - The server sends back a 401 and a WWW-Authenticate header with the value of digest along with a nonce value and a realm value
       * - The user concatenates his credentials with the nonce and realm and uses that as input to MD5 to produce one has (HA1)
       * - The user concatenates the method and the URI to create a second MD5 hash (HA2)
       * - The user then sends an Authorize header with the realm, nonce, URI, and the response--which is the MD5 of the two previous hashes combined
       *
       * Authorization: Digest username="s.casazza", realm="Protected Area", nonce="dcd98b7102dd2f0e8b11d0f600bfb0c093", uri="/",
       *                response="a74c1cb52877766bb0781d12b653d1a7", qop=auth, nc=00000001, cnonce="73ed2d9694b46324", algorithm=MD5
       */

      UVector<UString> name_value;
      UString name, value, realm, nonce, _uri, response, qop, nc, cnonce;

      for (int32_t i = 0, n = UStringExt::getNameValueFromData(content, name_value, U_CONSTANT_TO_PARAM(", \t")); i < n; i += 2)
         {
         name  = name_value[i];
         value = name_value[i+1];

         U_INTERNAL_DUMP("name = %.*S value = %.*S", U_STRING_TO_TRACE(name), U_STRING_TO_TRACE(value))

         switch (name.c_char(0))
            {
            case 'u':
               {
               if (name.equal(U_CONSTANT_TO_PARAM("username")))
                  {
                  U_INTERNAL_ASSERT_EQUALS((bool)user, false)

                  user = value;
                  }
               else if (name.equal(U_CONSTANT_TO_PARAM("uri")))
                  {
                  U_INTERNAL_ASSERT_EQUALS((bool)_uri, false)

                  // NB: there may be an ALIAS...

                  if (value != getRequestURIWithQuery()) U_RETURN(false);

                  _uri = value;
                  }
               }
            break;

            case 'r':
               {
               if (name.equal(U_CONSTANT_TO_PARAM("realm")))
                  {
                  U_INTERNAL_ASSERT_EQUALS((bool)realm, false)

                  realm = value;
                  }
               else if (name.equal(U_CONSTANT_TO_PARAM("response")))
                  {
                  U_INTERNAL_ASSERT_EQUALS((bool)response, false)

                  if (value.size() != 32) U_RETURN(false);

                  response = value;
                  }
               }
            break;

            case 'n':
               {
               if (name.equal(U_CONSTANT_TO_PARAM("nonce")))
                  {
                  U_INTERNAL_ASSERT_EQUALS((bool)nonce, false)

                  // XXX: Due to a bug in MSIE (version=??), we do not check for authentication timeout...

                  if ((u_now->tv_sec - value.strtol()) > 3600) U_RETURN(false);

                  nonce = value;
                  }
               else if (name.equal(U_CONSTANT_TO_PARAM("nc")))
                  {
                  U_INTERNAL_ASSERT_EQUALS((bool)nc, false)

                  nc = value;
                  }
               }
            break;

            case 'q':
               {
               if (name.equal(U_CONSTANT_TO_PARAM("qop")))
                  {
                  U_INTERNAL_ASSERT_EQUALS((bool)qop, false)

                  qop = value;
                  }
               }
            break;

            case 'c':
               {
               if (name.equal(U_CONSTANT_TO_PARAM("cnonce")))
                  {
                  U_INTERNAL_ASSERT_EQUALS((bool)cnonce, false)

                  cnonce = value;
                  }
               }
            break;

            case 'a':
               {
               if (name.equal(U_CONSTANT_TO_PARAM("algorithm")))
                  {
                  if (value.equal("MD5") == false) U_RETURN(false);
                  }
               }
            break;
            }
         }

      UString  a2(4 + 1 + _uri.size()),      //     method : uri
              ha2(33U),                      // MD5(method : uri)
              ha1 = getUserHA1(user, realm), // MD5(user : realm : password)
               a3(200U),
              ha3(33U);

      // MD5(method : uri)

      a2.snprintf("%.*s:%.*s", U_HTTP_METHOD_TO_TRACE, U_STRING_TO_TRACE(_uri));

      UServices::generateDigest(U_HASH_MD5, 0, a2, ha2, false);

      // MD5(HA1 : nonce : nc : cnonce : qop : HA2)

      a3.snprintf("%.*s:%.*s:%.*s:%.*s:%.*s:%.*s", U_STRING_TO_TRACE(ha1), U_STRING_TO_TRACE(nonce),
                                                   U_STRING_TO_TRACE(nc),  U_STRING_TO_TRACE(cnonce),
                                                   U_STRING_TO_TRACE(qop), U_STRING_TO_TRACE(ha2));

      UServices::generateDigest(U_HASH_MD5, 0, a3, ha3, false);

      result = (ha3 == response);
      }
   else
      {
      // Authorization: Basic cy5jYXNhenphOnN0ZWZhbm8x==

      UString buffer(100U);

      if (UBase64::decode(content, buffer))
         {
         U_INTERNAL_DUMP("buffer = %.*S", U_STRING_TO_TRACE(buffer))

         t.setData(buffer);
         t.setDelimiter(":");

         UString password(100U);

         if (t.next(user,     (bool*)0) &&
             t.next(password, (bool*)0) &&
             isUserAuthorized(user, password))
            {
            result = true;
            }
         }
      }

   U_SRV_LOG("Request authorization for user %.*S %s", U_STRING_TO_TRACE(user), result ? "success" : "failed");

   U_RETURN(result);
}

UString UHTTP::getUserHA1(const UString& user, const UString& realm)
{
   U_TRACE(0, "UHTTP::getUserHA1(%.*S,%.*S)", U_STRING_TO_TRACE(user), U_STRING_TO_TRACE(realm))

   UString ha1;

   if (htdigest)
      {
      // s.casazza:Protected Area:...............\n

      UString line(100U);

      line.snprintf("%.*s:%.*s:", U_STRING_TO_TRACE(user), U_STRING_TO_TRACE(realm));

      uint32_t pos = htdigest->find(line);

      if (pos != U_NOT_FOUND)
         {
         pos += line.size();
         ha1  = htdigest->substr(pos, 32);

         U_INTERNAL_ASSERT_EQUALS(htdigest->c_char(pos + 32), '\n')
         }
      }

   U_RETURN_STRING(ha1);
}

bool UHTTP::isUserAuthorized(const UString& user, const UString& password)
{
   U_TRACE(0, "UHTTP::isUserAuthorized(%.*S,%.*S)", U_STRING_TO_TRACE(user), U_STRING_TO_TRACE(password))

   if (htpasswd)
      {
      UString line(100U), output(100U);

      UServices::generateDigest(U_HASH_SHA1, 0, password, output, true);

      line.snprintf("%.*s:{SHA}%.*s\n", U_STRING_TO_TRACE(user), U_STRING_TO_TRACE(output));

      if (htpasswd->find(line) != U_NOT_FOUND) U_RETURN(true);
      }

   U_RETURN(false);
}

__pure bool UHTTP::isSOAPRequest()
{
   U_TRACE(0, "UHTTP::isSOAPRequest()")

   bool result = (isPOST() && (U_HTTP_URI_STRNEQ("/soap") || U_HTTP_CTYPE_STRNEQ("application/soap+xml")));

   U_RETURN(result);
}

__pure bool UHTTP::isTSARequest()
{
   U_TRACE(0, "UHTTP::isTSARequest()")

   bool result = (isPOST() && (U_HTTP_URI_STRNEQ("/tsa") || U_HTTP_CTYPE_STRNEQ("application/timestamp-query")));

   U_RETURN(result);
}

__pure bool UHTTP::isGenCGIRequest() // FCGI or SCGI request...
{
   U_TRACE(0, "UHTTP::isGenCGIRequest()")

   if ((fcgi_uri_mask &&
        u_dosmatch_with_OR(U_HTTP_URI_TO_PARAM, U_STRING_TO_PARAM(*fcgi_uri_mask), 0)) ||
       (scgi_uri_mask &&
        u_dosmatch_with_OR(U_HTTP_URI_TO_PARAM, U_STRING_TO_PARAM(*scgi_uri_mask), 0)))
      {
      U_RETURN(true);
      }

   U_RETURN(false);
}

bool UHTTP::checkUriProtected()
{
   U_TRACE(0, "UHTTP::checkUriProtected()")

   U_INTERNAL_ASSERT_POINTER(uri_protected_mask)

   if (vallow_IP)
      {
      bool ok = UServer_Base::pClientImage->isAllowed(*vallow_IP);

      if (ok &&
          U_http_ip_client_len)
         {
         ok = UIPAllow::isAllowed(UServer_Base::client_address, *vallow_IP);
         }

      if (ok == false)
         {
         U_SRV_LOG("URI_PROTECTED: request %.*S denied by access list", U_HTTP_URI_TO_TRACE);

         setForbidden();

         U_RETURN(false);
         }
      }

   // check if it's OK via authentication (digest|basic)

   if (processAuthorization() == false)
      {
      setUnAuthorized();

      U_RETURN(false);
      }

   U_RETURN(true);
}

void UHTTP::manageRequest(UString* prequest_uri,
                          UString* client_address,
                          const struct UHTTP::service_info*  GET_table, uint32_t n1,
                          const struct UHTTP::service_info* POST_table, uint32_t n2)
{
   U_TRACE(0, "UHTTP::manageRequest(%p,%p,%p,%u,%p,%u)", prequest_uri, client_address, GET_table, n1, POST_table, n2)

   U_INTERNAL_ASSERT_POINTER(prequest_uri)

   int32_t high;
   const struct UHTTP::service_info* table;

   if (UHTTP::isGET())
      {
      high  = n1;
      table = GET_table;
      }
   else if (UHTTP::isPOST())
      {
      high  = n2;
      table = POST_table;
      }
   else
      {
      u_http_info.nResponseCode = HTTP_BAD_METHOD;

      return;
      }

   if (high == 0)
      {
not_found:
      u_http_info.nResponseCode = HTTP_BAD_REQUEST;

      return;
      }

   int32_t cmp = -1, probe, low = -1;
   const struct UHTTP::service_info* key;

   *prequest_uri = getRequestURI();

   // NB: skip '/'...

   uint32_t target_len = prequest_uri->size()  - 1;  
   const char* target  = prequest_uri->c_pointer(1);

   U_INTERNAL_DUMP("target(%u) = %.*S", target_len, target_len, target)

   while ((high - low) > 1)
      {
      probe = ((low + high) & 0xFFFFFFFFL) >> 1;

      U_INTERNAL_DUMP("low = %d high = %d probe = %d", low, high, probe)

      key = table + probe;

      U_INTERNAL_DUMP("key(%u) = %.*S", key->len, key->len, key->name)

      cmp = memcmp(key->name, target, U_min(key->len, target_len));

      if (cmp == 0) cmp = (key->len - target_len);
      
           if (cmp  > 0) high = probe;
      else if (cmp == 0) goto found;
      else               low = probe;
      }

   if (low == -1 ||
       (key = table + low, memcmp(key->name, target, U_min(key->len, target_len))) != 0)
      {
      goto not_found;
      }

   probe = low;

found:
   if (client_address) (void) client_address->assign(UServer_Base::client_address);

   table[probe].function();

   U_INTERNAL_DUMP("u_http_info.nResponseCode = %d", u_http_info.nResponseCode)

   if (u_http_info.nResponseCode == 0) (void) UClientImage_Base::environment->append(U_CONSTANT_TO_PARAM("HTTP_RESPONSE_CODE=0\n"));
}

UString UHTTP::getHeaderMimeType(const char* content, const char* content_type, uint32_t size, time_t expire)
{
   U_TRACE(0, "UHTTP::getHeaderMimeType(%p,%S,%u,%ld)", content, content_type, size, expire)

   U_INTERNAL_ASSERT_POINTER(file)
   U_INTERNAL_ASSERT_POINTER(content_type)

   UString header(U_CAPACITY);

   // check magic byte

   U_INTERNAL_DUMP("u_mime_index = %C", u_mime_index)

   if (content &&
       U_MEMCMP(content, GZIP_MAGIC) == 0)
      {
      u_mime_index = U_gz;

      if (U_STRNEQ(content_type, "application/x-gzip")) content_type = file->getMimeType(false);

      U_ASSERT_EQUALS(U_STRNEQ(content_type, "application/x-gzip"), false)

      (void) header.assign(U_CONSTANT_TO_PARAM("Content-Encoding: gzip\r\n"));
      }

   /* http://code.google.com/speed/page-speed/docs/caching.html
    *
    * HTTP/1.1 provides the following caching response headers:
    *
    * Expires and Cache-Control: max-age. These specify the freshness lifetime of a resource, that is, the time period during which the browser
    * can use the cached resource without checking to see if a new version is available from the web server. They are "strong caching headers"
    * that apply unconditionally; that is, once they're set and the resource is downloaded, the browser will not issue any GET requests for the
    * resource until the expiry date or maximum age is reached.
    *
    * Last-Modified and ETag. These specify some characteristic about the resource that the browser checks to determine if the files are the same.
    * In the Last-Modified header, this is always a date. In the ETag header, this can be any value that uniquely identifies a resource (file
    * versions or content hashes are typical). Last-Modified is a "weak" caching header in that the browser applies a heuristic to determine
    * whether to fetch the item from cache or not. (The heuristics are different among different browsers.) However, these headers allow the
    * browser to efficiently update its cached resources by issuing conditional GET requests when the user explicitly reloads the page. Conditional
    * GETs don't return the full response unless the resource has changed at the server, and thus have lower latency than full GETs.
    *
    * It is important to specify one of Expires or Cache-Control max-age, and one of Last-Modified or ETag, for all cacheable resources. It is
    * redundant to specify both Expires and Cache-Control: max-age, or to specify both Last-Modified and ETag. 
    *
    * Recommendations:
    * Set caching headers aggressively for all static resources.
    * For all cacheable resources, we recommend the following settings:
    * Set Expires to a minimum of one month, and preferably up to one year, in the future. (We prefer Expires over Cache-Control: max-age because
    * it is is more widely supported.) Do not set it to more than one year in the future, as that violates the RFC guidelines. If you know exactly
    * when a resource is going to change, setting a shorter expiration is okay. But if you think it "might change soon" but don't know when, you
    * should set a long expiration and use URL fingerprinting (described below). Setting caching aggressively does not "pollute" browser caches:
    * as far as we know, all browsers clear their caches according to a Least Recently Used algorithm; we are not aware of any browsers that wait
    * until resources expire before purging them.
    * Set the Last-Modified date to the last time the resource was changed. If the Last-Modified date is sufficiently far enough in the past,
    * chances are the browser won't refetch it.
    *
    * Use fingerprinting to dynamically enable caching. For resources that change occasionally, you can have the browser cache the resource until
    * it changes on the server, at which point the server tells the browser that a new version is available. You accomplish this by embedding a
    * fingerprint of the resource in its URL (i.e. the file path). When the resource changes, so does its fingerprint, and in turn, so does its URL.
    * As soon as the URL changes, the browser is forced to re-fetch the resource. Fingerprinting allows you to set expiry dates long into the future
    * even for resources that change more frequently than that. Of course, this technique requires that all of the pages that reference the resource
    * know about the fingerprinted URL, which may or may not be feasible, depending on how your pages are coded.
    *
    * Set the Vary header correctly for Internet Explorer. Internet Explorer does not cache any resources that are served with the Vary header and any
    * fields but Accept-Encoding and User-Agent. To ensure these resources are cached by IE, make sure to strip out any other fields from the Vary
    * header, or remove the Vary header altogether if possible
    *
    * The rule is any resource that is cachable should not have a Vary: User-Agent header.
    *
    * Avoid URLs that cause cache collisions in Firefox. The Firefox disk cache hash functions can generate collisions for URLs that differ only
    * slightly, namely only on 8-character boundaries. When resources hash to the same key, only one of the resources is persisted to disk cache;
    * the remaining resources with the same key have to be re-fetched across browser restarts. Thus, if you are using fingerprinting or are otherwise
    * programmatically generating file URLs, to maximize cache hit rate, avoid the Firefox hash collision issue by ensuring that your application
    * generates URLs that differ on more than 8-character boundaries. 
    *
    * Use the Cache control: public directive to enable HTTPS caching for Firefox. Some versions of Firefox require that the Cache control: public
    * header to be set in order for resources sent over SSL to be cached on disk, even if the other caching headers are explicitly set. Although this
    * header is normally used to enable caching by proxy servers (as described below), proxies cannot cache any content sent over HTTPS, so it is
    * always safe to set this header for HTTPS resources. 
    */

   header.snprintf_add("Content-Type: %s\r\n", content_type);

   U_INTERNAL_DUMP("u_mime_index = %C", u_mime_index)

   const char* fmt;

   if (u_is_ssi() ||
       u_is_cgi())
      {
      fmt = "Content-Length: " "   " "%u\r\n\r\n";
      }
   else
      {
      fmt = "Content-Length: "       "%u\r\n\r\n";

      if (expire) header.snprintf_add("Expires: %#12D\r\n", expire);
                  header.snprintf_add("Last-Modified: %#12D\r\n", file->st_mtime);
      }

   if (u_is_css())
      {
      U_INTERNAL_ASSERT(U_STRNEQ(content_type, "text/css"))
      U_INTERNAL_ASSERT(u_endsWith(U_FILE_TO_PARAM(*file), U_CONSTANT_TO_PARAM(".css")))

      (void) header.append("Content-Style-Type: text/css\r\n");
      }
   else if (u_is_js())
      {
      U_INTERNAL_ASSERT(U_STRNEQ(content_type, "text/javascript"))
      U_INTERNAL_ASSERT(u_endsWith(U_FILE_TO_PARAM(*file), U_CONSTANT_TO_PARAM(".js")))

      (void) header.append("Content-Script-Type: text/javascript\r\n");
      }

   if (enable_caching_by_proxy_servers)
      {
      (void) header.append(U_CONSTANT_TO_PARAM("Cache-Control: public\r\n"
                                               "Vary: Accept-Encoding\r\n"
                                               "Accept-Ranges: bytes\r\n"));
      }

   if (size) header.snprintf_add(fmt, size);
   else     (void) header.append(fmt);

   U_RETURN_STRING(header);
}

U_NO_EXPORT void UHTTP::putDataInCache(const UString& fmt, UString& content)
{
   U_TRACE(0, "UHTTP::putDataInCache(%.*S,%.*S)", U_STRING_TO_TRACE(fmt), U_STRING_TO_TRACE(content))

   U_INTERNAL_ASSERT_POINTER(file)
   U_INTERNAL_ASSERT_POINTER(file_data)
   U_INTERNAL_ASSERT(fmt.isNullTerminated())
   U_INTERNAL_ASSERT_MAJOR(file_data->size, 0)

   uint32_t size;
   int ratio = 100;
   bool gzip = false;
   UString header(U_CAPACITY);
   const char* motivation = 0;

   U_NEW_DBG(UVector<UString>, file_data->array, UVector<UString>(4U));

   file_data->array->push_back(content);

   header.snprintf(fmt.data(), file_data->size);

   file_data->array->push_back(header);

   file_data->mime_index = u_mime_index;

   U_INTERNAL_DUMP("u_mime_index = %C", u_mime_index)

   if (u_is_img())
      {
      U_INTERNAL_ASSERT(u_endsWith(U_FILE_TO_PARAM(*file), U_CONSTANT_TO_PARAM(".gif")) ||
                        u_endsWith(U_FILE_TO_PARAM(*file), U_CONSTANT_TO_PARAM(".png")) ||
                        u_endsWith(U_FILE_TO_PARAM(*file), U_CONSTANT_TO_PARAM(".ico")) ||
                        u_endsWith(U_FILE_TO_PARAM(*file), U_CONSTANT_TO_PARAM(".jpg")) ||
                        u_endsWith(U_FILE_TO_PARAM(*file), U_CONSTANT_TO_PARAM(".jpeg")))

#if defined(USE_PAGE_SPEED) && defined(DEBUG)
           if (u_is_gif()) page_speed->optimize_gif(content);
      else if (u_is_png()) page_speed->optimize_png(content);
      else                 page_speed->optimize_jpg(content);

      if (content.size() < file_data->size) U_SRV_LOG("WARNING: found not optimized image: %S", pathname->data());
#endif

      goto next;
      }

   if (u_is_js() ||
       u_is_css())
      {
      U_INTERNAL_ASSERT(u_endsWith(U_FILE_TO_PARAM(*file), U_CONSTANT_TO_PARAM(".css")) ||
                        u_endsWith(U_FILE_TO_PARAM(*file), U_CONSTANT_TO_PARAM(".js")))

      if ((u_is_js() && 
           u_endsWith(U_FILE_TO_PARAM(*file), U_CONSTANT_TO_PARAM("compressed.js")  == false)) ||
           u_endsWith(U_FILE_TO_PARAM(*file), U_CONSTANT_TO_PARAM("compressed.css") == false))
         {
         content = UStringExt::minifyCssJs(content); // minifies CSS/JS by removing comments and whitespaces...
         }

      if (content.empty()) goto end;
      }
#ifdef USE_PAGE_SPEED
   else if (u_is_html())
      {
      U_INTERNAL_ASSERT(u_endsWith(U_FILE_TO_PARAM(*file), U_CONSTANT_TO_PARAM(".htm")) ||
                        u_endsWith(U_FILE_TO_PARAM(*file), U_CONSTANT_TO_PARAM(".html")))

      U_INTERNAL_ASSERT(pathname->isNullTerminated())

      page_speed->minify_html(pathname->data(), content);
      }
#endif

        if (file_data->size >= min_size_for_sendfile)  motivation = " (size exceeded)";  // NB: for major size it is better to use sendfile()
   else if (file_data->size <= U_MIN_SIZE_FOR_DEFLATE) motivation = " (size too small)";
#ifdef USE_LIBZ
   else if (u_is_ssi() == false &&
            u_is_gz()  == false)
      {
      /* http://zoompf.com/blog/2012/02/lose-the-wait-http-compression
       *
       * Unfortunately, the HTTP/1.1 RFC does a poor job when describing the allowable compression schemes for the Accept-Encoding
       * and Content-Encoding headers. It defines Content-Encoding: gzip to mean that the response body is composed of the GZIP
       * data format (GZIP headers, deflated data, and a checksum). It also defines Content-Encoding: deflate but, despite its name,
       * this does not mean the response body is a raw block of DEFLATE compressed data.
       *
       * According to RFC-2616, Content-Encoding: deflate means the response body is:
       * [the] "zlib" format defined in RFC 1950 in combination with the "deflate" compression mechanism described in RFC 1951.
       *
       * So, DEFLATE, and Content-Encoding: deflate, actually means the response body is composed of the zlib format (zlib header,
       * deflated data, and a checksum).
       *
       * This "deflate the identifier doesn't mean raw DEFLATE compressed data" idea was rather confusing.
       *
       * Browsers receive Content-Encoding: deflate had to handle two possible situations: the response body is raw DEFLATE data,
       * or the response body is zlib wrapped DEFLATE. So, how well do modern browser handle raw DEFLATE or zlib wrapped DEFLATE
       * responses? Verve Studios put together a test suite and tested a huge number of browsers. The results are not good. All
       * those fractional results in the table means the browser handled raw-DEFLATE or zlib-wrapped-DEFLATE inconsistently,
       * which is really another way of saying "It's broken and doesn't work reliably". This seems to be a tricky bug that
       * browser creators keep re-introducing into their products. Safari 5.0.2? No problem. Safari 5.0.3? Complete failure.
       * Safari 5.0.4? No problem. Safari 5.0.5? Inconsistent and broken.
       *
       * Sending raw DEFLATE data is just not a good idea. As Mark says "[it's] simply more reliable to only use GZIP"
       */

      content = UStringExt::deflate(content, (gzip = true));
      }
#endif

next:
   size = content.size();

   if (size < file_data->size)
      {
      ratio = (size * 100U) / file_data->size;

      U_INTERNAL_DUMP("ratio = %d", ratio)

      if (ratio < 85 && // NB: we accept only if ratio compression is better than 15%...
          motivation == 0)
         {
         file_data->array->push_back(content);

         header.setBuffer(U_CAPACITY);

         if (gzip) (void) header.assign(U_CONSTANT_TO_PARAM("Content-Encoding: gzip\r\n"));
                          header.snprintf_add(fmt.data(), size);

         file_data->array->push_back(header);
         }
      }

end:
   U_SRV_LOG("File cached: %S - %u bytes - (%d%%) compressed ratio%s", pathname->data(), file_data->size, 100 - ratio, (motivation ? motivation : ""));
}

void UHTTP::callInitForAllUSP(UStringRep* key, void* value)
{
   U_TRACE(0+256, "UHTTP::callInitForAllUSP(%.*S,%p)", U_STRING_TO_TRACE(*key), value)

   U_INTERNAL_ASSERT_POINTER(value)

   UFileCacheData* cptr = (UFileCacheData*)value;

   if (cptr->link == false &&
       cptr->mime_index == U_usp)
      {
      UServletPage* usp_page = (UServletPage*)cptr->ptr;

      U_INTERNAL_DUMP("usp_page->runDynamicPage = %p", usp_page->runDynamicPage)

      U_INTERNAL_ASSERT_POINTER(usp_page->runDynamicPage)

      // ------------------------------
      // argument value for usp module:
      // ------------------------------
      //  0 -> init
      // -1 -> reset
      // -2 -> destroy
      // -3 -> call it as service
      // ------------------------------

      (void) usp_page->runDynamicPage(0);
      }
}

void UHTTP::callEndForAllUSP(UStringRep* key, void* value)
{
   U_TRACE(0+256, "UHTTP::callEndForAllUSP(%.*S,%p)", U_STRING_TO_TRACE(*key), value)

   U_INTERNAL_ASSERT_POINTER(value)

   UFileCacheData* cptr = (UFileCacheData*)value;

   if (cptr->link == false &&
       cptr->mime_index == U_usp)
      {
      UServletPage* usp_page = (UServletPage*)cptr->ptr;

      U_INTERNAL_DUMP("usp_page->runDynamicPage = %p", usp_page->runDynamicPage)

      U_INTERNAL_ASSERT_POINTER(usp_page->runDynamicPage)

      // ------------------------------
      // argument value for usp module:
      // ------------------------------
      //  0 -> init
      // -1 -> reset
      // -2 -> destroy
      // -3 -> call it as service
      // ------------------------------

      (void) usp_page->runDynamicPage((void*)-2);
      }
}

U_NO_EXPORT void UHTTP::checkIfLink(UStringRep* key, void* value)
{
   U_TRACE(0+256, "UHTTP::checkIfLink(%.*S,%p)", U_STRING_TO_TRACE(*key), value)

   U_INTERNAL_ASSERT_POINTER(value)
   U_INTERNAL_ASSERT_POINTER(file_data)

   UFileCacheData* cptr = (UFileCacheData*)value;

   if (cptr->link       == false &&
       cptr->mime_index == file_data->mime_index)
      {
      if (cptr->mime_index == U_usp)
         {
         UServletPage* usp_page1 = (UServletPage*)cptr->ptr;
         UServletPage* usp_page2 = (UServletPage*)file_data->ptr;

         U_INTERNAL_DUMP("usp_page1->runDynamicPage = %p", usp_page1->runDynamicPage)
         U_INTERNAL_DUMP("usp_page2->runDynamicPage = %p", usp_page2->runDynamicPage)

         U_INTERNAL_ASSERT_POINTER(usp_page1->runDynamicPage)
         U_INTERNAL_ASSERT_POINTER(usp_page2->runDynamicPage)

         if (usp_page1->runDynamicPage ==
             usp_page2->runDynamicPage)
            {
            delete usp_page2;

            file_data->ptr  = usp_page1;
            file_data->link = true;

            cache_file->stopCallForAllEntry();
            }
         }
      else if (cptr->mime_index == U_csp)
         {
         UCServletPage* csp_page1 = (UCServletPage*)cptr->ptr;
         UCServletPage* csp_page2 = (UCServletPage*)file_data->ptr;

         U_INTERNAL_DUMP("csp_page1->prog_main = %p", csp_page1->prog_main)
         U_INTERNAL_DUMP("csp_page2->prog_main = %p", csp_page2->prog_main)

         U_INTERNAL_ASSERT_POINTER(csp_page1->prog_main)
         U_INTERNAL_ASSERT_POINTER(csp_page2->prog_main)

         if (csp_page1->prog_main ==
             csp_page2->prog_main)
            {
            delete csp_page2;

            file_data->ptr  = csp_page1;
            file_data->link = true;

            cache_file->stopCallForAllEntry();
            }
         }
      else if (cptr->mime_index == U_cgi)
         {
         UHTTP::ucgi* cgi1 = (UHTTP::ucgi*)cptr->ptr;
         UHTTP::ucgi* cgi2 = (UHTTP::ucgi*)file_data->ptr;

         uint32_t n = u__strlen(cgi1->dir, __PRETTY_FUNCTION__);

         U_INTERNAL_DUMP("cgi1->dir = %.*S", n, cgi1->dir)
         U_INTERNAL_DUMP("cgi2->dir = %.*S", n, cgi2->dir)

         if (strcmp(cgi1->dir,         cgi2->dir)         == 0 &&
             strcmp(cgi1->dir + n + 1, cgi2->dir + n + 1) == 0)
            {
            delete cgi2;

            file_data->ptr  = cgi1;
            file_data->link = true;

            cache_file->stopCallForAllEntry();
            }
         }
      else
         {
         U_INTERNAL_ASSERT_POINTER(file_data->ptr)

         U_INTERNAL_DUMP("file_data->ptr = %.*S", U_STRING_TO_TRACE(*(UString*)file_data->ptr))

         if (((UString*)file_data->ptr)->equal(key))
            {
            delete (UString*)file_data->ptr;

            file_data->ptr  = cptr->ptr;
            file_data->link = true;

            cache_file->stopCallForAllEntry();
            }
         }
      }
}

static UStringRep* usp_page_key;

U_NO_EXPORT void UHTTP::checkIfUSP(UStringRep* key, void* value)
{
   U_TRACE(0+256, "UHTTP::checkIfUSP(%.*S,%p)", U_STRING_TO_TRACE(*key), value)

   U_INTERNAL_ASSERT_POINTER(value)

   UFileCacheData* cptr = (UFileCacheData*)value;

   if (cptr->link == false &&
       cptr->mime_index == U_usp)
      {
      UServletPage* usp_page = (UServletPage*)cptr->ptr;

      U_INTERNAL_DUMP("usp_page->runDynamicPage = %p", usp_page->runDynamicPage)

      U_INTERNAL_ASSERT_POINTER(usp_page->runDynamicPage)

      if (u_endsWith(U_STRING_TO_PARAM(*key), U_STRING_TO_PARAM(*usp_page_key)))
         {
         file_data = cptr;

         cache_file->stopCallForAllEntry();

         U_SRV_LOG("getUSP(%.*S) has found the USP service (URI): %.*s", U_STRING_TO_TRACE(*usp_page_key), U_STRING_TO_TRACE(*key));
         }
      }
}

UHTTP::UServletPage* UHTTP::getUSP(const UString& key)
{
   U_TRACE(0, "UHTTP::getUSP(%.*S)", U_STRING_TO_TRACE(key))

   U_INTERNAL_ASSERT(key)
   U_INTERNAL_ASSERT_POINTER(cache_file)

   file_data    = 0;
   usp_page_key = key.rep;

   cache_file->callForAllEntry(checkIfUSP);

   UServletPage* usp_page = (file_data ? (UServletPage*)file_data->ptr : 0);

   U_RETURN_POINTER(usp_page, UHTTP::UServletPage);
}

U_NO_EXPORT void UHTTP::manageDataForCache()
{
   U_TRACE(0, "UHTTP::manageDataForCache()")

   U_INTERNAL_ASSERT_POINTER(file)
   U_INTERNAL_ASSERT_POINTER(pathname)
   U_INTERNAL_ASSERT_POINTER(cache_file)

   U_INTERNAL_DUMP("pathname = %.*S file = %.*S", U_STRING_TO_TRACE(*pathname), U_FILE_TO_TRACE(*file))

   U_NEW_DBG(UFileCacheData, file_data, UFileCacheData);

   // NB: copy attribute from file...

   file_data->size  = file->st_size;
   file_data->mode  = file->st_mode;
   file_data->mtime = file->st_mtime;

   if (file->dir())
      {
#  if defined(HAVE_SYS_INOTIFY_H) && defined(U_HTTP_INOTIFY_SUPPORT)
      if (UServer_Base::handler_inotify)
         {
         file_data->wd = U_SYSCALL(inotify_add_watch, "%d,%s,%u",
                                       UServer_Base::handler_inotify->fd,
                                       pathname->c_str(),
                                       IN_ONLYDIR | IN_CREATE | IN_DELETE | IN_MODIFY);
         }
#  endif
      }
   else
      {
      if (u_dosmatch_with_OR(U_FILE_TO_PARAM(*file), U_CONSTANT_TO_PARAM("*cgi-bin/*|*servlet/*"), 0))
         {
         uint32_t pos       = pathname->find_last_of('.');
         const char* suffix = (pos == U_NOT_FOUND ? 0 : pathname->c_pointer(pos + 1));

         pos = U_STRING_FIND(*pathname, 0, "servlet/");

         if (pos != U_NOT_FOUND)
            {
            // NB: when a url ends by "servlet/*.[c|so|dll]" it is assumed to be a servlet page...

            if (U_STRNEQ(suffix, U_LIB_SUFFIX))
               {
               const char* link;
               char buffer[U_PATH_MAX];

               UServletPage* usp_page = U_NEW(UHTTP::UServletPage);

               // NB: dlopen() fail if name is not prefixed with "./"...

               pos = u__snprintf(buffer, U_PATH_MAX, "./%.*s", U_FILE_TO_TRACE(*file));

               if (usp_page->UDynamic::load(buffer) == false)
                  {
                  U_SRV_LOG("USP load failed: %S", buffer);

                  delete usp_page;

                  goto end;
                  }

               usp_page->runDynamicPage = (iPFpv) (*usp_page)["runDynamicPage"];

               U_INTERNAL_ASSERT_POINTER(usp_page->runDynamicPage)

               file_data->ptr        = usp_page;
               file_data->mime_index = U_usp;

               link = (cache_file->callForAllEntry(checkIfLink), file_data->link) ? " (link)" : "";

               (void) pathname->replace(buffer + 2, pos - 2 - 1 - U_CONSTANT_SIZE(U_LIB_SUFFIX));

               U_SRV_LOG("USP found: %S%s, USP service registered (URI): %.*s", buffer, link, U_STRING_TO_TRACE(*pathname));

#           ifdef U_HTTP_UPLOAD_PROGRESS_SUPPORT
               if (UServer_Base::isPreForked() &&
                   UStringExt::endsWith(*pathname, U_CONSTANT_TO_PARAM("upload_progress")) &&
                   USocketExt::byte_read_hook == 0)
                  {
                  // UPLOAD PROGRESS

                  U_SRV_LOG("USP upload progress registered");

                  USocketExt::byte_read_hook = updateUploadProgress;

                  U_INTERNAL_ASSERT_EQUALS(ptr_upload_progress, 0)

                  ptr_upload_progress = (upload_progress*) UServer_Base::getOffsetToDataShare(sizeof(upload_progress) * U_MAX_UPLOAD_PROGRESS);
                  }
#           endif
               }
#        ifdef HAVE_LIBTCC
            else if (U_STRNEQ(suffix, "c"))
               {
               const char* link;
               char buffer[U_PATH_MAX];
               UString program = file->getContent();

               UCServletPage* csp_page = U_NEW(UHTTP::UCServletPage);

               if (program.empty()            == false &&
                   csp_page->compile(program) == false)
                  {
                  U_SRV_LOG("CSP load failed: %.*S", U_FILE_TO_TRACE(*file));

                  delete csp_page;

                  goto end;
                  }

               U_INTERNAL_ASSERT_POINTER(csp_page->prog_main)

               file_data->ptr        = csp_page;
               file_data->mime_index = U_csp;

               link = (cache_file->callForAllEntry(checkIfLink), file_data->link) ? " (link)" : "";

               pos = u__snprintf(buffer, U_PATH_MAX, "%.*s", U_FILE_TO_TRACE(*file));

               (void) pathname->replace(buffer, pos - U_CONSTANT_SIZE(".c"));

               U_SRV_LOG("CSP found: %S%s, CSP service registered (URI): %.*s", buffer, link, U_STRING_TO_TRACE(*pathname));
               }
#           endif
            }
         else
            {
            // NB: when a url ends by "cgi-bin/" it is assumed to be a cgi script...

            pos = U_STRING_FIND(*pathname, 0, "cgi-bin/");

            U_INTERNAL_ASSERT_DIFFERS(pos, U_NOT_FOUND)

            if (suffix &&
                suffix[-2] != '/') // NB: dir "cgi-bin" often have some 'function file' (that starts with '.')...
               {
               UHTTP::ucgi* cgi = U_MALLOC_TYPE(UHTTP::ucgi);

               pathname->copy(cgi->dir);

               U_INTERNAL_DUMP("cgi_dir = %S", cgi->dir)

               cgi->dir[pos + U_CONSTANT_SIZE("cgi-bin")] = '\0';

               U_INTERNAL_DUMP("cgi_doc = %S", cgi->dir + u__strlen(cgi->dir, __PRETTY_FUNCTION__) + 1)

               const char* ptr = pathname->c_pointer(pathname->size() - 2);

               cgi->sh_script = U_STRNEQ(ptr, "sh");

               U_INTERNAL_DUMP("cgi->sh_script = %d", cgi->sh_script)

                    if (U_STRNEQ(suffix, "sh"))  cgi->interpreter = U_PATH_SHELL;
               else if (U_STRNEQ(suffix, "php")) cgi->interpreter = "php-cgi";
               else if (U_STRNEQ(suffix, "pl"))  cgi->interpreter = "perl";
               else if (U_STRNEQ(suffix, "py"))  cgi->interpreter = "python";
               else if (U_STRNEQ(suffix, "rb"))  cgi->interpreter = "ruby";
               else                              cgi->interpreter = 0;

               U_INTERNAL_DUMP("cgi->interpreter = %S", cgi->interpreter)

               file_data->ptr        = cgi;
               file_data->mime_index = U_cgi;

               const char* link = (cache_file->callForAllEntry(checkIfLink), file_data->link) ? " (link)" : "";

               U_SRV_LOG("cgi-bin found: %.*S%s, interpreter registered: %S", U_FILE_TO_TRACE(*file), link, cgi->interpreter);
               }
            }
         }
      else if (u_dosmatch_with_OR(U_FILE_TO_PARAM(*file), U_STRING_TO_PARAM(*cache_file_mask), 0))
         {
         U_INTERNAL_DUMP("file_data->size = %u", file_data->size)

         if (file_data->size == 0)
            {
            U_SRV_LOG("WARNING: found empty file: %S", pathname->data());
            }
         else if (file->open())
            {
            UString content;

            // NB: manage for possibly partial write in response...

#        ifdef U_CLIENT_RESPONSE_PARTIAL_WRITE_SUPPORT
            if (file_data->size > U_MIN_SIZE_FOR_PARTIAL_WRITE)
               {
               file_data->fd = file->fd;

               (void) file->memmap(PROT_READ, &content);
               }
            else
#        endif
            content = file->getContent(true, false, true);

            if (content.empty() == false)
               {
#           ifndef __MINGW32__
               if ((file->lstat(), file->slink()))
                  {
                  U_NEW_DBG(UString, file_data->ptr, UString(*pathname));

                  cache_file->callForAllEntry(checkIfLink);
                  }
#           endif

               if (file_data->link == false)
                  {
                  u_mime_index = -1;

                  putDataInCache(getHeaderMimeType(content.data(), file->getMimeType(true), 0, U_TIME_FOR_EXPIRE), content);
                  }
               }
            }
         }
      }

end:
   U_INTERNAL_DUMP("file_data->mime_index = %C", file_data->mime_index)

   cache_file->insert(*pathname, file_data); // NB: we don't need to call u_construct<UFileCacheData>()...
}

UHTTP::UFileCacheData* UHTTP::getFileInCache(const char* path, uint32_t len)
{
   U_TRACE(0, "UHTTP::getFileInCache(%.*S,%u)", len, path, len)

   U_INTERNAL_ASSERT_MAJOR(len,0)
   U_INTERNAL_ASSERT_POINTER(path)
   U_INTERNAL_ASSERT_POINTER(UHashMap<void*>::pkey)

   UHashMap<void*>::pkey->str     = path;
   UHashMap<void*>::pkey->_length = len;

   UFileCacheData* ptr_file_data = (*cache_file)[UHashMap<void*>::pkey];

   U_RETURN_POINTER(ptr_file_data, UFileCacheData);
}

bool UHTTP::isFileInCache()
{
   U_TRACE(0, "UHTTP::isFileInCache()")

   file_data = getFileInCache(file->getPathRelativ(), file->getPathRelativLen());

   if (file_data)
      {
      file->st_size  = file_data->size;
      file->st_mode  = file_data->mode;
      file->st_mtime = file_data->mtime;

      U_INTERNAL_DUMP("file_data->fd = %d st_size = %I st_mtime = %ld dir() = %b", file_data->fd, file->st_size, file->st_mtime, file->dir())

      U_RETURN(true);
      }

   U_RETURN(false);
}

void UHTTP::renewDataCache()
{
   U_TRACE(0, "UHTTP::renewDataCache()")

   // NB: we need to do this before call erase...

   int fd          = file_data->fd;
   UStringRep* key = cache_file->key();

   U_INTERNAL_DUMP("file_data->fd = %d cache_file->key = %.*S", file_data->fd, U_STRING_TO_TRACE(*key))

   U_INTERNAL_ASSERT_POINTER(pathname)

   uint32_t sz = key->size();

   pathname->setBuffer(sz);

   (void) pathname->snprintf("%.*s", sz, key->data());

   cache_file->eraseAfterFind();

   checkFileForCache();

   if (fd != -1 &&
       file->st_ino) // stat() ok...
      {
      UFile::close(fd);

      if (file->open()) file_data->fd = file->fd;
      }
}

UString UHTTP::getDataFromCache(bool bheader, bool gzip)
{
   U_TRACE(0, "UHTTP::getDataFromCache(%b,%b)", bheader, gzip)

   U_INTERNAL_ASSERT_POINTER(file_data)

   U_INTERNAL_DUMP("u_now->tv_sec     = %#3D", u_now->tv_sec)
   U_INTERNAL_DUMP("file_data->expire = %#3D", file_data->expire)

   if (u_now->tv_sec > file_data->expire)
      {
      renewDataCache();

      U_INTERNAL_DUMP("file_data->array = %p", file_data->array)
      }

   UString result;

   if (file_data->array)
      {
      U_INTERNAL_DUMP("idx = %u", (gzip * 2) + bheader)

      U_INTERNAL_ASSERT_POINTER(file_data->array)

      result = file_data->array->operator[]((gzip * 2) + bheader);
      }

   U_RETURN_STRING(result);
}

U_NO_EXPORT bool UHTTP::processFileCache()
{
   U_TRACE(0, "UHTTP::processFileCache()")

   U_INTERNAL_ASSERT_POINTER(file_data)

   if (U_http_is_accept_gzip &&
       isDataCompressFromCache())
      {
      U_http_is_accept_gzip = '2';

                             *UClientImage_Base::wbuffer = getHeaderForResponse(getDataFromCache( true, true), false);
      if (isHEAD() == false) *UClientImage_Base::body    =                      getDataFromCache(false, true);

      U_RETURN(true);
      }

   bool result    = true;
   UString header = getDataFromCache(true, false);

   U_INTERNAL_DUMP("header = %.*S", U_STRING_TO_TRACE(header))

   // The Range: header is used with a GET/HEAD request.
   // For example assume that will return only a portion (let's say the first 32 bytes) of the requested resource...
   //
   // Range: bytes=0-31

   uint32_t sz;

   range_size  = sz = file_data->size;
   range_start = 0;

   if (U_http_range_len &&
       checkGetRequestIfRange(UString::getStringNull()))
      {
      if (checkGetRequestForRange(header, getDataFromCache(false, false)) != U_PARTIAL) // NB: we have a complete response...
         {
#     ifdef U_CLIENT_RESPONSE_PARTIAL_WRITE_SUPPORT
         goto prepare_for_partial_write_at_response;
#     else
         U_RETURN(true);
#     endif
         }

      // NB: range_start is modified only if we have as response from checkGetRequestForRange() U_PARTIAL...

      sz -= range_start;

      U_INTERNAL_DUMP("sz = %u", sz)

      u_http_info.nResponseCode = HTTP_PARTIAL;
      }

   if (isHEAD() == false)
      {
      U_INTERNAL_DUMP("min_size_for_sendfile = %u", min_size_for_sendfile)

      if (sz >= min_size_for_sendfile)
         {
         // NB: for major size it is better to use sendfile()...

         result    = false;
         bsendfile = true;

         goto build_response;
         }

      *UClientImage_Base::body = getDataFromCache(false, false).substr(range_start, range_size);
      }

   if (u_is_ssi() == false)
      {
build_response:

      *UClientImage_Base::wbuffer = getHeaderForResponse(header, false);

#ifdef U_CLIENT_RESPONSE_PARTIAL_WRITE_SUPPORT
prepare_for_partial_write_at_response:

      if (bsendfile == false          &&
#        ifdef USE_LIBSSL
          UServer_Base::bssl == false &&
#        endif
          sz > U_MIN_SIZE_FOR_PARTIAL_WRITE)
         {
         UServer_Base::pClientImage->sfd   = file_data->fd;
         UServer_Base::pClientImage->start = range_start;
         }
#endif
      }

   U_RETURN(result);
}

U_NO_EXPORT void UHTTP::checkPath()
{
   U_TRACE(0, "UHTTP::checkPath()")

   U_INTERNAL_DUMP("pathname = %.*S", U_STRING_TO_TRACE(*pathname))

   U_INTERNAL_ASSERT(pathname->isNullTerminated())

   if (u_canonicalize_pathname(pathname->data())) pathname->size_adjust_force(); // NB: can be referenced by file...

   if (UServer_Base::isFileInsideDocumentRoot(*pathname) == false)
      {
      setRequestForbidden(); // like chroot()...
      }
   else if (pathname->size() == u_cwd_len)
      {
      // NB: special case: '/' alias '.'...

      U_INTERNAL_ASSERT_EQUALS(u_http_info.uri[0], '/')

      file->setRoot();

      setRequestNeedProcessing();
      }
   else
      {
      file->setPath(*pathname);

      U_INTERNAL_DUMP("file = %.*S", U_FILE_TO_TRACE(*file))

      if (isFileInCache() == false)
         {
         if (nostat ||
             U_http_websocket)
            {
            return;
            }

         if (u_endsWith(U_FILE_TO_PARAM(*file), U_CONSTANT_TO_PARAM("nostat"))) // NOT a static page...
            {
            // NB: there may be an ALIAS...

            uint32_t len = request_uri->size();

            if (len > 1) // NB: special case: '/' alias '/nostat'...
               {
               pathname->setBuffer(u_cwd_len + len);

               pathname->snprintf("%w%.*s", len, request_uri->data());

               U_INTERNAL_ASSERT_EQUALS(nostat, false)

               nostat = true;

               checkPath();

               nostat = false;
               }

            return;
            }

         if (u_endsWith(U_FILE_TO_PARAM(*file), U_CONSTANT_TO_PARAM("nocontent"))) // we don't wont to process this kind of request...
            {
            U_http_is_connection_close = U_YES;
            u_http_info.nResponseCode  = HTTP_NO_CONTENT;

            setResponse(0, 0);

            setRequestProcessed();

            return;
            }

         U_INTERNAL_ASSERT_EQUALS(nostat, false)

         if (file->stat())
            {
            U_SRV_LOG("Found file not in cache: %.*S - inotify %s enabled", U_FILE_TO_TRACE(*file), UServer_Base::handler_inotify ? "is" : "NOT");

            manageDataForCache();

            U_INTERNAL_ASSERT_POINTER(file_data)
            }
         }

      if (file_data) setRequestInFileCache();
      }
}

U_NO_EXPORT bool UHTTP::checkPath(uint32_t len)
{
   U_TRACE(0, "UHTTP::checkPath(%u)", len)

   // NB: pathname is referenced by file...

   file->path_relativ_len = len;

   U_INTERNAL_DUMP("file = %.*S", U_FILE_TO_TRACE(*file))

   pathname->size_adjust_force(u_cwd_len + len);

   pathname->rep->setNullTerminated();

   checkPath();

   if (isRequestNotFound()) U_RETURN(false);

   U_RETURN(true);
}

// REWRITE RULE

UVector<UHTTP::RewriteRule*>* UHTTP::vRewriteRule;

U_NO_EXPORT void UHTTP::processRewriteRule()
{
   U_TRACE(0, "UHTTP::processRewriteRule()")

#ifndef USE_LIBPCRE
   U_SRV_LOG("REWRITE_RULE_NF: pcre support is missing, please install libpcre and the headers and recompile ULib...");
#else
   uint32_t pos, len;
   UHTTP::RewriteRule* rule;
   UString _uri(U_HTTP_URI_TO_PARAM), new_uri;

   for (uint32_t i = 0, n = vRewriteRule->size(); i < n; ++i)
      {
      rule    = (*vRewriteRule)[i];
      new_uri = rule->key.replace(_uri, rule->replacement);

      if (rule->key.matched())
         {
         pos = new_uri.find('?');
         len = (pos == U_NOT_FOUND ? new_uri.size() : pos);

         pathname->setBuffer(u_cwd_len + len);

         pathname->snprintf("%w%.*s", len, new_uri.data());

         U_SRV_LOG("REWRITE_RULE_NF: URI request changed to: %.*s", U_STRING_TO_TRACE(new_uri));

         checkPath();

         if (isRequestNeedProcessing())
            {
            request_uri->clear();

            (void) alias->replace(new_uri);

            u_http_info.uri     = alias->data();
            u_http_info.uri_len = len;

            U_INTERNAL_DUMP("uri = %.*S", U_HTTP_URI_TO_TRACE)

            if (pos != U_NOT_FOUND)
               {
               const char* ptr = alias->c_pointer(len+1);

               u_http_info.query     = ptr;
               u_http_info.query_len = alias->remain(ptr);

               U_INTERNAL_DUMP("query = %.*S", U_HTTP_QUERY_TO_TRACE)
               }
            }

         break;
         }
      }
#endif
}

// manage dynamic page request (C/ULib Servlet Page)
 
void UHTTP::manageServletRequest(bool as_service)
{
   U_TRACE(0, "UHTTP::manageServletRequest(%b)", as_service)

   U_INTERNAL_ASSERT(isValidRequest())

   U_INTERNAL_DUMP("u_http_info.nResponseCode = %d", u_http_info.nResponseCode)

   if (u_is_usp())
      {
      UServletPage* usp = (UServletPage*)file_data->ptr;

      U_INTERNAL_ASSERT_POINTER(usp)
      U_INTERNAL_ASSERT_POINTER(usp->runDynamicPage)

      if (UServer_Base::isLog()) UServer_Base::mod_name->snprintf("[usp] ", 0);

      // ------------------------------
      // argument value for usp module:
      // ------------------------------
      //  0 -> init
      // -1 -> reset
      // -2 -> destroy
      // -3 -> call it as service
      // ------------------------------

      if (as_service)
         {
         u_http_info.nResponseCode = 0;

         (void) usp->runDynamicPage((void*)-3);
         }
      else
         {
         (void) usp->runDynamicPage((void*)-1);

         UClientImage_Base::wbuffer->setBuffer(U_CAPACITY);

         u_http_info.nResponseCode = usp->runDynamicPage(UServer_Base::pClientImage);
         }
      }
#ifdef HAVE_LIBTCC
   else
      {
      UCServletPage* csp = (UCServletPage*)file_data->ptr;

      U_INTERNAL_ASSERT_POINTER(csp)
      U_INTERNAL_ASSERT_POINTER(csp->prog_main)
      U_INTERNAL_ASSERT_EQUALS(u_mime_index, U_csp)

      // retrieve information on specific HTML form elements
      // (such as checkboxes, radio buttons, and text fields), or uploaded files

      const char* argv[4096];
      uint32_t i, n = processForm();

      U_INTERNAL_ASSERT_MINOR(n, 4096)

      for (i = 0; i < n; ++i) argv[i] = (*form_name_value)[i].c_str();
                              argv[i] = 0;

      UClientImage_Base::wbuffer->setBuffer(U_CAPACITY);

      if (UServer_Base::isLog()) UServer_Base::mod_name->snprintf("[csp] ", 0);

      u_http_info.nResponseCode = csp->prog_main(n, argv);

      UClientImage_Base::wbuffer->size_adjust();
      }
#endif
}

U_NO_EXPORT bool UHTTP::runDynamicPage(UString* penvironment)
{
   U_TRACE(0, "UHTTP::runDynamicPage(%p)", penvironment)

   U_INTERNAL_ASSERT_POINTER(file_data)

   U_INTERNAL_DUMP("u_mime_index = %C u_is_cgi() = %b u_is_usp() = %b", u_mime_index, u_is_cgi(), u_is_usp())

   bool async      = false,
        as_service = (penvironment != 0);

   if (u_is_cgi() == false) manageServletRequest(as_service);
   else
      {
      UHTTP::ucgi* cgi = (UHTTP::ucgi*)file_data->ptr;

      U_INTERNAL_DUMP("cgi->dir = %S cgi->sh_script = %d cgi->interpreter = %S", cgi->dir, cgi->sh_script, cgi->interpreter)

      const char* cgi_dir = cgi->dir;

      // NB: we can't use relativ path because after we call chdir()...

      UString command(U_CAPACITY), path(U_CAPACITY);

      // NB: we can't use U_HTTP_URI_TO_TRACE because this function can be called by SSI...

      path.snprintf("%w/%s/%s", cgi_dir, cgi_dir + u__strlen(cgi_dir, __PRETTY_FUNCTION__) + 1);

      U_INTERNAL_DUMP("path = %.*S", U_STRING_TO_TRACE(path))

      if (cgi->interpreter) command.snprintf("%s %.*s", cgi->interpreter, U_STRING_TO_TRACE(path));
      else           (void) command.assign(path);

      // ULIB facility: check if present form data and convert them in parameters for shell script...

      if (cgi->sh_script) setCGIShellScript(command);

      UCommand cmd(command);

      // NB: if server is no preforked (ex: nodog) process the HTTP CGI request with fork....

      async = (as_service == false                   &&
               UServer_Base::preforked_num_kids == 0 &&
               UClientImage_Base::isPipeline()  == false);

      (void) processCGIRequest(cmd, penvironment, cgi_dir, async);
      }

   if (form_name_value->size()) resetForm(true);

   U_INTERNAL_DUMP("u_http_info.nResponseCode = %d", u_http_info.nResponseCode)

   U_INTERNAL_DUMP("wbuffer(%u) = %.*S ", UClientImage_Base::wbuffer->size(), U_STRING_TO_TRACE(*UClientImage_Base::wbuffer))
   U_INTERNAL_DUMP("   body(%u) = %.*S ", UClientImage_Base::body->size(),    U_STRING_TO_TRACE(*UClientImage_Base::body))

   switch (u_http_info.nResponseCode)
      {
      case HTTP_OK:
         {
         if (as_service == false)
            {
            // NB: we assume to have as response an HTML without HTTP headers...

            u_http_info.clength = UClientImage_Base::wbuffer->size();

            setCgiResponse(false, isCompressable(), false);
            }

         U_RETURN(true);
         }

      case HTTP_NOT_FOUND:       setNotFound();           break;
      case HTTP_BAD_METHOD:      setBadMethod();          break;
      case HTTP_BAD_REQUEST:     setBadRequest();         break;
      case HTTP_UNAVAILABLE:     setServiceUnavailable(); break;
      case HTTP_UNAUTHORIZED:    setUnAuthorized();       break;
      case HTTP_GATEWAY_TIMEOUT: setResponse(0, 0);       break;

      case 0:
      case HTTP_NO_CONTENT:
      case HTTP_MOVED_TEMP:
         {
         if (async      ||
             as_service ||
             processCGIOutput())
            {
            U_RETURN(true);
            }

         u_http_info.nResponseCode = HTTP_INTERNAL_ERROR;
         }

      default:
         {
         if (u_http_info.nResponseCode != HTTP_INTERNAL_ERROR) U_WARNING("received from dynamic page wrong response code: %d", u_http_info.nResponseCode);

         setInternalError();
         }
      break;
      }

   U_RETURN(false);
}

bool UHTTP::callService(const UString& path)
{
   U_TRACE(0, "UHTTP::callService(%.*S)", U_STRING_TO_TRACE(path))

   file->setPath(path, UClientImage_Base::environment);

   if (isFileInCache() == false)
      {
      pathname->setBuffer(U_CAPACITY);

      if (u_getsuffix(U_FILE_TO_PARAM(*file))) (void) pathname->snprintf("%.*s",    U_FILE_TO_TRACE(*file));
      else                                     (void) pathname->snprintf("%.*s.%s", U_FILE_TO_TRACE(*file), U_LIB_SUFFIX);

      file->setPath(*pathname);

      if (file->stat() == false)
         {
         setNotFound();

         goto end;
         }

      U_SRV_LOG("Called service not in cache: %.*S - inotify %s enabled", U_FILE_TO_TRACE(*file), UServer_Base::handler_inotify ? "is" : "NOT");

      manageDataForCache();

      callInitForAllUSP(pathname->rep, file_data);
      }

   U_INTERNAL_ASSERT_POINTER(file_data)

   u_mime_index = file_data->mime_index;

   if (runDynamicPage(UClientImage_Base::environment)) U_RETURN(true);

end:
   U_INTERNAL_ASSERT_MAJOR(u_http_info.nResponseCode, 0)

   U_RETURN(false);
}

/*
Set up CGI environment variables. The following table describes common CGI environment variables that the server
creates (some of these are not available with some servers):

CGI server variable Description
-------------------------------------------------------------------------------------------------------------------------------------------
AUTH_TYPE         - If the server supports user authentication, and the script is protected, the protocol-specific
                    authentication method used to validate the user.
PATH_INFO         - The remainder of the request URL 'path', designating the virtual 'location' of the request
                    target within the application. This may be an empty string, if the request URL targets the
                    application root and does not have a trailing slash. This value may be percent-encoded when I
                    originating from a URL. Scripts can be accessed by their virtual pathname, followed by this
                    extra information at the end of this path. This extra information is sent as PATH_INFO. If
                    non-empty, must start with /.
SCRIPT_NAME       - The initial portion of the request URL 'path' that corresponds to the application object, so that
                    the application knows its virtual 'location'. This may be an empty string, if the application
                    corresponds to the 'root' of the server. If non-empty, must start with /
REMOTE_USER       - If the server supports user authentication, and the script is protected, the username the user has
                    authenticated as. (Also available as AUTH_USER)
SERVER_PORT       - Port number to which the request was sent.
SERVER_NAME       - Server's hostname, DNS alias, or IP address as it appears in self-referencing URLs.
                    When SERVER_NAME and SERVER_PORT are combined with SCRIPT_NAME and PATH_INFO, these variables can be
                    used to complete the URL. Note, however, that HTTP_HOST, if present, should be used in preference to
                    SERVER_NAME for reconstructing the request URL. SERVER_NAME and SERVER_PORT can never be empty strings,
                    and so are always required.
REMOTE_HOST       - Hostname making the request. If the server does not have this information, it sets REMOTE_ADDR and does not set REMOTE_HOST.
REMOTE_ADDR       - IP address of the remote host making the request.
REMOTE_IDENT      - If the HTTP server supports RFC 931 identification, this variable is set to the remote username
                    retrieved from the server. Use this variable for logging only.
QUERY_STRING      - The portion of the request URL that follows the ?, if any. May be empty, but is always required!
CONTENT_TYPE      - For queries that have attached information, such as HTTP POST and PUT, this is the content type of the data.
REQUEST_METHOD    - The HTTP request method, such as 'GET' or 'POST'. This cannot ever be an empty string, and so is always required.
CONTENT_LENGTH    - Length of the content as given by the client. If given, must consist of digits only.
PATH_TRANSLATED   - Translated version of PATH_INFO after any virtual-to-physical mapping.
SERVER_PROTOCOL   - Name and revision of the information protocol this request came in with. Format: protocol/revision.
SERVER_SOFTWARE   - Name and version of the information server software answering the request (and running the gateway). Format: name/version.
GATEWAY_INTERFACE - CGI specification revision with which this server complies. Format: CGI/revision.

CERT_ISSUER       - Issuer field of the client certificate (O=MS, OU=IAS, CN=user name, C=USA).
CERT_SUBJECT      - Subject field of the client certificate.
CERT_SERIALNUMBER - Serial number field of the client certificate.
-------------------------------------------------------------------------------------------------------------------------------------------

The following table describes common CGI environment variables the browser creates and passes in the request header:

CGI client variable      Description
-------------------------------------------------------------------------------------------------------------------------------------------
HTTP_REFERER           - The referring document that linked to or submitted form data.
HTTP_USER_AGENT        - The browser that the client is currently using to send the request. Format: software/version library/version.
HTTP_IF_MODIFIED_SINCE - The last time the page was modified. The browser determines whether to set this variable, usually in response
                         to the server having sent the LAST_MODIFIED HTTP header. It can be used to take advantage of browser-side caching.
-------------------------------------------------------------------------------------------------------------------------------------------

HTTP_Variables: Variables corresponding to the client-supplied HTTP request headers (i.e., variables whose names begin with HTTP_).
                The presence or absence of these variables should correspond with the presence or absence of the appropriate HTTP
                header in the request. The environment must not contain the keys HTTP_CONTENT_TYPE or HTTP_CONTENT_LENGTH
                (use the versions without HTTP_).

There are the following restrictions:

1) One of SCRIPT_NAME or PATH_INFO must be set. PATH_INFO should be / if SCRIPT_NAME is empty. SCRIPT_NAME never should be /, but instead be empty.
2) http://${SERVER_NAME}:${SERVER_PORT}${SCRIPT_NAME}${PATH_INFO} will always be an accessible URL that points to the current script.

Example:
-------------------------------------------------------------------------------------------------------------------------------------------
PATH="/lib64/rc/sbin:/lib64/rc/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin"
UNIQUE_ID="b032zQoeAYMAAA-@BZwAAAAA"
REMOTE_ADDR=127.0.0.1
SCRIPT_NAME=/cgi-bin/printenv
SERVER_NAME=localhost
SERVER_PORT=80
REQUEST_URI="/cgi-bin/printenv"
REMOTE_PORT="35653"
SERVER_ADDR="127.0.0.1"
SERVER_ADMIN="root@localhost"
QUERY_STRING=
DOCUMENT_ROOT="/var/www/localhost/htdocs"
REQUEST_METHOD=GET
SERVER_SOFTWARE=Apache
SERVER_PROTOCOL=HTTP/1.1
SCRIPT_FILENAME="/var/www/localhost/cgi-bin/printenv"
SERVER_SIGNATURE="<address>Apache Server at localhost Port 80</address>\n"
GATEWAY_INTERFACE=CGI/1.1

HTTP_HOST="localhost"
HTTP_COOKIE="_saml_idp=dXJuOm1hY2U6dGVzdC5zXRo; _redirect_user_idp=urn%3Amace%3Atest.shib%3Afederation%3Alocalhost;"
HTTP_ACCEPT="text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png"
HTTP_KEEP_ALIVE="300"
HTTP_USER_AGENT="Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.8.1.14) Gecko/20080421 Firefox/2.0.0.14"
HTTP_CONNECTION="keep-alive"
HTTP_ACCEPT_CHARSET="ISO-8859-1,utf-8;q=0.7,*;q=0.7"
HTTP_ACCEPT_ENCODING="gzip, deflate"
HTTP_ACCEPT_LANGUAGE="en-us,en;q=0.5"
-------------------------------------------------------------------------------------------------------------------------------------------
*/

U_NO_EXPORT void UHTTP::add_HTTP_Variables(UStringRep* key, void* value)
{
   U_TRACE(0+256, "UHTTP::add_HTTP_Variables(%.*S,%.*S)", U_STRING_TO_TRACE(*key), U_STRING_TO_TRACE(*(UStringRep*)value))

   U_INTERNAL_ASSERT_POINTER(value)

   UString buffer(20U + key->size() + ((UStringRep*)value)->size()), name = UStringExt::toupper(U_STRING_TO_PARAM(*key));

   (void) buffer.snprintf("'HTTP_%*.s=%.*s'\n", U_STRING_TO_TRACE(name), U_STRING_TO_TRACE(*(UStringRep*)value));

   (void) string_HTTP_Variables->append(buffer);
}

UString UHTTP::getCGIEnvironment(bool bHTTP_Variables)
{
   U_TRACE(0, "UHTTP::getCGIEnvironment(%b)", bHTTP_Variables)

   UString _uri            = getRequestURI(), buffer(4000U + u_http_info.query_len + u_http_info.referer_len + u_http_info.user_agent_len);
   uint32_t agent          = getUserAgent();
   int remote_port         = UServer_Base::pClientImage->socket->remotePortNumber(); 
   const char* remote_addr = UServer_Base::client_address;

   U_INTERNAL_DUMP("u_mime_index = %C", u_mime_index)

   const char* cgi_dir = (u_is_cgi() ? (bHTTP_Variables = true, ((UHTTP::ucgi*)file_data->ptr)->dir) : "");

   U_INTERNAL_DUMP("cgi_dir = %S", cgi_dir)

   buffer.snprintf("CONTENT_LENGTH=%u\n"   // The first header must have the name "CONTENT_LENGTH" and a value that is body length in decimal.
                                           // The "CONTENT_LENGTH" header must always be present, even if its value is "0".
                // "REMOTE_HOST=%.*s\n"    // The hostname of the visitor (if your server has reverse-name-lookups on; otherwise this is IP address again)
                // "REMOTE_USER=%.*s\n"    // The visitor's username (for .htaccess-protected pages)
                // "SERVER_ADMIN=%.*s\n"   // The email address for your server's webmaster
                   "REMOTE_PORT=%d\n"      // The port the visitor is connected to on the web server
                   "REMOTE_ADDR=%s\n"      // The IP address of the visitor
                   "SESSION_ID=%s:%u\n"    // The IP address of the visitor        + HTTP_USER_AGENT hashed (NB: it is weak respect to netfilter MASQUERADE)
                   "REQUEST_ID=%s:%d:%u\n" // The IP address of the visitor + port + HTTP_USER_AGENT hashed
                   "SERVER_PROTOCOL=HTTP/1.%c\n"
                   // ext
                   "PWD=%w/%s\n"
                   "SCRIPT_FILENAME=%w%.*s\n" // The full pathname of the current CGI (is used by PHP for determining the name of script to execute)
                   // PHP
                   "REQUEST_METHOD=%.*s\n",   // dealing with POST requests
                   (isPOST() ? UClientImage_Base::body->size() : 0),
                   remote_port,
                   remote_addr,
                   remote_addr,              agent,
                   remote_addr, remote_port, agent,
                   (U_http_version ? U_http_version : '0'),
                   // ext
                   cgi_dir,
                   U_HTTP_URI_TO_TRACE,
                   // PHP
                   U_HTTP_METHOD_TO_TRACE);

#ifdef USE_LIBSSL
   if (UServer_Base::bssl)
      {
      X509* x509 = ((USSLSocket*)UServer_Base::pClientImage->socket)->getPeerCertificate();

      if (x509)
         {
         UString issuer  = UCertificate::getIssuer(x509),
                 subject = UCertificate::getSubject(x509);

         buffer.snprintf_add("'SSL_CLIENT_I_DN=%.*s'\n"
                             "'SSL_CLIENT_S_DN=%.*s'\n"
                             "SSL_CLIENT_CERT_SERIAL=%ld\n",
                             U_STRING_TO_TRACE(issuer),
                             U_STRING_TO_TRACE(subject),
                             UCertificate::getSerialNumber(x509));
         }

      (void) buffer.append(U_CONSTANT_TO_PARAM("HTTPS=on\n")); // "on" if the script is being called through a secure server
      }
#endif

   if (U_http_content_type_len) buffer.snprintf_add("'CONTENT_TYPE=%.*s'\n", U_HTTP_CTYPE_TO_TRACE);

   if (npathinfo == 0)
      {
      buffer.snprintf_add("SCRIPT_NAME=%.*s\n", U_HTTP_URI_TO_TRACE); // The interpreted pathname of the current CGI (relative to the document root)
      }
   else
      {
      UString scriptname(u_http_info.uri,                                   npathinfo),
                pathinfo(u_http_info.uri + npathinfo, u_http_info.uri_len - npathinfo);

      buffer.snprintf_add("SCRIPT_NAME=%.*s\n"
                            "PATH_INFO=%.*s\n",
                          U_STRING_TO_TRACE(scriptname),
                          U_STRING_TO_TRACE(pathinfo));
      }

   UMimeHeader requestHeader;
   UHashMap<UString>* prequestHeader = 0;

   if (bHTTP_Variables)
      {
      requestHeader.setIgnoreCase(true);

      if (requestHeader.parse(UClientImage_Base::request->c_pointer(u_http_info.startHeader), u_http_info.szHeader))
         {
         // The environment must not contain the keys HTTP_CONTENT_TYPE or HTTP_CONTENT_LENGTH (use the versions without HTTP_).

         requestHeader.removeHeader(*USocket::str_content_type);
         requestHeader.removeHeader(*USocket::str_content_length);

         if (requestHeader.empty() == false) prequestHeader = &(requestHeader.table);
         }
      }

   // The hostname of your server from header's request.
   // The difference between HTTP_HOST and U_HTTP_VHOST is that
   // HTTP_HOST can include the Ť:PORTť text, and U_HTTP_VHOST only the name

   if (U_http_host_len)
      {
                        buffer.snprintf_add("HTTP_HOST=%.*s\n",    U_HTTP_HOST_TO_TRACE);
      if (virtual_host) buffer.snprintf_add("VIRTUAL_HOST=%.*s\n", U_HTTP_VHOST_TO_TRACE);

      if (prequestHeader)
         {
         requestHeader.removeHeader(*USocket::str_host);

         if (requestHeader.empty()) prequestHeader = 0;
         }
      }

   // The visitor's cookie, if one is set
   // ------------------------------------------------------------------------------------------------------------------
   // Cookie: _saml_idp=dXJuOm1hY2U6dGVzdC5zXRo, _redirect_user_idp=urn%3Amace%3Atest.shib%3Afederation%3Alocalhost; ...
   // ------------------------------------------------------------------------------------------------------------------

   if (u_http_info.cookie_len)
      {
      UString cookie;

      if (getCookie(&cookie))
         {
         (void) buffer.append(U_CONSTANT_TO_PARAM("'ULIB_SESSION="));
         (void) buffer.append(*keyID);
         (void) buffer.append(U_CONSTANT_TO_PARAM("'\n"));
         }

      if (cookie.empty() == false)
         {
         (void) buffer.append(U_CONSTANT_TO_PARAM("'HTTP_COOKIE="));
         (void) buffer.append(cookie);
         (void) buffer.append(U_CONSTANT_TO_PARAM("'\n"));
         }

      if (prequestHeader)
         {
         requestHeader.removeHeader(*USocket::str_cookie);

         if (requestHeader.empty()) prequestHeader = 0;
         }
      }

   // The URL of the page that called your script

   if (u_http_info.referer_len)
      {
      buffer.snprintf_add("'HTTP_REFERER=%.*s'\n", U_HTTP_REFERER_TO_TRACE);

      if (prequestHeader)
         {
         requestHeader.removeHeader(*USocket::str_referer);

         if (requestHeader.empty()) prequestHeader = 0;
         }
      }

   // The browser type of the visitor

   if (u_http_info.user_agent_len)
      {
      buffer.snprintf_add("'HTTP_USER_AGENT=%.*s'\n", U_HTTP_USER_AGENT_TO_TRACE);

      if (prequestHeader)
         {
         requestHeader.removeHeader(*USocket::str_user_agent);

         if (requestHeader.empty()) prequestHeader = 0;
         }
      }

   if (U_http_accept_len)
      {
      buffer.snprintf_add("'HTTP_ACCEPT=%.*s'\n", U_HTTP_ACCEPT_TO_TRACE);

      if (prequestHeader)
         {
         requestHeader.removeHeader(*USocket::str_accept);

         if (requestHeader.empty()) prequestHeader = 0;
         }
      }

   if (U_http_accept_language_len)
      {
      buffer.snprintf_add("'HTTP_ACCEPT_LANGUAGE=%.*s'\n", U_HTTP_ACCEPT_LANGUAGE_TO_TRACE);

      if (prequestHeader)
         {
         requestHeader.removeHeader(*USocket::str_accept_language);

         if (requestHeader.empty()) prequestHeader = 0;
         }
      }

   if (buffer.isBinary())
      {
#  ifdef DEBUG
      char buf[4096];
#  endif

      U_INTERNAL_DUMP("buffer:\n"
                      "--------------------------------------\n"
                      "%s", u_memoryDump(buf, (unsigned char*)U_STRING_TO_PARAM(buffer)))

      U_RETURN_STRING(UString::getStringNull());
      }

   // contains the parameters of the request
   if (u_http_info.query_len) buffer.snprintf_add("'QUERY_STRING=%.*s'\n", U_HTTP_QUERY_TO_TRACE);

   // The interpreted pathname of the requested document or CGI (relative to the document root)
   buffer.snprintf_add("'REQUEST_URI=%.*s'\n", U_STRING_TO_TRACE(_uri));

   if (prequestHeader)
      {
      if (string_HTTP_Variables == 0) string_HTTP_Variables = U_NEW(UString(U_CAPACITY));

      prequestHeader->callForAllEntry(add_HTTP_Variables);

      (void) buffer.append(*string_HTTP_Variables);

      string_HTTP_Variables->clear();
      }

   (void) buffer.append(*geoip);
   (void) buffer.append(*UServer_Base::senvironment);

   U_ASSERT_EQUALS(buffer.isBinary(), false)

   U_RETURN_STRING(buffer);
}

U_NO_EXPORT void UHTTP::setCGIShellScript(UString& command)
{
   U_TRACE(0, "UHTTP::setCGIShellScript(%.*S)", U_STRING_TO_TRACE(command))

   U_INTERNAL_ASSERT_POINTER(form_name_value)

   // ULIB facility: check if present form data and convert it in parameters for shell script...

   char c;
   UString item;
   const char* ptr;
   uint32_t i, sz, n = processForm();

   for (i = 1; i < n; i += 2)
      {
      item = (*form_name_value)[i];

      // check for binary data (not valid)...

      if (item.empty() ||
          item.isBinary())
         {
         c    = '\'';
         ptr  = 0;
         sz   = 0;

         if (item.empty() == false)
            {
#        ifdef DEBUG
            char buffer[4096];
#        endif

            U_INTERNAL_DUMP("form item:\n"
                            "--------------------------------------\n"
                            "%s", u_memoryDump(buffer, (unsigned char*)U_STRING_TO_PARAM(item)))

            U_WARNING("Found binary data in form item: %.*S", U_STRING_TO_TRACE((*form_name_value)[i-1]));
            }
         }
      else
         {
         ptr = item.data();
         sz  = item.size();

         // find how to escape the param...

         c = (memchr(ptr, '"', sz) ? '\'' : '"');
         }

      (void) command.reserve(command.size() + sz + 4U);

      command.snprintf_add(" %c%.*s%c ", c, sz, ptr, c);
      }
}

/* X-Sendfile is a special, non-standard HTTP header. At first you might think it is no big deal, but think again.
 * It can be enabled in any CGI, FastCGI or SCGI backend. Basically its job is to instruct the web server to ignore
 * the content of the response and replace it by whatever is specified in the header. The main advantage of this is
 * that it will be server the one serving the file, making use of all its optimizations. It is useful for processing
 * script-output of e.g. php, perl, ruby or any cgi. This is particularly useful because it hands the load to server,
 * all the response headers from the backend are forwarded, the whole application uses a lot less resources and performs
 * several times faster not having to worry about a task best suited for a web server. You retain the ability to check for
 * special privileges or dynamically deciding anything contemplated by your backend logic, you speed up things a lot while
 * having more resources freed, and you can even specify the delivery of files outside of the web server's document root path.
 * Of course, this is to be done solely in controlled environments. In short, it offers a huge performance gain at absolutely
 * no cost. Note that the X-Sendfile feature also supports X-Accel-Redirect header, a similar feature offered by other web
 * servers. This is to allow the migration of applications supporting it without having to make major code rewrites.
 */

bool UHTTP::XSendfile(UString& _pathname, const UString& ext)
{
   U_TRACE(0, "UHTTP::XSendfile(%.*S,%.*S)", U_STRING_TO_TRACE(_pathname), U_STRING_TO_TRACE(ext))

   if (u_canonicalize_pathname(_pathname.data())) _pathname.size_adjust_force(); // NB: _pathname is referenced...

   file->setPath(_pathname);

   if (file->stat()    &&
       file->st_size   &&
       file->regular() &&
       file->open())
      {
      U_SRV_LOG("Header X-Sendfile found in CGI output: serving file %.*S", U_STRING_TO_TRACE(_pathname));

      UClientImage_Base::body->clear();
      UClientImage_Base::wbuffer->clear();

      if (ext.empty() == false)
         {
         U_INTERNAL_ASSERT(u_endsWith(U_STRING_TO_PARAM(ext), U_CONSTANT_TO_PARAM(U_CRLF)))

         UString request(300U + _pathname.size() + ext.size());

         request.snprintf("GET %.*s HTTP/1.1\r\n" \
                          "%.*s" \
                          "\r\n",
                          U_STRING_TO_TRACE(_pathname),
                          U_STRING_TO_TRACE(ext));

         U_INTERNAL_DUMP("U_http_is_accept_gzip = %C", U_http_is_accept_gzip)

         char c = U_http_is_accept_gzip;

         (void) readHeader(0, request);
         (void) checkRequestForHeader(request);

         U_http_is_accept_gzip = c;
         }

      file->_unlink();

      file_data->fd   = file->fd;
      file_data->size = file->st_size;

      UString tmp;
      U_http_is_navigation = true;

      processGetRequest(UString::getStringNull(), tmp);

      if (UServer_Base::pClientImage->sfd == 0) file->close();
      else
         {
         U_INTERNAL_ASSERT_EQUALS(UServer_Base::pClientImage->sfd, file_data->fd)

         UServer_Base::pClientImage->bclose |= U_CLOSE;
         }

      U_RETURN(true);
      }

   U_RETURN(false);
}

U_NO_EXPORT bool UHTTP::splitCGIOutput(const char*& ptr1, const char* ptr2, uint32_t endHeader, UString& ext)
{
   U_TRACE(0, "UHTTP::splitCGIOutput(%p,%p,%u,%.*S)", ptr1, ptr2, endHeader, U_STRING_TO_TRACE(ext))

   uint32_t pos = UClientImage_Base::wbuffer->distance(ptr1);

   if (pos) ext = UClientImage_Base::wbuffer->substr(0U, pos);

   ptr1 = (const char*) memchr(ptr2, '\r', endHeader - pos);

   if (ptr1)
      {
      pos = UClientImage_Base::wbuffer->distance(ptr1) + U_CONSTANT_SIZE(U_CRLF); // NB: we cut \r\n

      U_INTERNAL_ASSERT_MINOR(pos, endHeader)

      uint32_t diff = endHeader - pos;

      U_INTERNAL_DUMP("diff = %u pos = %u endHeader = %u", diff, pos, endHeader)

      if (diff > U_CONSTANT_SIZE(U_CRLF2)) ext += UClientImage_Base::wbuffer->substr(pos, diff - U_CONSTANT_SIZE(U_CRLF)); // NB: we cut one couple of \r\n

      U_INTERNAL_DUMP("value = %.*S ext = %.*S", (uint32_t)((ptrdiff_t)ptr1 - (ptrdiff_t)ptr2), ptr2, U_STRING_TO_TRACE(ext))

      U_RETURN(true);
      }

   U_RETURN(false);
}

bool UHTTP::processCGIOutput()
{
   U_TRACE(0, "UHTTP::processCGIOutput()")

   /*
   CGI Script Output

   The script sends its output to stdout. This output can either be a document generated by the script, or instructions to the server
   for retrieving the desired output.

   Script naming conventions

   Normally, scripts produce output which is interpreted and sent back to the client. An advantage of this is that the scripts do not
   need to send a full HTTP/1.x header for every request.

   Some scripts may want to avoid the extra overhead of the server parsing their output, and talk directly to the client. In order to
   distinguish these scripts from the other scripts, CGI requires that the script name begins with nph- if a script does not want the
   server to parse its header. In this case, it is the script's responsibility to return a valid HTTP response to the client.

   Parsed headers

   The output of scripts begins with a small header. This header consists of text lines, in the same format as an HTTP header,
   terminated by a blank line (a line with only a linefeed or CR/LF).

   Any headers which are not server directives are sent directly back to the client. Currently, this specification defines three server
   directives:

   * Status:       This is used to give the server an HTTP status line to send to the client. The format is nnn xxxxx, where nnn is
                   the 3-digit status code, and xxxxx is the reason string, such as "Forbidden".

   * Content-type: This is the MIME type of the document you are returning.

   * Location:     This is used to specify to the server that you are returning a reference to a document rather than an actual document.
                   If the argument to this is a URL, the server will issue a redirect to the client.
   */

   const char* ptr;
   const char* endptr;
   const char* location;
   uint32_t endHeader, sz = UClientImage_Base::wbuffer->size();
   bool connection_close, header_content_length, header_content_type, content_encoding;

   U_INTERNAL_DUMP("sz = %u", sz)

   if (sz == 0) goto error;

   ptr = UClientImage_Base::wbuffer->data();

   U_INTERNAL_DUMP("ptr = %.*S", U_min(20, sz), ptr)

   // NB: we check for <h(1|tml)> (HTML without HTTP headers..)

   while (u__isspace(*ptr)) ++ptr; // skip space...

   if (          ptr[0]  == '<' &&
       u__toupper(ptr[1]) == 'H')
      {
      goto no_headers;
      }

rescan:
   endHeader = u_findEndHeader(ptr, sz);

   // NB: endHeader comprende anche la blank line...

   U_INTERNAL_DUMP("endHeader = %u u_line_terminator_len = %d", endHeader, u_line_terminator_len)

   if (endHeader == U_NOT_FOUND)
      {
no_headers: // NB: we assume to have HTML without HTTP headers...

#  ifdef USE_LIBMAGIC
      U_ASSERT(U_STRNEQ(UMagic::getType(ptr, sz).data(), "text"))
#  endif

      u_http_info.clength       = sz;
      u_http_info.nResponseCode = HTTP_OK;

      setCgiResponse(false, isCompressable(), false);

      U_RETURN(true);
      }

   if (u_line_terminator_len == 1)
      {
      UString tmp                 = UStringExt::dos2unix(UClientImage_Base::wbuffer->substr(0U, endHeader), true) +
                                                         UClientImage_Base::wbuffer->substr(    endHeader);
      *UClientImage_Base::wbuffer = tmp;

      sz  = UClientImage_Base::wbuffer->size();
      ptr = UClientImage_Base::wbuffer->data();

      goto rescan;
      }

   U_INTERNAL_ASSERT_EQUALS(u_line_terminator_len, 2)

   endptr           = UClientImage_Base::wbuffer->c_pointer(endHeader);
   connection_close = header_content_length = header_content_type = content_encoding = false;

   u_http_info.clength       = sz - endHeader;
   u_http_info.nResponseCode = HTTP_OK;

   U_INTERNAL_DUMP("u_http_info.clength = %u", u_http_info.clength)

   while (ptr < endptr)
      {
      U_INTERNAL_DUMP("ptr = %.*S", 20, ptr)

      switch (u__toupper(*ptr))
         {
         case 'H': // response line: HTTP/1.n nnn <ssss>
            {
            if (scanfHeader(ptr, endptr - ptr)) // check for script's responsibility to return a valid HTTP response to the client...
               {
               U_INTERNAL_DUMP("wbuffer(%u) = %.*S", UClientImage_Base::wbuffer->size(), U_STRING_TO_TRACE(*UClientImage_Base::wbuffer))

               U_INTERNAL_DUMP("U_http_is_connection_close = %d", U_http_is_connection_close)

               if (U_http_is_connection_close == U_MAYBE)
                  {
                  U_http_is_connection_close = (u_find(ptr + 15, endHeader, U_CONSTANT_TO_PARAM("Connection: close")) ? U_YES : U_NOT);

                  U_INTERNAL_DUMP("U_http_is_connection_close = %d", U_http_is_connection_close)
                  }

               U_RETURN(true);
               }
            }
         break;

         case 'L': // check if is used to specify to the server that you are returning a reference to a document...
            {
            U_INTERNAL_DUMP("check 'Location: ...'")

            if (U_STRNEQ(ptr+1, "ocation: "))
               {
               UString ext;

               location = ptr + U_CONSTANT_SIZE("Location: ");

               if (splitCGIOutput(ptr, location, endHeader, ext))
                  {
                  setRedirectResponse(0, ext, location, ptr - location);

                  U_RETURN(true);
                  }

               goto error;
               }
            }
         break;

         case 'X':
            {
            U_INTERNAL_DUMP("check 'X-Sendfile: ...' or 'X-Accel-Redirect: ...'")

            /* X-Sendfile is a special, non-standard HTTP header. At first you might think it is no big deal, but think again.
             * It can be enabled in any CGI, FastCGI or SCGI backend. Basically its job is to instruct the web server to ignore
             * the content of the response and replace it by whatever is specified in the header. The main advantage of this is
             * that it will be server the one serving the file, making use of all its optimizations. It is useful for processing
             * script-output of e.g. php, perl, ruby or any cgi. This is particularly useful because it hands the load to server,
             * all the response headers from the backend are forwarded, the whole application uses a lot less resources and performs
             * several times faster not having to worry about a task best suited for a web server. You retain the ability to check for
             * special privileges or dynamically deciding anything contemplated by your backend logic, you speed up things a lot while
             * having more resources freed, and you can even specify the delivery of files outside of the web server's document root path.
             * Of course, this is to be done solely in controlled environments. In short, it offers a huge performance gain at absolutely
             * no cost. Note that the X-Sendfile feature also supports X-Accel-Redirect header, a similar feature offered by other web
             * servers. This is to allow the migration of applications supporting it without having to make major code rewrites.
             */

            location = ptr+1;

                 if (U_STRNEQ(location, "-Sendfile: "))       location += U_CONSTANT_SIZE("X-Sendfile:");
            else if (U_STRNEQ(location, "-Accel-Redirect: ")) location += U_CONSTANT_SIZE("X-Accel-Redirect:");

            if (location > (ptr+1))
               {
               UString ext;

               if (splitCGIOutput(ptr, location, endHeader, ext))
                  {
                  uint32_t len = ptr - location;

                  pathname->setBuffer(u_cwd_len + 1 + len);

                  pathname->snprintf(location[0] == '/' ?    "%.*s"
                                                        : "%w/%.*s", len, location);

                  if (XSendfile(*pathname, ext)) U_RETURN(true);
                  }

               goto error;
               }
            }
         break;

         case 'S':
            {
            // check if is used to give the server an HTTP status line to send to the client...
            // ---------------------------------------------------------------------------------------------------------------------------------------------
            // Ex: "Status: 503 Service Unavailable\r\nX-Powered-By: PHP/5.2.6-pl7-gentoo\r\nContent-Type: text/html;charset=utf-8\r\n\r\n<!DOCTYPE html"...

            U_INTERNAL_DUMP("check 'Status: ...'")

            if (U_STRNEQ(ptr+1, "tatus: "))
               {
               location = ptr + U_CONSTANT_SIZE("Status: ");

               u_http_info.nResponseCode = strtol(location, 0, 0);

               U_INTERNAL_DUMP("u_http_info.nResponseCode = %d", u_http_info.nResponseCode)

               location = (const char*) memchr(location, '\n', sz);

               if (location)
                  {
                  uint32_t diff = (location - ptr) + 1; // NB: we cut also \n...

                  U_INTERNAL_ASSERT_MINOR(diff,512)
                  U_INTERNAL_ASSERT_MINOR(diff, endHeader)

                  sz        -= diff;
                  endHeader -= diff;

                  U_INTERNAL_DUMP("diff = %u sz = %u endHeader = %u", diff, sz, endHeader)

                  UClientImage_Base::wbuffer->erase(0, diff);

                  U_INTERNAL_DUMP("wbuffer(%u) = %.*S", sz, U_STRING_TO_TRACE(*UClientImage_Base::wbuffer))

                  U_ASSERT_EQUALS(sz, UClientImage_Base::wbuffer->size())

                  ptr    = UClientImage_Base::wbuffer->data();
                  endptr = UClientImage_Base::wbuffer->c_pointer(endHeader);

                  continue;
                  }

               goto error;
               }

            // ULIB facility: check for request TODO timed session cookies...

            U_INTERNAL_DUMP("check 'Set-Cookie: TODO['")

            if (U_STRNEQ(ptr+1, "et-Cookie: TODO["))
               {
               uint32_t pos1,
                        pos2 = U_CONSTANT_SIZE("Set-Cookie: "),
                        n1   = U_CONSTANT_SIZE("TODO[");

               ptr += pos2;
               pos1 = UClientImage_Base::wbuffer->distance(ptr);

               ptr     += n1;
               location = ptr;

               ptr = (const char*) memchr(location, ']', sz);

               if (ptr)
                  {
                  uint32_t len = ptr - location;

                  n1 += len + 1; // ']'

                  setCookie(UClientImage_Base::wbuffer->substr(location, len));

                  uint32_t n2   = set_cookie->size() - pos2,
                           diff = n2 - n1;

                  sz        += diff;
                  endHeader += diff;

                  U_INTERNAL_DUMP("diff = %u sz = %u endHeader = %u", diff, sz, endHeader)

                  U_INTERNAL_ASSERT_MINOR(diff,512)

                  (void) UClientImage_Base::wbuffer->replace(pos1, n1, *set_cookie, pos2, n2);

                  set_cookie->setEmpty();

                  U_INTERNAL_DUMP("wbuffer(%u) = %#.*S", UClientImage_Base::wbuffer->size(), U_STRING_TO_TRACE(*UClientImage_Base::wbuffer))

                  U_ASSERT_EQUALS(sz, UClientImage_Base::wbuffer->size())

                  // for next parsing...

                  ptr    = UClientImage_Base::wbuffer->c_pointer(pos1 + n2 + 2);
                  endptr = UClientImage_Base::wbuffer->c_pointer(endHeader);

                  continue;
                  }

               goto error;
               }
            }
         break;

         case 'C':
            {
            // check if is used to specify to the server to close the connection...

            U_INTERNAL_DUMP("check 'Connection: close'")

            if (U_STRNEQ(ptr+1, "onnection: close"))
               {
               connection_close = true;

               ptr += U_CONSTANT_SIZE("Connection: close") + 2;

               continue;
               }

            U_INTERNAL_DUMP("check 'Content-...: ...'")

            if (U_STRNEQ(ptr+1, "ontent-"))
               {
               ptr += U_CONSTANT_SIZE("Content-");

               char c = u__toupper(*ptr++);

               if (c == 'T' &&
                   U_STRNEQ(ptr, "ype: "))
                  {
                  ptr += U_CONSTANT_SIZE("ype: ");

                  header_content_type = true;

                  U_INTERNAL_DUMP("header_content_type = %b", header_content_type)

                  if (U_STRNEQ(ptr, "text/") == false) content_encoding = true;

                  U_INTERNAL_DUMP("content_encoding = %b", content_encoding)
                  }
               else if (c == 'L' &&
                        U_STRNEQ(ptr, "ength:"))
                  {
                  ptr += U_CONSTANT_SIZE("ength:");

                  header_content_length = true;

                  U_INTERNAL_DUMP("header_content_length = %b", header_content_length)

                  uint32_t pos = UClientImage_Base::wbuffer->distance(ptr);

                  if (checkContentLength(*UClientImage_Base::wbuffer, u_http_info.clength, pos)) // NB: with drupal can happens...!!!
                     {
                     sz     = UClientImage_Base::wbuffer->size();
                     ptr    = UClientImage_Base::wbuffer->c_pointer(pos);
                     endptr = UClientImage_Base::wbuffer->c_pointer(endHeader);
                     }
                  }
               else if (c == 'E' &&
                        U_STRNEQ(ptr, "ncoding: "))
                  {
                  content_encoding = true;

                  ptr += U_CONSTANT_SIZE("ncoding: ");
                  }
               }
            }
         break;
         }

      ptr = (const char*) memchr(ptr, '\n', sz);

      if (ptr == 0) goto error;

      ++ptr;
      }

   U_INTERNAL_ASSERT_MAJOR(endHeader, 0)

   U_INTERNAL_DUMP("u_http_info.clength = %u", u_http_info.clength)

   if (u_http_info.clength)
      {
      if (header_content_length)
         {
         *UClientImage_Base::body    = *UClientImage_Base::wbuffer;
         *UClientImage_Base::wbuffer = getHeaderForResponse(*str_ulib_header, connection_close);
         }
      else
         {
         bool bcompress = (content_encoding == false && isCompressable());

         setCgiResponse(header_content_type, bcompress, connection_close);
         }
      }
   else
      {
      // NB: no body...it's ok Content-Length: 0...

      if (header_content_length == false)
         {
         u_http_info.nResponseCode = HTTP_NO_CONTENT;

         (void) UClientImage_Base::wbuffer->insert(0, U_CONSTANT_TO_PARAM("Content-Length: 0\r\n"));
         }

      UClientImage_Base::body->clear();

      *UClientImage_Base::wbuffer = getHeaderForResponse(*UClientImage_Base::wbuffer, connection_close);
      }

   U_RETURN(true);

error:
   U_SRV_LOG("WARNING: UHTTP::processCGIOutput() failed...");

   setInternalError(); // set internal error response...

   U_RETURN(false);
}

bool UHTTP::processCGIRequest(UCommand& cmd, UString* penv, const char* cgi_dir, bool& async)
{
   U_TRACE(0, "UHTTP::processCGIRequest(%p,%p,%S,%b)", &cmd, penv, cgi_dir, async)

   static int fd_stderr = UServices::getDevNull("/tmp/processCGIRequest.err");

   // process the CGI or script request

   U_INTERNAL_DUMP("method = %.*S method_type = %C URI = %.*S", U_HTTP_METHOD_TO_TRACE, U_http_method_type, U_HTTP_URI_TO_TRACE)

   if (cmd.checkForExecute() == false)
      {
      setForbidden();

      U_RETURN(false);
      }

   if (async)
      {
      if (UServer_Base::parallelization())
         {
         if (form_name_value->size()) resetForm(false);

         // NB: UServer_Base::parallelization() set UClientImage_Base::write_off to true...

         U_RETURN(true);
         }

      async = false; // NB: we need to distinghish between child and parent...
      }

   UString environment;

   if (penv) environment = *penv;

   if (environment.empty())
      {
      environment = getCGIEnvironment(true);

      if (environment.empty())
         {
         setBadRequest();

         U_RETURN(false);
         }
      }

   cmd.setEnvironment(&environment);

   /* When a url ends by "cgi-bin/" it is assumed to be a cgi script.
    * The server changes directory to the location of the script and
    * executes it after setting QUERY_STRING and other environment variables.
    */

   if (cgi_dir[0]) (void) UFile::chdir(cgi_dir, true);

   // execute script...

   U_INTERNAL_DUMP("u_http_info.nResponseCode = %d", u_http_info.nResponseCode)

   u_http_info.nResponseCode = 0;

   UString* pinput = (isPOST() == false || UClientImage_Base::body->empty() ? 0 : UClientImage_Base::body);

   bool result = cmd.execute(pinput, UClientImage_Base::wbuffer, -1, fd_stderr);

   if (cgi_dir[0]) (void) UFile::chdir(0, true);

   UServer_Base::logCommandMsgError(cmd.getCommand(), false);

   cmd.reset(penv);

   if (result == false)
      {
      if (UCommand::isTimeout()) u_http_info.nResponseCode = HTTP_GATEWAY_TIMEOUT;
      else
         {
         // NB: exit_value consists of the least significant 8 bits of the status argument that the child specified in a call to exit()...

         if (UCommand::exit_value > 128 &&
             U_IS_HTTP_ERROR(UCommand::exit_value + 256))
            {
            u_http_info.nResponseCode = UCommand::exit_value + 256;
            }
         else if (UClientImage_Base::wbuffer->empty())
            {
            u_http_info.nResponseCode = HTTP_INTERNAL_ERROR;
            }
         }

      U_RETURN(false);
      }

   U_RETURN(true);
}

bool UHTTP::checkContentLength(UString& x, uint32_t length, uint32_t pos)
{
   U_TRACE(0, "UHTTP::checkContentLength(%.*S,%u,%u)", U_STRING_TO_TRACE(x), length, pos)

   const char* ptr;

   if (pos != U_NOT_FOUND) ptr = x.c_pointer(pos);  
   else
      {
      pos = x.find(*USocket::str_content_length);

      U_INTERNAL_ASSERT_DIFFERS(pos, U_NOT_FOUND)

      ptr = x.c_pointer(pos + USocket::str_content_length->size() + 1);
      }

   if (u__isspace(*ptr)) ++ptr; // NB: weighttp need at least a space...

   char* nptr;
   uint32_t clength = (uint32_t) strtoul(ptr, &nptr, 0);

   U_INTERNAL_DUMP("ptr = %.*S clength = %u", 20, ptr, clength)

   if (clength != length)
      {
      char bp[12];

      uint32_t sz_len1 = nptr - ptr,
               sz_len2 = u__snprintf(bp, sizeof(bp), "%u", length);

      U_INTERNAL_DUMP("sz_len1 = %u sz_len2 = %u", sz_len1, sz_len2)

      while (u__isspace(*ptr))
         {
         if (sz_len1 == sz_len2) break;

         ++ptr;
         --sz_len1;
         }

      pos = x.distance(ptr);

      (void) x.replace(pos, sz_len1, bp, sz_len2);

      U_INTERNAL_DUMP("x(%u) = %#.*S", x.size(), U_STRING_TO_TRACE(x))

      U_RETURN(true);
      }

   U_RETURN(false);
}

typedef struct { uint32_t start, end; } HTTPRange;

/**
 * The Range: header is used with a GET request.
 *
 * For example assume that will return only a portion (let's say the first 32 bytes) of the requested resource...
 * Range: bytes=0-31
 *
 * If @end is non-negative, then @start and @end represent the bounds
 * of the range, counting from %0. (Eg, the first 500 bytes would be
 * represented as @start = %0 and @end = %499.)
 *
 * If @end is %-1 and @start is non-negative, then this represents a
 * range starting at @start and ending with the last byte of the
 * requested resource body. (Eg, all but the first 500 bytes would be
 * @start = %500, and @end = %-1.)
 *
 * If @end is %-1 and @start is negative, then it represents a "suffix
 * range", referring to the last -@start bytes of the resource body.
 * (Eg, the last 500 bytes would be @start = %-500 and @end = %-1.)
 *
 * The If-Range: header allows a client to "short-circuit" the request (conditional GET).
 * Informally, its meaning is `if the entity is unchanged, send me the part(s) that I am
 * missing; otherwise, send me the entire new entity'.
 *
 * If-Range: ( entity-tag | HTTP-date )
 *
 * If the client has no entity tag for an entity, but does have a Last-Modified date, it
 * MAY use that date in an If-Range header. (The server can distinguish between a valid
 * HTTP-date and any form of entity-tag by examining no more than two characters.) The If-Range
 * header SHOULD only be used together with a Range header, and MUST be ignored if the request
 * does not include a Range header, or if the server does not support the sub-range operation. 
 *
 */

U_NO_EXPORT bool UHTTP::checkGetRequestIfRange(const UString& etag)
{
   U_TRACE(0, "UHTTP::checkGetRequestIfRange(%.*S)", U_STRING_TO_TRACE(etag))

   const char* ptr = getHeaderValuePtr(*USocket::str_if_range, false);

   if (ptr)
      {
      if (*ptr == '"') // entity-tag
         {
         if (etag.equal(ptr, etag.size()) == false) U_RETURN(false);
         }
      else // HTTP-date
         {
         time_t since = UTimeDate::getSecondFromTime(ptr, true);

         U_INTERNAL_DUMP("since = %ld", since)
         U_INTERNAL_DUMP("mtime = %ld", file->st_mtime)

         if (file->st_mtime > since) U_RETURN(false);
         }
      }

   U_RETURN(true);
}

U_NO_EXPORT __pure int UHTTP::sortRange(const void* a, const void* b)
{
   U_TRACE(0, "UHTTP::sortRange(%p,%p)", a, b)

   HTTPRange* ra = *(HTTPRange**)a;
   HTTPRange* rb = *(HTTPRange**)b;

   U_INTERNAL_DUMP("ra->start = %u ra->end = %u", ra->start, ra->end)
   U_INTERNAL_DUMP("rb->start = %u rb->end = %u", rb->start, rb->end)

   uint32_t diff = ra->start - rb->start;

   U_INTERNAL_DUMP("diff = %u", diff)

   U_RETURN(diff);
}

U_NO_EXPORT void UHTTP::setResponseForRange(uint32_t _start, uint32_t _end, uint32_t _header, UString& ext)
{
   U_TRACE(0, "UHTTP::setResponseForRange(%u,%u,%u,%.*S)", _start, _end, _header, U_STRING_TO_TRACE(ext))

   // Single range

   U_INTERNAL_ASSERT(_start <= _end)
   U_INTERNAL_ASSERT_RANGE(_start,_end,range_size-1)

   UString tmp(100U);

   tmp.snprintf("Content-Range: bytes %u-%u/%u\r\n", _start, _end, range_size);

   range_size = _end - _start + 1;

   if (ext.empty() == false) (void) checkContentLength(ext, _header + range_size);

   (void) ext.insert(0, tmp);

   U_INTERNAL_DUMP("ext = %.*S", U_STRING_TO_TRACE(ext))
}

// return U_YES     - ok    - HTTP response     complete 
// return U_PARTIAL - ok    - HTTP response NOT complete 
// return U_NOT     - error - HTTP response     complete

U_NO_EXPORT int UHTTP::checkGetRequestForRange(UString& ext, const UString& data)
{
   U_TRACE(0, "UHTTP::checkGetRequestForRange(%.*S,%.*S)", U_STRING_TO_TRACE(ext), U_STRING_TO_TRACE(data))
 
   U_INTERNAL_ASSERT_MAJOR(U_http_range_len, 0)

   char* pend;
   HTTPRange* cur;
   HTTPRange* prev;
   const char* spec;
   UVector<HTTPRange*> array;
   UVector<UString> range_list;
   uint32_t i, n, _end, cur_start, cur_end;
   UString range(u_http_info.range, U_http_range_len), item;

   for (i = 0, n = range_list.split(u_http_info.range, U_http_range_len, ','); i < n; ++i)
      {
      item = range_list[i];
      spec = item.data();

      U_INTERNAL_DUMP("spec = %.*S", 10, spec)

      if (*spec == '-')
         {
         cur_start = strtol(spec, &pend, 0) + range_size;
         cur_end   = range_size - 1;
         }
      else
         {
         cur_start = strtol(spec, &pend, 0);

         if (*pend == '-') ++pend;

         U_INTERNAL_DUMP("item.remain(pend) = %u", item.remain(pend))

         cur_end = (item.remain(pend) ? strtol(pend, &pend, 0) : range_size - 1);
         }

      U_INTERNAL_DUMP("cur_start = %u cur_end = %u", cur_start, cur_end)

      if (cur_end >= range_size) cur_end = range_size - 1;

      if (cur_start <= cur_end)
         {
         U_INTERNAL_ASSERT_RANGE(cur_start,cur_end,range_size-1)

         cur = new HTTPRange;

         cur->end   = cur_end;
         cur->start = cur_start;

         array.push(cur);
         }
      }

   n = array.size();

   if (n > 1)
      {
      array.sort(sortRange);

      for (i = 1, n = array.size(); i < n; ++i)
         {
         cur  = array[i];
         prev = array[i-1];

         U_INTERNAL_DUMP("prev->start = %u prev->end = %u", prev->start, prev->end)
         U_INTERNAL_DUMP(" cur->start = %u  cur->end = %u",  cur->start,  cur->end)

         if (cur->start <= prev->end)
            {
            prev->end = U_max(prev->end, cur->end);

            array.erase(i);
            }
         }

      n = array.size();
      }

   if (n == 0)
      {
      u_http_info.nResponseCode = HTTP_REQ_RANGE_NOT_OK;

      setResponse(0, 0);

      U_RETURN(U_NOT);
      }

   if (n == 1) // Single range
      {
      cur = array[0];

      setResponseForRange((range_start = cur->start), cur->end, 0, ext);

      U_RETURN(U_PARTIAL);
      }

   /* Multiple ranges, so build a multipart/byteranges response
   --------------------------
   GET /index.html HTTP/1.1
   Host: www.unirel.com
   User-Agent: curl/7.21.0 (x86_64-pc-linux-gnu) libcurl/7.21.0 GnuTLS/2.10.0 zlib/1.2.5
   Range: bytes=100-199,500-599

   --------------------------
   HTTP/1.1 206 Partial Content
   Date: Fri, 09 Jul 2010 10:27:52 GMT
   Server: Apache/2.0.49 (Linux/SuSE)
   Last-Modified: Fri, 06 Nov 2009 17:59:33 GMT
   Accept-Ranges: bytes
   Content-Length: 431
   Content-Type: multipart/byteranges; boundary=48af1db00244c25fa


   --48af1db00244c25fa
   Content-type: text/html; charset=ISO-8859-1
   Content-range: bytes 100-199/598

   ............
   --48af1db00244c25fa
   Content-type: text/html; charset=ISO-8859-1
   Content-range: bytes 500-597/598

   ............
   --48af1db00244c25fa--
   --------------------------
   */

   uint32_t start;
   UString tmp(100U);
   const char* ptr = tmp.data();

   tmp.snprintf("Content-Length: %u", range_size);

   UMimeMultipartMsg response("byteranges", UMimeMultipartMsg::NONE, ptr, false);

   for (i = 0; i < n; ++i)
      {
      cur   = array[i];
      _end  = cur->end;
      start = cur->start;

      U_INTERNAL_ASSERT(start <= _end)
      U_INTERNAL_ASSERT_RANGE(start,_end,range_size-1)

      tmp.snprintf("Content-Range: bytes %u-%u/%u", start, _end, range_size);

      response.add(UMimeMultipartMsg::section(data.substr(start, _end - start + 1), U_CTYPE_HTML, UMimeMultipartMsg::NONE, "", "", ptr));
      }

   UString msg;
   uint32_t content_length = response.message(msg);

   (void) checkContentLength(msg, content_length);

#ifdef DEBUG
   (void) UFile::writeToTmpl("/tmp/byteranges", msg);
#endif

   u_http_info.nResponseCode   = HTTP_PARTIAL;
   *UClientImage_Base::wbuffer = getHeaderForResponse(msg, false);

   U_RETURN(U_YES);
}

#define U_NO_If_Unmodified_Since // I think it's not very much used...

U_NO_EXPORT bool UHTTP::checkGetRequestIfModified()
{
   U_TRACE(0, "UHTTP::checkGetRequestIfModified()")

   U_INTERNAL_ASSERT(*UClientImage_Base::request)

   /*
   The If-Modified-Since: header is used with a GET request. If the requested resource has been modified since the given date,
   ignore the header and return the resource as you normally would. Otherwise, return a "304 Not Modified" response, including
   the Date: header and no message body, like

   HTTP/1.1 304 Not Modified
   Date: Fri, 31 Dec 1999 23:59:59 GMT
   [blank line here]
   */

   if (u_http_info.if_modified_since)
      {
      U_INTERNAL_DUMP("since = %ld", u_http_info.if_modified_since)
      U_INTERNAL_DUMP("mtime = %ld", file->st_mtime)

      if (file->st_mtime <= u_http_info.if_modified_since)
         {
         u_http_info.nResponseCode = HTTP_NOT_MODIFIED;

         setResponse(0, 0);

         U_RETURN(false);
         }
      }
#ifndef U_NO_If_Unmodified_Since
   else
      {
      /*
      The If-Unmodified-Since: header is similar, but can be used with any method. If the requested resource has not been modified
      since the given date, ignore the header and return the resource as you normally would. Otherwise, return a "412 Precondition Failed"
      response, like

      HTTP/1.1 412 Precondition Failed
      Date: Fri, 31 Dec 1999 23:59:59 GMT
      [blank line here]
      */

      const char* ptr = getHeaderValuePtr(*USocket::str_if_unmodified_since, false);

      if (ptr)
         {
         time_t since = UTimeDate::getSecondFromTime(ptr, true);

         U_INTERNAL_DUMP("since = %ld", since)
         U_INTERNAL_DUMP("mtime = %ld", file->st_mtime)

         if (file->st_mtime > since)
            {
            U_http_is_connection_close = U_YES;
            u_http_info.nResponseCode  = HTTP_PRECON_FAILED;

            setResponse(0, 0);

            U_RETURN(false);
            }
         }
      }
#endif

   U_RETURN(true);
}

void UHTTP::processGetRequest()
{
   U_TRACE(0, "UHTTP::processGetRequest()")

   U_INTERNAL_ASSERT_EQUALS((bool)*UClientImage_Base::body, false)

   bool result;
   UString etag, ext(U_CAPACITY);

   /* 
    * If the browser has to validate a component, it uses the If-None-Match header to pass the ETag back to
    * the origin server. If the ETags match, a 304 status code is returned reducing the response...
    *
    * For me it's enough Last-Modified: ...

   etag = file->etag();

   const char* ptr = getHeaderValuePtr(*USocket::str_if_none_match, false);

   if (ptr)
      {
      U_INTERNAL_ASSERT_EQUALS(*ptr, '"') // entity-tag

      if (etag.equal(ptr, etag.size()))
         {
         u_http_info.nResponseCode = HTTP_NOT_MODIFIED;

         setResponse(0, 0);

         return;
         }
      }

   ext.snprintf("Etag: %.*s\r\n", U_STRING_TO_TRACE(etag));
   */

   if (file->dir())
      {
      // we have a directory...

      if (u_fnmatch(U_FILE_TO_PARAM(*file), U_CONSTANT_TO_PARAM("servlet"), 0)) // NB: servlet is forbidden...
         {
         setForbidden(); // set forbidden error response...

         return;
         }

      // Check if there is an index file (index.html) in the directory... (we check in the CACHE FILE SYSTEM)

      U_INTERNAL_DUMP("U_http_is_navigation = %b", U_http_is_navigation)

      if (u_http_info.query_len == 0 &&
          U_http_is_navigation  == false)
         {
         uint32_t sz      = file->getPathRelativLen(), len = str_indexhtml->size();
         const char* ptr  = file->getPathRelativ();
         const char* ptr1 = str_indexhtml->data();

         U_INTERNAL_ASSERT_MAJOR(sz,0)

         pathname->setBuffer(sz + 1 + len);

         bool broot = (sz == 1 && ptr[0] == '/');

         if (broot) pathname->snprintf(     "%.*s",          len, ptr1);
         else       pathname->snprintf("%.*s/%.*s", sz, ptr, len, ptr1);

         file_data = (*cache_file)[*pathname];

         if (file_data)
            {
            // NB: we have an index file (index.html) in the directory...

            if (isDataFromCache()) // NB: check if we have the content of the index file in cache...
               {
               u_http_info.nResponseCode = HTTP_OK;

               bool gzip = (U_http_is_accept_gzip &&
                            isDataCompressFromCache());

               if (gzip) U_http_is_accept_gzip = '2';

               if (isHEAD() == false) *UClientImage_Base::body    = getDataFromCache(false, gzip);
                                      *UClientImage_Base::wbuffer = getHeaderForResponse(getDataFromCache(true, gzip), false);

               goto end;
               }

            file->setPath(*pathname);

            file->st_size  = file_data->size;
            file->st_mode  = file_data->mode;
            file->st_mtime = file_data->mtime;

            goto check_file;
            }
         }

      // now we check the directory...

      if (checkGetRequestIfModified())
         {
         // check if it's OK to do directory listing via authentication (digest|basic)

         if (processAuthorization() == false)
            {
            setUnAuthorized();

            return;
            }

         uint32_t sz;

         if (isHEAD()) sz = getHTMLDirectoryList().size();
         else
            {
            *UClientImage_Base::body = getHTMLDirectoryList();

            if (U_http_is_accept_gzip)
               {
               U_http_is_accept_gzip = '2';

               (void) ext.append(U_CONSTANT_TO_PARAM("Content-Encoding: gzip\r\n"));

               *UClientImage_Base::body = UStringExt::deflate(*UClientImage_Base::body, true);
               }

            sz = UClientImage_Base::body->size();
            }

         u_mime_index              = -1;
         u_http_info.nResponseCode = HTTP_OK;

         (void) ext.append(getHeaderMimeType(0, U_CTYPE_HTML, sz, 0));

         *UClientImage_Base::wbuffer = getHeaderForResponse(ext, false); // build response...
         }

      goto end;
      }

check_file: // now we check the file...

   U_INTERNAL_DUMP("file_data = %p", file_data)

   U_INTERNAL_ASSERT_POINTER(file_data)

   if (file_data->fd == -1)
      {
      // NB: '.htpasswd' and '.htdigest' are forbidden...

      result = (file->regular()                                                           &&
                file->isNameDosMatch(U_CONSTANT_TO_PARAM(".htpasswd|.htdigest")) == false &&
                file->open());

      if (result) file_data->fd = file->fd;
      }
   else
      {
      result   = true;
      file->fd = file_data->fd;
      }

   if (result == false)
      {
      setForbidden(); // set forbidden error response...

      return;
      }

   if (file->modified())
      {
      file_data->mode  = file->st_mode;
      file_data->size  = file->st_size;
      file_data->mtime = file->st_mtime;

      U_INTERNAL_DUMP("file_data->fd = %d st_mode = %d st_size = %I st_mtime = %ld", file_data->fd, file->st_mode, file->st_size, file->st_mtime)
      }

   if (checkGetRequestIfModified())
      {
      // NB: check for empty document...

      if (file_data->size) processGetRequest(etag, ext);
      else
         {
         file->close();

         file_data->fd = file->fd = -1;

         *UClientImage_Base::wbuffer = getHeaderForResponse(UString::getStringNull(), false);
         }
      }

end:
#ifdef U_HTTP_CACHE_REQUEST
   manageRequestCache();   
#else
   ; // NB: gcc version 4.4.6 20120305 (Red Hat 4.4.6-4) (GCC) need this...
#endif
}

U_NO_EXPORT void UHTTP::processGetRequest(const UString& etag, UString& ext)
{
   U_TRACE(0, "UHTTP::processGetRequest(%.*S,%.*S)", U_STRING_TO_TRACE(etag), U_STRING_TO_TRACE(ext))

   U_INTERNAL_ASSERT_POINTER(file_data)
   U_INTERNAL_ASSERT_MAJOR(file_data->fd,0)
   U_INTERNAL_ASSERT_MAJOR(file_data->size,0)
   U_INTERNAL_ASSERT_EQUALS(file->fd,file_data->fd)
   U_INTERNAL_ASSERT_EQUALS(file->st_size,file_data->size)

   UString mmap;

   // NB: we check if we need to send the body with sendfile()

   U_INTERNAL_DUMP("bsendfile = %b", bsendfile)

   if (bsendfile)                               goto sendfile;

   if (file->memmap(PROT_READ, &mmap) == false) goto error;

   (void) ext.append(getHeaderMimeType(file->map, file->getMimeType(U_http_is_navigation), file->st_size, 0));

   range_size  = file->st_size;
   range_start = 0;

   u_http_info.nResponseCode = HTTP_OK;

   if (U_http_range_len &&
       checkGetRequestIfRange(etag))
      {
      // The Range: header is used with a GET request.
      // For example assume that will return only a portion (let's say the first 32 bytes) of the requested resource...
      //
      // Range: bytes=0-31

      if (checkGetRequestForRange(ext, mmap) != U_PARTIAL) return; // NB: we have already a complete response...

      u_http_info.nResponseCode = HTTP_PARTIAL;

      goto build_response;
      }

   // ---------------------------------------------------------------------
   // NB: check for Flash pseudo-streaming
   // ---------------------------------------------------------------------
   // Adobe Flash Player can start playing from any part of a FLV movie
   // by sending the HTTP request below ('123' is the bytes offset):
   //
   // GET /movie.flv?start=123
   //
   // HTTP servers that support Flash Player requests must send the binary 
   // FLV Header ("FLV\x1\x1\0\0\0\x9\0\0\0\x9") before the requested data.
   // ---------------------------------------------------------------------

   if (u_is_flv() &&
       U_HTTP_QUERY_STRNEQ("start="))
      {
      U_INTERNAL_ASSERT_DIFFERS(u_http_info.nResponseCode, HTTP_PARTIAL)

      range_start = atol(u_http_info.query + U_CONSTANT_SIZE("start="));

      U_SRV_LOG("Request for flash pseudo-streaming: video = %.*S start = %u", U_FILE_TO_TRACE(*file), range_start);

      if (range_start >= range_size) range_start = 0;
      else
         {
         // build response...

         u_http_info.nResponseCode = HTTP_PARTIAL;

         setResponseForRange(range_start, range_size-1, U_CONSTANT_SIZE(U_FLV_HEAD), ext);

         *UClientImage_Base::wbuffer = getHeaderForResponse(ext, false);

         (void) UClientImage_Base::wbuffer->append(U_CONSTANT_TO_PARAM(U_FLV_HEAD));

         goto next;
         }
      }

build_response:
   *UClientImage_Base::wbuffer = getHeaderForResponse(ext, false);

next:
   U_INTERNAL_DUMP("range_start = %u range_size = %u min_size_for_sendfile = %u", range_start, range_size, min_size_for_sendfile)

   if (isGET())
      {
      U_INTERNAL_ASSERT_EQUALS((bool)*UClientImage_Base::body, false)

      // NB: we check if we need to send the body with sendfile()

      if (range_size >= min_size_for_sendfile)
         {
         bsendfile = true;
sendfile:
         U_INTERNAL_DUMP("UServer_Base::pClientImage->sfd = %d", UServer_Base::pClientImage->sfd)

         U_ASSERT_EQUALS(isHEAD(), false)
         U_INTERNAL_ASSERT(range_size >= min_size_for_sendfile)
         U_INTERNAL_ASSERT_EQUALS(UServer_Base::pClientImage->sfd, 0)

         UServer_Base::pClientImage->sfd    = file->fd;
         UServer_Base::pClientImage->start  = UServer_Base::start  = range_start;
         UServer_Base::pClientImage->count  = UServer_Base::count  = range_size;
         UServer_Base::pClientImage->bclose = UServer_Base::bclose = U_http_is_connection_close;
                                                                     U_http_is_connection_close = U_NOT;

         return;
         }

      if (u_http_info.nResponseCode == HTTP_PARTIAL &&
          file->memmap(PROT_READ, &mmap, range_start, range_size) == false)
         {
error:
         setServiceUnavailable();

         return;
         }

      *UClientImage_Base::body = mmap;

      // NB: manage for possibly partial write in response...

      U_INTERNAL_ASSERT_EQUALS(bsendfile, false)

#  ifdef U_CLIENT_RESPONSE_PARTIAL_WRITE_SUPPORT
      if (
#        ifdef USE_LIBSSL
          UServer_Base::bssl == false &&
#        endif
          (range_size - range_start) > U_MIN_SIZE_FOR_PARTIAL_WRITE)
         {
         UServer_Base::pClientImage->sfd   = file->fd;
         UServer_Base::pClientImage->start = range_start;
         }
#  endif
      }
}

#ifdef U_HTTP_UPLOAD_PROGRESS_SUPPORT
// ------------------------------------------------------------------------------------------------------------------------------
// UPLOAD PROGRESS
// ------------------------------------------------------------------------------------------------------------------------------
// The basic technique is simple: on the page containing your upload form is some JavaScript to generate a unique identifier
// for the upload request. When the form is submitted, that identifier is passed along with the rest of the data. When the
// server starts receiving the POST request, it starts logging the bytes received for the identifier. While the file's being
// uploaded, the JavaScript on the upload page makes periodic requests asking for the upload progress, and updates a progress
// widget accordingly. On the server, you need two views: one to handle the upload form, and one to respond to progress requests.
// ------------------------------------------------------------------------------------------------------------------------------
U_NO_EXPORT bool UHTTP::initUploadProgress(int byte_read)
{
   U_TRACE(0, "UHTTP::initUploadProgress(%d)", byte_read)

   U_ASSERT(UServer_Base::isPreForked())
   U_INTERNAL_ASSERT(*UClientImage_Base::request)

   int i;
   in_addr_t client     = UServer_Base::pClientImage->socket->remoteIPAddress().getInAddr();
   upload_progress* ptr = ptr_upload_progress;

   U_INTERNAL_DUMP("ptr = %p", ptr)

   U_INTERNAL_ASSERT_POINTER(ptr)

   if (byte_read > 0)
      {
      int uuid_key_size = USocket::str_X_Progress_ID->size() + 1;

      // find first available slot

      for (i = 0; i < U_MAX_UPLOAD_PROGRESS; ++i)
         {
         U_INTERNAL_DUMP("i = %2d uuid = %.32S byte_read = %d count = %d",
                          i, ptr[i].uuid, ptr[i].byte_read, ptr[i].count)

         if (ptr[i].byte_read ==
             ptr[i].count)
            {
            ptr += i;

            ptr->count      = byte_read;
            ptr->client     = client;
            ptr->user_agent = getUserAgent();
            ptr->byte_read  = 0;

            // The HTTP POST request can contain a query parameter, 'X-Progress-ID', which should contain a
            // unique string to identify the upload to be tracked.

            if (u_http_info.query_len ==                  32U ||
                u_http_info.query_len == (uuid_key_size + 32U))
               {
               U_INTERNAL_DUMP("query(%u) = %.*S", u_http_info.query_len, U_HTTP_QUERY_TO_TRACE)

               U__MEMCPY(ptr->uuid, u_http_info.query + (u_http_info.query_len == 32 ? 0 : uuid_key_size), 32);

               U_INTERNAL_DUMP("uuid = %.32S", ptr->uuid)
               }

            break;
            }
         }
      }
   else
      {
      // find current slot: The HTTP request to this location must have either an X-Progress-ID parameter or
      // X-Progress-ID HTTP header containing the unique identifier as specified in your upload/POST request
      // to the relevant tracked zone. If you are using the X-Progress-ID as a query-string parameter, ensure
      // it is the LAST argument in the URL.

      uint32_t n = form_name_value->size();

      const char* uuid_ptr = (n >= 2 && form_name_value->isEqual(n-2, *USocket::str_X_Progress_ID, true)
                                 ? form_name_value->c_pointer(n-1)
                                 : getHeaderValuePtr(*USocket::str_X_Progress_ID, true));

      U_INTERNAL_DUMP("uuid = %.32S", uuid_ptr)

      for (i = 0; i < U_MAX_UPLOAD_PROGRESS; ++i)
         {
         U_INTERNAL_DUMP("i = %2d uuid = %.32S byte_read = %d count = %d",
                          i, ptr[i].uuid, ptr[i].byte_read, ptr[i].count)

         if ((uuid_ptr ? memcmp(uuid_ptr, ptr[i].uuid, 32)  == 0
                       :                  ptr[i].client     == client &&
                                          ptr[i].user_agent == getUserAgent()))
            {
            ptr += i;

            break;
            }
         }
      }

   upload_progress_index = i;

   U_RETURN(upload_progress_index < U_MAX_UPLOAD_PROGRESS);
}

U_NO_EXPORT void UHTTP::updateUploadProgress(int byte_read)
{
   U_TRACE(0, "UHTTP::updateUploadProgress(%d)", byte_read)

   U_INTERNAL_ASSERT_MAJOR(byte_read,0)

   U_ASSERT(UServer_Base::isPreForked())

   if (upload_progress_index < U_MAX_UPLOAD_PROGRESS)
      {
      upload_progress* ptr = ptr_upload_progress + upload_progress_index;

      U_INTERNAL_DUMP("ptr = %p", ptr)

      U_INTERNAL_ASSERT_POINTER(ptr)

      U_INTERNAL_DUMP("byte_read = %d count = %d", ptr->byte_read, ptr->count)

      U_INTERNAL_ASSERT_MAJOR(ptr->count,0)

      ptr->byte_read = byte_read;
      }
}
#endif

// Return JSON object with information about the progress of an upload.
// The JSON returned is what the JavaScript uses to render the progress bar...

UString UHTTP::getUploadProgress()
{
   U_TRACE(0, "UHTTP::getUploadProgress()")

   U_ASSERT(UServer_Base::isPreForked())

   UString result(100U);

#ifdef U_HTTP_UPLOAD_PROGRESS_SUPPORT
   if (upload_progress_index >= U_MAX_UPLOAD_PROGRESS)
      {
      UTimeVal to_sleep(0L, 300 * 1000L);

      for (int i = 0; i < 10 && initUploadProgress(0) == false; ++i) to_sleep.nanosleep();
      }

   if (upload_progress_index >= U_MAX_UPLOAD_PROGRESS)
#endif

   (void) result.assign(U_CONSTANT_TO_PARAM("{ 'state' : 'error' }"));

#ifdef U_HTTP_UPLOAD_PROGRESS_SUPPORT
   else
      {
      upload_progress* ptr = ptr_upload_progress + upload_progress_index;

      U_INTERNAL_DUMP("ptr = %p", ptr)

      U_INTERNAL_ASSERT_POINTER(ptr)

      U_INTERNAL_DUMP("byte_read = %d count = %d", ptr->byte_read, ptr->count)

      U_INTERNAL_ASSERT_MAJOR(ptr->count,0)

      result.snprintf("{ 'state' : '%s', 'received' : %d, 'size' : %d }",
                              (ptr->byte_read < ptr->count ? "uploading" : "done"),
                               ptr->byte_read,
                               ptr->count);
      }
#endif

   U_RETURN_STRING(result);
}

// STREAM

U_EXPORT istream& operator>>(istream& is, UHTTP::UFileCacheData& d)
{
   U_TRACE(0, "UFileCacheData::operator>>(%p,%p)", &is, &d)

   d.array = 0;

   if (is.good())
      {
      streambuf* sb = is.rdbuf();

      int c = sb->sbumpc(); // skip '{'

      do { c = sb->sbumpc(); } while (u__isspace(c) && c != EOF); // skip white-space

           if (c == '}') is.setstate(ios::badbit);
      else if (c == EOF) is.setstate(ios::eofbit);
      else
         {
         sb->sputbackc(c);

         is >> d.mime_index // index file mime type
            >> d.size       // size content
            >> d.mtime      // time of last modification
            >> d.expire;    // expire time of the entry

         U_INTERNAL_DUMP("d.mime_index = %C", d.mime_index)

         do { c = sb->sbumpc(); } while (u__isspace(c) && c != EOF); // skip white-space

         if (c         == '(' &&
             is.peek() != ')')
            {
            sb->sputbackc(c);

            UVector<UString> vec(4U);

            is >> vec;

            // content, header, gzip(content, header)

            if (vec.empty() == false)
               {
               U_NEW_DBG(UVector<UString>, d.array, UVector<UString>(4U));

               UString encoded, decoded;

               // content

               encoded = vec[0],
               decoded.setBuffer(encoded.size());

               if (d.mime_index == U_png ||
                   d.mime_index == U_gif ||
                   d.mime_index == U_jpg ||
                   d.mime_index == U_ico)
                  {
                  (void) UEscape::decode(encoded, decoded);
                  }
               else
                  {
                  (void) UBase64::decode(encoded, decoded);
                  }

               d.array->push_back(decoded);

               // header

               encoded = vec[1];
               decoded.setBuffer(encoded.size());

               (void) UEscape::decode(encoded, decoded);

               d.array->push_back(decoded);

               encoded = vec[2];

               if (encoded.empty() == false)
                  {
                  // gzip(content)

                  decoded.setBuffer(encoded.size());

                  (void) UBase64::decode(encoded, decoded);

                  d.array->push_back(decoded);

                  // gzip(header)

                  encoded = vec[3];
                  decoded.setBuffer(encoded.size());

                  (void) UEscape::decode(encoded, decoded);

                  d.array->push_back(decoded);
                  }
               }
            }

         // skip '}'

         do { c = sb->sbumpc(); } while (c != '}' && c != EOF);

         if (c == EOF) is.setstate(ios::eofbit);
         }
      }

   return is;
}

U_EXPORT ostream& operator<<(ostream& os, const UHTTP::UFileCacheData& d)
{
   U_TRACE(0, "UFileCacheData::operator<<(%p,%p)", &os, &d)

   os.put('{');

   if (d.ptr  == 0     &&
       d.link == false &&
       S_ISDIR(d.mode) == 0)
      {
      os.put(' ');
      os << d.mime_index; // index file mime type
      os.put(' ');
      os << d.size;       // size content
      os.put(' ');
      os << d.mtime;      // time of last modification
      os.put(' ');
      os << d.expire;     // expire time of the entry
      os.put(' ');
      os.put('(');

      if (d.array && // content, header, gzip(content, header)
          d.size < (64 * 1024))
         {
         U_INTERNAL_ASSERT_EQUALS(d.ptr, 0)

         UString str;
         uint32_t pos;
         char buffer[100 * 1024];

         str = d.array->at(0); // content

         pos = (d.mime_index == U_png ||
                d.mime_index == U_gif ||
                d.mime_index == U_jpg ||
                d.mime_index == U_ico
                     ? u_base64_encode((const unsigned char*)U_STRING_TO_PARAM(str), (unsigned char*)buffer)
                     : u_escape_encode((const unsigned char*)U_STRING_TO_PARAM(str),                 buffer, sizeof(buffer), false));

         os.put('\n');
         os.write(buffer, pos);
         os.put('\n');

         str = d.array->at(1); // header

         pos = u_escape_encode((const unsigned char*)U_STRING_TO_PARAM(str), buffer, sizeof(buffer), false);

         os.put('\n');
         os.write(buffer, pos);
         os.put('\n');

         if (d.array->size() == 2) os.write(U_CONSTANT_TO_PARAM("\n\"\"\n\"\"\n"));
         else
            {
            str = d.array->at(2); // gzip(content)

            pos = u_base64_encode((const unsigned char*)U_STRING_TO_PARAM(str), (unsigned char*)buffer);

            os.put('\n');
            os.write(buffer, pos);
            os.put('\n');

            str = d.array->at(3); // gzip(header)

            pos = u_escape_encode((const unsigned char*)U_STRING_TO_PARAM(str), buffer, sizeof(buffer), false);

            os.put('\n');
            os.write(buffer, pos);
            os.put('\n');
            }
         }

      os.put(')');
      }

   os.put(' ');
   os.put('}');
   os.put('\n');

   return os;
}

// DEBUG

#ifdef DEBUG
#  include <ulib/internal/objectIO.h>

U_EXPORT const char* UHTTP::UServletPage::dump(bool reset) const
{
   UDynamic::dump(false);

   *UObjectIO::os << '\n'
                  << "runDynamicPage" << (void*)runDynamicPage << '\n';

   if (reset)
      {
      UObjectIO::output();

      return UObjectIO::buffer_output;
      }

   return 0;
}

U_EXPORT const char* UHTTP::UCServletPage::dump(bool reset) const
{
   *UObjectIO::os << "size      " << size             << '\n'
                  << "relocated " << (void*)relocated << '\n'
                  << "prog_main " << (void*)prog_main << '\n';

   if (reset)
      {
      UObjectIO::output();

      return UObjectIO::buffer_output;
      }

   return 0;
}

#  ifdef USE_PAGE_SPEED
U_EXPORT const char* UHTTP::UPageSpeed::dump(bool reset) const
{
   UDynamic::dump(false);

   *UObjectIO::os << '\n'
                  << "minify_html   " << (void*)minify_html  << '\n'
                  << "optimize_gif  " << (void*)optimize_gif << '\n'
                  << "optimize_png  " << (void*)optimize_png << '\n'
                  << "optimize_jpg  " << (void*)optimize_jpg << '\n';

   if (reset)
      {
      UObjectIO::output();

      return UObjectIO::buffer_output;
      }

   return 0;
}
#  endif

#  ifdef USE_LIBV8
U_EXPORT const char* UHTTP::UV8JavaScript::dump(bool reset) const
{
   UDynamic::dump(false);

   *UObjectIO::os << '\n'
                  << "runv8         " << (void*)runv8  << '\n';

   if (reset)
      {
      UObjectIO::output();

      return UObjectIO::buffer_output;
      }

   return 0;
}
#  endif

U_EXPORT const char* UHTTP::RewriteRule::dump(bool reset) const
{
   *UObjectIO::os
#              ifdef USE_LIBPCRE
                  << "key         (UPCRE   " << (void*)&key         << ")\n"
#              endif
                  << "replacement (UString " << (void*)&replacement << ')';

   if (reset)
      {
      UObjectIO::output();

      return UObjectIO::buffer_output;
      }

   return 0;
}

U_EXPORT const char* UHTTP::UFileCacheData::dump(bool reset) const
{
   *UObjectIO::os << "wd                      " << wd            << '\n'
                  << "size                    " << size          << '\n'
                  << "mode                    " << mode          << '\n'
                  << "expire                  " << expire        << '\n'
                  << "mtime                   " << mtime         << '\n'
                  << "array (UVector<UString> " << (void*)&array << ')';

   if (reset)
      {
      UObjectIO::output();

      return UObjectIO::buffer_output;
      }

   return 0;
}
#endif
