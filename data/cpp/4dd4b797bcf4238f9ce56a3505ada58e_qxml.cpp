/****************************************************************************
** 
**
** Implementation of QXmlSimpleReader and related classes.
**
** Created : 000518
**
** Copyright (C) 1992-2000 Trolltech AS.  All rights reserved.
**
** This file is part of the XML module of the Qt GUI Toolkit.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
** This file may be distributed and/or modified under the terms of the
** GNU General Public License version 2 as published by the Free Software
** Foundation and appearing in the file LICENSE.GPL included in the
** packaging of this file.
**
** Licensees holding valid Qt Enterprise Edition licenses may use this
** file in accordance with the Qt Commercial License Agreement provided
** with the Software.
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
** See http://www.trolltech.com/pricing.html or email sales@trolltech.com for
**   information about Qt Commercial License Agreements.
** See http://www.trolltech.com/qpl/ for QPL licensing information.
** See http://www.trolltech.com/gpl/ for GPL licensing information.
**
** Contact info@trolltech.com if any conditions of this licensing are
** not clear to you.
**
**********************************************************************/

#define QT_XML_CPP
#include "qxml.h"
#include "qtextcodec.h"
#include "qbuffer.h"

#ifndef QT_NO_XML
// NOT REVISED

// Error strings for the XML reader
#define XMLERR_OK                         "no error occured"
#define XMLERR_TAGMISMATCH                "tag mismatch"
#define XMLERR_UNEXPECTEDEOF              "unexpected end of file"
#define XMLERR_FINISHEDPARSINGWHILENOTEOF "parsing is finished but end of file is not reached"
#define XMLERR_LETTEREXPECTED             "letter is expected"
#define XMLERR_ERRORPARSINGELEMENT        "error while parsing element"
#define XMLERR_ERRORPARSINGPROLOG         "error while parsing prolog"
#define XMLERR_ERRORPARSINGMAINELEMENT    "error while parsing main element"
#define XMLERR_ERRORPARSINGCONTENT        "error while parsing content"
#define XMLERR_ERRORPARSINGNAME           "error while parsing name"
#define XMLERR_ERRORPARSINGNMTOKEN        "error while parsing Nmtoken"
#define XMLERR_ERRORPARSINGATTRIBUTE      "error while parsing attribute"
#define XMLERR_ERRORPARSINGMISC           "error while parsing misc"
#define XMLERR_ERRORPARSINGCHOICE         "error while parsing choice or seq"
#define XMLERR_ERRORBYCONSUMER            "error triggered by consumer"
#define XMLERR_UNEXPECTEDCHARACTER        "unexpected character"
#define XMLERR_EQUALSIGNEXPECTED          "expected '=' but not found"
#define XMLERR_QUOTATIONEXPECTED          "expected \" or ' but not found"
#define XMLERR_ERRORPARSINGREFERENCE      "error while parsing reference"
#define XMLERR_ERRORPARSINGPI             "error while parsing processing instruction"
#define XMLERR_ERRORPARSINGATTLISTDECL    "error while parsing attribute list declaration"
#define XMLERR_ERRORPARSINGATTTYPE        "error while parsing attribute type declaration"
#define XMLERR_ERRORPARSINGATTVALUE       "error while parsing attribute value declaration"
#define XMLERR_ERRORPARSINGELEMENTDECL    "error while parsing element declaration"
#define XMLERR_ERRORPARSINGENTITYDECL     "error while parsing entity declaration"
#define XMLERR_ERRORPARSINGNOTATIONDECL   "error while parsing notation declaration"
#define XMLERR_ERRORPARSINGEXTERNALID     "error while parsing external id"
#define XMLERR_ERRORPARSINGCOMMENT        "error while parsing comment"
#define XMLERR_ERRORPARSINGENTITYVALUE    "error while parsing entity value declaration"
#define XMLERR_CDSECTHEADEREXPECTED       "expected the header for a cdata section"
#define XMLERR_MORETHANONEDOCTYPE         "more than one document type definition"
#define XMLERR_ERRORPARSINGDOCTYPE        "error while parsing document type definition"
#define XMLERR_INVALIDNAMEFORPI           "invalid name for processing instruction"
#define XMLERR_VERSIONEXPECTED            "version expected while reading the XML declaration"
#define XMLERR_EDECLORSDDECLEXPECTED      "EDecl or SDDecl expected while reading the XML declaration"
#define XMLERR_SDDECLEXPECTED             "SDDecl expected while reading the XML declaration"
#define XMLERR_WRONGVALUEFORSDECL         "wrong value for standalone declaration"
#define XMLERR_UNPARSEDENTITYREFERENCE    "unparsed entity reference in wrong context"
#define XMLERR_INTERNALGENERALENTITYINDTD "internal general entity reference not allowed in DTD"
#define XMLERR_EXTERNALGENERALENTITYINDTD "external parsed general entity reference not allowed in DTD"
#define XMLERR_EXTERNALGENERALENTITYINAV  "external parsed general entity reference not allowed in attribute value"


// the constants for the lookup table
static const signed char cltWS      =  0; // white space
static const signed char cltPer     =  1; // %
static const signed char cltAmp     =  2; // &
static const signed char cltGt      =  3; // >
static const signed char cltLt      =  4; // <
static const signed char cltSlash   =  5; // /
static const signed char cltQm      =  6; // ?
static const signed char cltEm      =  7; // !
static const signed char cltDash    =  8; // -
static const signed char cltCB      =  9; // ]
static const signed char cltOB      = 10; // [
static const signed char cltEq      = 11; // =
static const signed char cltDq      = 12; // "
static const signed char cltSq      = 13; // '
static const signed char cltUnknown = 14;

// character lookup table
static const signed char charLookupTable[256]={
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0x00 - 0x07
    cltUnknown, // 0x08
    cltWS,      // 0x09 \t
    cltWS,      // 0x0A \n
    cltUnknown, // 0x0B
    cltUnknown, // 0x0C
    cltWS,      // 0x0D \r
    cltUnknown, // 0x0E
    cltUnknown, // 0x0F
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0x17 - 0x16
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0x18 - 0x1F
    cltWS,      // 0x20 Space
    cltEm,      // 0x21 !
    cltDq,      // 0x22 "
    cltUnknown, // 0x23
    cltUnknown, // 0x24
    cltPer,     // 0x25 %
    cltAmp,     // 0x26 &
    cltSq,      // 0x27 '
    cltUnknown, // 0x28
    cltUnknown, // 0x29
    cltUnknown, // 0x2A
    cltUnknown, // 0x2B
    cltUnknown, // 0x2C
    cltDash,    // 0x2D -
    cltUnknown, // 0x2E
    cltSlash,   // 0x2F /
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0x30 - 0x37
    cltUnknown, // 0x38
    cltUnknown, // 0x39
    cltUnknown, // 0x3A
    cltUnknown, // 0x3B
    cltLt,      // 0x3C <
    cltEq,      // 0x3D =
    cltGt,      // 0x3E >
    cltQm,      // 0x3F ?
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0x40 - 0x47
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0x48 - 0x4F
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0x50 - 0x57
    cltUnknown, // 0x58
    cltUnknown, // 0x59
    cltUnknown, // 0x5A
    cltOB,      // 0x5B [
    cltUnknown, // 0x5C
    cltCB,      // 0x5D ]
    cltUnknown, // 0x5E
    cltUnknown, // 0x5F
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0x60 - 0x67
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0x68 - 0x6F
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0x70 - 0x77
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0x78 - 0x7F
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0x80 - 0x87
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0x88 - 0x8F
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0x90 - 0x97
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0x98 - 0x9F
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0xA0 - 0xA7
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0xA8 - 0xAF
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0xB0 - 0xB7
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0xB8 - 0xBF
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0xC0 - 0xC7
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0xC8 - 0xCF
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0xD0 - 0xD7
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0xD8 - 0xDF
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0xE0 - 0xE7
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0xE8 - 0xEF
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, // 0xF0 - 0xF7
    cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown, cltUnknown  // 0xF8 - 0xFF
};


class QXmlNamespaceSupportPrivate
{
};
class QXmlAttributesPrivate
{
};
class QXmlInputSourcePrivate
{
};
class QXmlParseExceptionPrivate
{
};
class QXmlLocatorPrivate
{
};
class QXmlDefaultHandlerPrivate
{
};

#if defined(Q_FULL_TEMPLATE_INSTANTIATION)
bool operator==( const QMap<QString, QString>, const QMap<QString, QString> )
{
    return FALSE;
}
#endif

/*!
  \class QXmlParseException qxml.h
  \brief The QXmlParseException class is used to report errors with the
  QXmlErrorHandler interface.

  \module XML

  \sa QXmlErrorHandler
*/
/*!
  \fn QXmlParseException::QXmlParseException( const QString& name, int c, int l, const QString& p, const QString& s )

  Constructs a parse exception with the error string \a name in the column
  \a c and line \a l for the public identifier \a p and the system identifier
  \a s.
*/
/*!
  Returns the error message.
*/
QString QXmlParseException::message() const
{
    return msg;
}
/*!
  Returns the column number the error occured.
*/
int QXmlParseException::columnNumber() const
{
    return column;
}
/*!
  Returns the line number the error occured.
*/
int QXmlParseException::lineNumber() const
{
    return line;
}
/*!
  Returns the public identifier the error occured.
*/
QString QXmlParseException::publicId() const
{
    return pub;
}
/*!
  Returns the system identifier the error occured.
*/
QString QXmlParseException::systemId() const
{
    return sys;
}


/*!
  \class QXmlLocator qxml.h
  \brief The QXmlLocator class provides the XML handler classes with
  information about the actual parsing position.

  \module XML

  The reader reports a QXmlLocator to the content handler before he starts to
  parse the document. This is done with the
  QXmlContentHandler::setDocumentLocator() function. The handler classes can
  now use this locator to get the actual position the reader is at.
*/
/*!
    \fn QXmlLocator::QXmlLocator( QXmlSimpleReader* parent )

    Constructor.
*/
/*!
    \fn QXmlLocator::~QXmlLocator()

    Destructor.
*/
/*!
    Gets the column number (starting with 1) or -1 if there is no column number
    available.
*/
int QXmlLocator::columnNumber()
{
    return ( reader->columnNr == -1 ? -1 : reader->columnNr + 1 );
}
/*!
    Gets the line number (starting with 1) or -1 if there is no line number
    available.
*/
int QXmlLocator::lineNumber()
{
    return ( reader->lineNr == -1 ? -1 : reader->lineNr + 1 );
}


/*********************************************
 *
 * QXmlNamespaceSupport
 *
 *********************************************/

/*!
  \class QXmlNamespaceSupport qxml.h
  \brief The QXmlNamespaceSupport class is a helper class for XML readers which
  want to include namespace support.

  \module XML

  It provides some functions that makes it easy to handle namespaces. Its main
  use is for subclasses of QXmlReader which want to provide namespace
  support.

  See also the <a href="xml-sax.html#namespaces">namespace description</a>.
*/

/*!
  Constructs a QXmlNamespaceSupport.
*/
QXmlNamespaceSupport::QXmlNamespaceSupport()
{
    reset();
}

/*!
  Destructs a QXmlNamespaceSupport.
*/
QXmlNamespaceSupport::~QXmlNamespaceSupport()
{
}

/*!
  This function declares a prefix in the current namespace context; the prefix
  will remain in force until this context is popped, unless it is shadowed in a
  descendant context.

  Note that there is an asymmetry in this library: while prefix() will not
  return the default "" prefix, even if you have declared one; to check for a
  default prefix, you have to look it up explicitly using uri(). This
  asymmetry exists to make it easier to look up prefixes for attribute names,
  where the default prefix is not allowed.
*/
void QXmlNamespaceSupport::setPrefix( const QString& pre, const QString& uri )
{
    if( pre.isNull() ) {
	ns.insert( "", uri );
    } else {
	ns.insert( pre, uri );
    }
}

/*!
  Returns one of the prefixes mapped to a namespace URI.

  If more than one prefix is currently mapped to the same URI, this function
  will make an arbitrary selection; if you want all of the prefixes, use the
  prefixes() function instead.

  Note: this will never return the empty (default) prefix; to check for a
  default prefix, use the uri() function with an argument of "".
*/
QString QXmlNamespaceSupport::prefix( const QString& uri ) const
{
    QMap<QString, QString>::ConstIterator itc, it = ns.begin();
    while ( (itc=it) != ns.end() ) {
	++it;
	if ( itc.data() == uri && !itc.key().isEmpty() )
	    return itc.key();
    }
    return "";
}

/*!
  Looks up a prefix in the current context and returns the currently-mapped
  namespace URI. Use the empty string ("") for the default namespace.
*/
QString QXmlNamespaceSupport::uri( const QString& prefix ) const
{
    const QString& returi = ns[ prefix ];
    return returi;
}

/*!
  Splits the name at the ':' and returns the prefix and the local name.
*/
void QXmlNamespaceSupport::splitName( const QString& qname,
	QString& prefix, QString& localname ) const
{
    uint pos;
    // search the ':'
    for( pos=0; pos<qname.length(); pos++ ) {
	if ( qname.at(pos) == ':' )
	    break;
    }
    // and split
    prefix = qname.left( pos );
    localname = qname.mid( pos+1 );
}

/*!
  Processes a raw XML 1.0 name in the current context by removing the prefix
  and looking it up among the prefixes currently declared.

  First parameter is the raw XML 1.0 name to be processed. The second parameter
  is a flag wheter the name is the name of an attribute (TRUE) or not (FALSE).

  The return values will be stored in the last two parameters as follows:
  <ul>
  <li> The namespace URI, or an empty string if none is in use.
  <li> The local name (without prefix).
  </ul>

  If the raw name has a prefix that has not been declared, then the return
  value will be empty.

  Note that attribute names are processed differently than element names: an
  unprefixed element name will received the default namespace (if any), while
  an unprefixed element name will not
*/
void QXmlNamespaceSupport::processName( const QString& qname,
	bool isAttribute,
	QString& nsuri, QString& localname ) const
{
    uint pos;
    // search the ':'
    for( pos=0; pos<qname.length(); pos++ ) {
	if ( qname.at(pos) == ':' )
	    break;
    }
    if ( pos < qname.length() ) {
	// there was a ':'
	nsuri = uri( qname.left( pos ) );
	localname = qname.mid( pos+1 );
    } else {
	// there was no ':'
	if ( isAttribute ) {
	    nsuri = ""; // attributes don't take default namespace
	} else {
	    nsuri = uri( "" ); // get default namespace
	}
	localname = qname;
    }
}

/*!
  Returns an enumeration of all prefixes currently declared.

  Note: if there is a default prefix, it will not be returned in this
  enumeration; check for the default prefix using uri() with an argument
  of "".
*/
QStringList QXmlNamespaceSupport::prefixes() const
{
    QStringList list;

    QMap<QString, QString>::ConstIterator itc, it = ns.begin();
    while ( (itc=it) != ns.end() ) {
	++it;
	if ( !itc.key().isEmpty() )
	    list.append( itc.key() );
    }
    return list;
}

/*!
  Returns a list of all prefixes currently declared for a URI.

  The xml: prefix will be included. If you want only one prefix that's
  mapped to the namespace URI, and you don't care which one you get, use the
  prefix() function instead.

  Note: the empty (default) prefix is never included in this enumeration; to
  check for the presence of a default namespace, use uri() with an
  argument of "".
*/
QStringList QXmlNamespaceSupport::prefixes( const QString& uri ) const
{
    QStringList list;

    QMap<QString, QString>::ConstIterator itc, it = ns.begin();
    while ( (itc=it) != ns.end() ) {
	++it;
	if ( itc.data() == uri && !itc.key().isEmpty() )
	    list.append( itc.key() );
    }
    return list;
}

/*!
  Starts a new namespace context.

  Normally, you should push a new context at the beginning of each XML element:
  the new context will automatically inherit the declarations of its parent
  context, but it will also keep track of which declarations were made within
  this context.
*/
void QXmlNamespaceSupport::pushContext()
{
    nsStack.push( ns );
}

/*!
  Reverts to the previous namespace context.

  Normally, you should pop the context at the end of each XML element.  After
  popping the context, all namespace prefix mappings that were previously in
  force are restored.
*/
void QXmlNamespaceSupport::popContext()
{
    if( !nsStack.isEmpty() )
	ns = nsStack.pop();
}

/*!
  Resets this namespace support object for reuse.
*/
void QXmlNamespaceSupport::reset()
{
    nsStack.clear();
    ns.clear();
    ns.insert( "xml", "http://www.w3.org/XML/1998/namespace" ); // the XML namespace
}



/*********************************************
 *
 * QXmlAttributes
 *
 *********************************************/

/*!
  \class QXmlAttributes qxml.h
  \brief The QXmlAttributes class provides XML attributes.

  \module XML

  If attributes are reported by QXmlContentHandler::startElement() this
  class is used to pass the attribute values. It provides you with different
  functions to access the attribute names and values.
*/
/*!
  \fn QXmlAttributes::QXmlAttributes()

  Constructs an empty attribute list.
*/
/*!
  \fn QXmlAttributes::~QXmlAttributes()

  Destructs attributes.
*/

/*!
  Look up the index of an attribute by an XML 1.0 qualified name.

  Returns the index of the attribute (starting with 0) or -1 if it wasn't
  found.

  See also the <a href="xml-sax.html#namespaces">namespace description</a>.
*/
int QXmlAttributes::index( const QString& qName ) const
{
    return qnameList.findIndex( qName );
}

/*!
  Looks up the index of an attribute by a namespace name.

  \a uri specifies the namespace URI, or the empty string if the name has no
  namespace URI. \a localPart specifies the attribute's local name.

  Returns the index of the attribute (starting with 0) or -1 if it wasn't
  found.

  See also the <a href="xml-sax.html#namespaces">namespace description</a>.
*/
int QXmlAttributes::index( const QString& uri, const QString& localPart ) const
{
    uint count = uriList.count();
    for ( uint i=0; i<count; i++ ) {
	if ( uriList[i] == uri && localnameList[i] == localPart )
	    return i;
    }
    return -1;
}

/*!
  Returns the number of attributes in the list.
*/
int QXmlAttributes::length() const
{
    return valueList.count();
}

/*!
  Looks up an attribute's local name by index (starting with 0).

  See also the <a href="xml-sax.html#namespaces">namespace description</a>.
*/
QString QXmlAttributes::localName( int index ) const
{
    return localnameList[index];
}

/*!
  Looks up an attribute's XML 1.0 qualified name by index (starting with 0).

  See also the <a href="xml-sax.html#namespaces">namespace description</a>.
*/
QString QXmlAttributes::qName( int index ) const
{
    return qnameList[index];
}

/*!
  Looks up an attribute's namespace URI by index (starting with 0).

  See also the <a href="xml-sax.html#namespaces">namespace description</a>.
*/
QString QXmlAttributes::uri( int index ) const
{
    return uriList[index];
}

/*!
  Looks up an attribute's type by index (starting with 0).

  At the moment only 'CDATA' is returned.
*/
QString QXmlAttributes::type( int ) const
{
    return "CDATA";
}

/*!
  Looks up an attribute's type by XML 1.0 qualified name.

  At the moment only 'CDATA' is returned.
*/
QString QXmlAttributes::type( const QString& ) const
{
    return "CDATA";
}

/*!
  Looks up an attribute's type by namespace name.

  The first parameter specifies the namespace URI, or the empty string if
  the name has no namespace URI. The second parameter specifies the
  attribute's local name.

  At the moment only 'CDATA' is returned.
*/
QString QXmlAttributes::type( const QString&, const QString& ) const
{
    return "CDATA";
}

/*!
  Looks up an attribute's value by index (starting with 0).
*/
QString QXmlAttributes::value( int index ) const
{
    return valueList[index];
}

/*!
  Looks up an attribute's value by XML 1.0 qualified name.

  See also the <a href="xml-sax.html#namespaces">namespace description</a>.
*/
QString QXmlAttributes::value( const QString& qName ) const
{
    int i = index( qName );
    if ( i == -1 )
	return QString::null;
    return valueList[ i ];
}

/*!
  Looks up an attribute's value by namespace name.

  \a uri specifies the namespace URI, or the empty string if the name has no
  namespace URI. \a localName specifies the attribute's local name.

  See also the <a href="xml-sax.html#namespaces">namespace description</a>.
*/
QString QXmlAttributes::value( const QString& uri, const QString& localName ) const
{
    int i = index( uri, localName );
    if ( i == -1 )
	return QString::null;
    return valueList[ i ];
}


/*********************************************
 *
 * QXmlInputSource
 *
 *********************************************/

/*!
  \class QXmlInputSource qxml.h
  \brief The QXmlInputSource class is the source where XML data is read from.

  \module XML

  All subclasses of QXmlReader read the input from this class.
*/

/*!
  Returns all the data this input source contains.
*/
const QString& QXmlInputSource::data() const
{
    return input;
}

/*!
  Constructs a input source which contains no data.
*/
QXmlInputSource::QXmlInputSource( )
{
    input = "";
}

/*!
  Constructs a input source and get the data from the text stream.
*/
QXmlInputSource::QXmlInputSource( QTextStream& stream )
{
    QByteArray rawData;
    if ( stream.device()->isDirectAccess() ) {
	rawData = stream.device()->readAll();
    } else {
	int nread = 0;
	const int bufsize = 512;
	while ( !stream.device()->atEnd() ) {
	    rawData.resize( nread + bufsize );
	    nread += stream.device()->readBlock( rawData.data()+nread, bufsize );
	}
	rawData.resize( nread );
    }
    readInput( rawData );
}

/*!
  Constructs a input source and get the data from a file. If the file cannot be
  read the input source is empty.
*/
QXmlInputSource::QXmlInputSource( QFile& file )
{
    if ( !file.open(IO_ReadOnly) ) {
	input = "";
	return;
    }
    QByteArray rawData = file.readAll();
    readInput( rawData );
    file.close();
}

/*!
  Destructor.
*/
QXmlInputSource::~QXmlInputSource()
{
}

/*!
  Sets the data of the input source to \a dat.
*/
void QXmlInputSource::setData( const QString& dat )
{
    input = dat;
}

/*!
  Read the XML file from the byte array; try to recoginize the encoding.
*/
// ### The input source should not do the encoding detection!
void QXmlInputSource::readInput( QByteArray& rawData )
{
    QBuffer buf( rawData );
    buf.open( IO_ReadOnly );
    QTextStream *stream = new QTextStream( &buf );
    QChar tmp;
    // assume UTF8 or UTF16 at first
    stream->setEncoding( QTextStream::UnicodeUTF8 );
    input = "";
    // read the first 5 characters
    for ( int i=0; i<5; i++ ) {
	*stream >> tmp;
	input += tmp;
    }
    // starts the document with an XML declaration?
    if ( input == "<?xml" ) {
	// read the whole XML declaration
	do {
	    *stream >> tmp;
	    input += tmp;
	} while( tmp != '>' );
	// and try to find out if there is an encoding
	int pos = input.find( "encoding" );
	if ( pos != -1 ) {
	    QString encoding;
	    do {
		pos++;
		if ( pos > (int)input.length() )
		    goto finished;
	    } while( input[pos] != '"' && input[pos] != '\'' );
	    pos++;
	    while( input[pos] != '"' && input[pos] != '\'' ) {
		encoding += input[pos];
		pos++;
		if ( pos > (int)input.length() )
		    goto finished;
	    }
	    delete stream;
	    stream = new QTextStream( &buf );
	    stream->setCodec( QTextCodec::codecForName( encoding ) );
	    buf.reset();
	    input = "";
	}
    }
finished:
    input += stream->read();
    delete stream;
    buf.close();
}


/*********************************************
 *
 * QXmlDefaultHandler
 *
 *********************************************/

/*!
  \class QXmlContentHandler qxml.h
  \brief The QXmlContentHandler class provides an interface to report logical
  content of XML data.

  \module XML

  If the application needs to be informed of basic parsing events, it
  implements this interface and sets it with QXmlReader::setContentHandler().
  The reader reports basic document-related events like the start and end of
  elements and character data through this interface.

  The order of events in this interface is very important, and mirrors the
  order of information in the document itself. For example, all of an element's
  content (character data, processing instructions, and/or subelements) will
  appear, in order, between the startElement() event and the corresponding
  endElement() event.

  The class QXmlDefaultHandler gives a default implementation for this
  interface; subclassing from this class is very convenient if you want only be
  informed of some parsing events.

  See also the <a href="xml.html#introSAX2">Introduction to SAX2</a>.

  \sa QXmlDTDHandler QXmlDeclHandler QXmlEntityResolver QXmlErrorHandler
  QXmlLexicalHandler
*/
/*!
  \fn void QXmlContentHandler::setDocumentLocator( QXmlLocator* locator )

  The reader calls this function before he starts parsing the document. The
  argument \a locator is a pointer to a QXmlLocator which allows the
  application to get the actual position of the parsing in the document.

  Do not destroy the \a locator; it is destroyed when the reader is destroyed
  (do not use the \a locator after the reader got destroyed).
*/
/*!
  \fn bool QXmlContentHandler::startDocument()

  The reader calls this function when he starts parsing the document.
  The reader will call this function only once before any other functions in
  this class or in the QXmlDTDHandler class are called (except
  QXmlContentHandler::setDocumentLocator()).

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.

  \sa endDocument()
*/
/*!
  \fn bool QXmlContentHandler::endDocument()

  The reader calls this function after he has finished the parsing. It
  is only called once. It is the last function of all handler functions that is
  called. It is called after the reader has read all input or has abandoned
  parsing because of a fatal error.

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.

  \sa startDocument()
*/
/*!
  \fn bool QXmlContentHandler::startPrefixMapping( const QString& prefix, const QString& uri )

  The reader calls this function to signal the begin of a prefix-URI
  namespace mapping scope. This information is not necessary for normal
  namespace processing since the reader automatically replaces prefixes for
  element and attribute names.

  Note that startPrefixMapping and endPrefixMapping calls are not guaranteed to
  be properly nested relative to each-other: all startPrefixMapping events will
  occur before the corresponding startElement event, and all endPrefixMapping
  events will occur after the corresponding endElement event, but their order
  is not otherwise guaranteed.

  The argument \a prefix is the namespace prefix being declared and the
  argument \a uri is the namespace URI the prefix is mapped to.

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.

  See also the <a href="xml-sax.html#namespaces">namespace description</a>.

  \sa endPrefixMapping()
*/
/*!
  \fn bool QXmlContentHandler::endPrefixMapping( const QString& prefix )

  The reader calls this function to signal the end of a prefix mapping.

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.

  See also the <a href="xml-sax.html#namespaces">namespace description</a>.

  \sa startPrefixMapping()
*/
/*!
  \fn bool QXmlContentHandler::startElement( const QString& namespaceURI, const QString& localName, const QString& qName, const QXmlAttributes& atts )

  The reader calls this function when he has parsed a start element tag.

  There will be a corresponding endElement() call when the corresponding end
  element tag was read. The startElement() and endElement() calls are always
  nested correctly. Empty element tags (e.g. &lt;a/&gt;) are reported by
  startElement() directly followed by a call to endElement().

  The attribute list provided will contain only attributes with explicit
  values. The attribute list will contain attributes used for namespace
  declaration (i.e. attributes starting with xmlns) only if the
  namespace-prefix property of the reader is TRUE.

  The argument \a uri is the namespace URI, or the empty string if the element
  has no namespace URI or if namespace processing is not being performed, \a
  localName is the local name (without prefix), or the empty string if
  namespace processing is not being performed, \a qName is the qualified name
  (with prefix), or the empty string if qualified names are not available and
  \a atts are the attributes attached to the element. If there are no
  attributes, \a atts is an empty attributes object

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.

  See also the <a href="xml-sax.html#namespaces">namespace description</a>.

  \sa endElement()
*/
/*!
  \fn bool QXmlContentHandler::endElement( const QString& namespaceURI, const QString& localName, const QString& qName )

  The reader calls this function when he has parsed an end element tag.

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.

  See also the <a href="xml-sax.html#namespaces">namespace description</a>.

  \sa startElement()
*/
/*!
  \fn bool QXmlContentHandler::characters( const QString& ch )

  The reader calls this function when he has parsed a chunk of character
  data (either normal character data or character data inside a CDATA section;
  if you have to distinguish between those two types you have to use
  QXmlLexicalHandler::startCDATA() and QXmlLexicalHandler::endCDATA() in
  addition).

  Some readers will report whitespace in element content using the
  ignorableWhitespace() function rather than this one (QXmlSimpleReader will
  do it not though).

  A reader is allowed to report the character data of an element in more than
  one chunk; e.g. a reader might want to report "a &amp;lt; b" in three
  characters() events ("a ", "<" and " b").

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.
*/
/*!
  \fn bool QXmlContentHandler::ignorableWhitespace( const QString& ch )

  Some readers may use this function to report each chunk of whitespace in
  element content (QXmlSimpleReader does not though).

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.
*/
/*!
  \fn bool QXmlContentHandler::processingInstruction( const QString& target, const QString& data )

  The reader calls this function when he has parsed a processing
  instruction.

  \a target is the target name of the processing instruction and \a data is the
  data of the processing instruction.

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.
*/
/*!
  \fn bool QXmlContentHandler::skippedEntity( const QString& name )

  Some readers may skip entities if they have not seen the declarations (e.g.
  because they are in an external DTD). If they do so they will report it by
  calling this function.

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.
*/
/*!
  \fn QString QXmlContentHandler::errorString()

  The reader calls this function to get an error string if any of the handler
  functions returns FALSE to him.
*/


/*!
  \class QXmlErrorHandler qxml.h
  \brief The QXmlErrorHandler class provides an interface to report errors in
  XML data.

  \module XML

  If the application is interested in reporting errors to the user or any other
  customized error handling, you should subclass this class.

  You can set the error handler with QXmlReader::setErrorHandler().

  See also the <a href="xml.html#introSAX2">Introduction to SAX2</a>.

  \sa QXmlDTDHandler QXmlDeclHandler QXmlContentHandler QXmlEntityResolver
  QXmlLexicalHandler
*/
/*!
  \fn bool QXmlErrorHandler::warning( const QXmlParseException& exception )

  A reader might use this function to report a warning. Warnings are conditions
  that are not errors or fatal errors as defined by the XML 1.0 specification.

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.
*/
/*!
  \fn bool QXmlErrorHandler::error( const QXmlParseException& exception )

  A reader might use this function to report a recoverable error. A recoverable
  error corresponds to the definiton of "error" in section 1.2 of the XML 1.0
  specification.

  The reader must continue to provide normal parsing events after invoking this
  function.

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.
*/
/*!
  \fn bool QXmlErrorHandler::fatalError( const QXmlParseException& exception )

  A reader must use this function to report a non-recoverable error.

  If this function returns TRUE the reader might try to go on parsing and
  reporting further errors; but no regular parsing events are reported.
*/
/*!
  \fn QString QXmlErrorHandler::errorString()

  The reader calls this function to get an error string if any of the handler
  functions returns FALSE to him.
*/


/*!
  \class QXmlDTDHandler qxml.h
  \brief The QXmlDTDHandler class provides an interface to report DTD content
  of XML data.

  \module XML

  If an application needs information about notations and unparsed entities,
  then the application implements this interface and registers an instance with
  QXmlReader::setDTDHandler().

  Note that this interface includes only those DTD events that the XML
  recommendation requires processors to report: notation and unparsed entity
  declarations.

  See also the <a href="xml.html#introSAX2">Introduction to SAX2</a>.

  \sa QXmlDeclHandler QXmlContentHandler QXmlEntityResolver QXmlErrorHandler
  QXmlLexicalHandler
*/
/*!
  \fn bool QXmlDTDHandler::notationDecl( const QString& name, const QString& publicId, const QString& systemId )

  The reader calls this function when he has parsed a notation
  declaration.

  The argument \a name is the notation name, \a publicId is the notations's
  public identifier and \a systemId is the notations's system identifier.

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.
*/
/*!
  \fn bool QXmlDTDHandler::unparsedEntityDecl( const QString& name, const QString& publicId, const QString& systemId, const QString& notationName )

  The reader calls this function when he finds an unparsed entity declaration.

  The argument \a name is the unparsed entity's name, \a publicId is the
  entity's public identifier, \a systemId is the entity's system identifier and
  \a notation is the name of the associated notation.

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.
*/
/*!
  \fn QString QXmlDTDHandler::errorString()

  The reader calls this function to get an error string if any of the handler
  functions returns FALSE to him.
*/


/*!
  \class QXmlEntityResolver qxml.h
  \brief The QXmlEntityResolver class provides an interface to resolve extern
  entities contained in XML data.

  \module XML

  If an application needs to implement customized handling for external
  entities, it must implement this interface and register it with
  QXmlReader::setEntityResolver().

  See also the <a href="xml.html#introSAX2">Introduction to SAX2</a>.

  \sa QXmlDTDHandler QXmlDeclHandler QXmlContentHandler QXmlErrorHandler
  QXmlLexicalHandler
*/
/*!
  \fn bool QXmlEntityResolver::resolveEntity( const QString& publicId, const QString& systemId, QXmlInputSource* ret )

  The reader will call this function before he opens any external entity,
  except the top-level document entity. The application may request the reader
  to resolve the entity itself (\a ret is 0) or to use an entirely different
  input source (\a ret points to the input source).

  The reader will delete the input source \a ret when he no longer needs it. So
  you should allocate it on the heap with \c new.

  The argument \a publicId is the public identifier of the external entity, \a
  systemId is the system identifier of the external entity and \a ret is the
  return value of this function: if it is 0 the reader should resolve the
  entity itself, if it is non-zero it must point to an input source which the
  reader will use instead.

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.
*/
/*!
  \fn QString QXmlEntityResolver::errorString()

  The reader calls this function to get an error string if any of the handler
  functions returns FALSE to him.
*/


/*!
  \class QXmlLexicalHandler qxml.h
  \brief The QXmlLexicalHandler class provides an interface to report lexical
  content of XML data.

  \module XML

  The events in the lexical handler apply to the entire document, not just to
  the document element, and all lexical handler events appear between the
  content handler's startDocument and endDocument events.

  You can set the lexical handler with QXmlReader::setLexicalHandler().

  This interface is designed after the SAX2 extension LexicalHandler. The
  functions startEntity() and endEntity() are not included though.

  See also the <a href="xml.html#introSAX2">Introduction to SAX2</a>.

  \sa QXmlDTDHandler QXmlDeclHandler QXmlContentHandler QXmlEntityResolver
  QXmlErrorHandler
*/
/*!
  \fn bool QXmlLexicalHandler::startDTD( const QString& name, const QString& publicId, const QString& systemId )

  The reader calls this function to report the start of a DTD declaration, if
  any.

  All declarations reported through QXmlDTDHandler or QXmlDeclHandler appear
  between the startDTD() and endDTD() calls.

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.

  \sa endDTD()
*/
/*!
  \fn bool QXmlLexicalHandler::endDTD()

  The reader calls this function to report the end of a DTD declaration, if
  any.

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.

  \sa startDTD()
*/
/*!
  \fn bool QXmlLexicalHandler::startCDATA()

  The reader calls this function to report the start of a CDATA section. The
  content of the CDATA section will be reported through the regular
  QXmlContentHandler::characters(). This function is intended only to report
  the boundary.

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.

  \sa endCDATA()
*/
/*!
  \fn bool QXmlLexicalHandler::endCDATA()

  The reader calls this function to report the end of a CDATA section.

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.

  \sa startCDATA()
*/
/*!
  \fn bool QXmlLexicalHandler::comment( const QString& ch )

  The reader calls this function to report an XML comment anywhere in the
  document.

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.
*/
/*!
  \fn QString QXmlLexicalHandler::errorString()

  The reader calls this function to get an error string if any of the handler
  functions returns FALSE to him.
*/


/*!
  \class QXmlDeclHandler qxml.h
  \brief The QXmlDeclHandler class provides an interface to report declaration
  content of XML data.

  \module XML

  You can set the declaration handler with QXmlReader::setDeclHandler().

  This interface is designed after the SAX2 extension DeclHandler.

  See also the <a href="xml.html#introSAX2">Introduction to SAX2</a>.

  \sa QXmlDTDHandler QXmlContentHandler QXmlEntityResolver QXmlErrorHandler
  QXmlLexicalHandler
*/
/*!
  \fn bool QXmlDeclHandler::attributeDecl( const QString& eName, const QString& aName, const QString& type, const QString& valueDefault, const QString& value )

  The reader calls this function to report an attribute type declaration. Only
  the effective (first) declaration for an attribute will be reported.

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.
*/
/*!
  \fn bool QXmlDeclHandler::internalEntityDecl( const QString& name, const QString& value )

  The reader calls this function to report an internal entity declaration. Only
  the effective (first) declaration will be reported.

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.
*/
/*!
  \fn bool QXmlDeclHandler::externalEntityDecl( const QString& name, const QString& publicId, const QString& systemId )

  The reader calls this function to report a parsed external entity
  declaration. Only the effective (first) declaration for each entity will be
  reported.

  If this function returns FALSE the reader will stop parsing and will report
  an error. The reader will use the function errorString() to get the error
  message that will be used for reporting the error.
*/
/*!
  \fn QString QXmlDeclHandler::errorString()

  The reader calls this function to get an error string if any of the handler
  functions returns FALSE to him.
*/


/*!
  \class QXmlDefaultHandler qxml.h
  \brief The QXmlDefaultHandler class provides a default implementation of all
  XML handler classes.

  \module XML

  Very often you are only interested in parts of the things that that the
  reader reports to you. This class simply implements a default behaviour of
  the handler classes (most of the time: do nothing). Normally this is the
  class you subclass for implementing your customized handler.

  See also the <a href="xml.html#introSAX2">Introduction to SAX2</a>.

  \sa QXmlDTDHandler QXmlDeclHandler QXmlContentHandler QXmlEntityResolver
  QXmlErrorHandler QXmlLexicalHandler
*/
/*!
  \fn QXmlDefaultHandler::QXmlDefaultHandler()

  Constructor.
*/
/*!
  \fn QXmlDefaultHandler::~QXmlDefaultHandler()

  Destructor.
*/

/*!
  Does nothing.
*/
void QXmlDefaultHandler::setDocumentLocator( QXmlLocator* )
{
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::startDocument()
{
    return TRUE;
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::endDocument()
{
    return TRUE;
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::startPrefixMapping( const QString&, const QString& )
{
    return TRUE;
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::endPrefixMapping( const QString& )
{
    return TRUE;
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::startElement( const QString&, const QString&,
	const QString&, const QXmlAttributes& )
{
    return TRUE;
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::endElement( const QString&, const QString&,
	const QString& )
{
    return TRUE;
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::characters( const QString& )
{
    return TRUE;
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::ignorableWhitespace( const QString& )
{
    return TRUE;
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::processingInstruction( const QString&,
	const QString& )
{
    return TRUE;
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::skippedEntity( const QString& )
{
    return TRUE;
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::warning( const QXmlParseException& )
{
    return TRUE;
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::error( const QXmlParseException& )
{
    return TRUE;
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::fatalError( const QXmlParseException& )
{
    return TRUE;
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::notationDecl( const QString&, const QString&,
	const QString& )
{
    return TRUE;
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::unparsedEntityDecl( const QString&, const QString&,
	const QString&, const QString& )
{
    return TRUE;
}

/*!
  Always sets \a ret to 0, so that the reader will use the system identifier
  provided in the XML document.
*/
bool QXmlDefaultHandler::resolveEntity( const QString&, const QString&,
	QXmlInputSource* ret )
{
    ret = 0;
    return TRUE;
}

/*!
  Returns the default error string.
*/
QString QXmlDefaultHandler::errorString()
{
    return QString( XMLERR_ERRORBYCONSUMER );
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::startDTD( const QString&, const QString&, const QString& )
{
    return TRUE;
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::endDTD()
{
    return TRUE;
}

#if 0
/*!
  Does nothing.
*/
bool QXmlDefaultHandler::startEntity( const QString& )
{
    return TRUE;
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::endEntity( const QString& )
{
    return TRUE;
}
#endif

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::startCDATA()
{
    return TRUE;
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::endCDATA()
{
    return TRUE;
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::comment( const QString& )
{
    return TRUE;
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::attributeDecl( const QString&, const QString&, const QString&, const QString&, const QString& )
{
    return TRUE;
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::internalEntityDecl( const QString&, const QString& )
{
    return TRUE;
}

/*!
  Does nothing.
*/
bool QXmlDefaultHandler::externalEntityDecl( const QString&, const QString&, const QString& )
{
    return TRUE;
}


/*********************************************
 *
 * QXmlSimpleReaderPrivate
 *
 *********************************************/

class QXmlSimpleReaderPrivate
{
private:
    // constructor
    QXmlSimpleReaderPrivate()
    { }


    // used for entity declarations
    struct ExternParameterEntity
    {
	ExternParameterEntity( ) {}
	ExternParameterEntity( const QString &p, const QString &s )
	    : publicId(p), systemId(s) {}
	QString publicId;
	QString systemId;
    };
    struct ExternEntity
    {
	ExternEntity( ) {}
	ExternEntity( const QString &p, const QString &s, const QString &n )
	    : publicId(p), systemId(s), notation(n) {}
	QString publicId;
	QString systemId;
	QString notation;
    };
    QMap<QString,ExternParameterEntity> externParameterEntities;
    QMap<QString,QString> parameterEntities;
    QMap<QString,ExternEntity> externEntities;
    QMap<QString,QString> entities;

    // used for standalone declaration
    enum Standalone { Yes, No, Unknown };

    QString doctype; // only used for the doctype
    QString xmlVersion; // only used to store the version information
    QString encoding; // only used to store the encoding
    Standalone standalone; // used to store the value of the standalone declaration

    QString publicId; // used by parseExternalID() to store the public ID
    QString systemId; // used by parseExternalID() to store the system ID
    QString attDeclEName; // use by parseAttlistDecl()
    QString attDeclAName; // use by parseAttlistDecl()

    // flags for some features support
    bool useNamespaces;
    bool useNamespacePrefixes;
    bool reportWhitespaceCharData;

    // used to build the attribute list
    QXmlAttributes attList;

    // helper classes
    QXmlLocator *locator;
    QXmlNamespaceSupport namespaceSupport;

    // error string
    QString error;

    // friend declarations
    friend class QXmlSimpleReader;
};


/*********************************************
 *
 * QXmlSimpleReader
 *
 *********************************************/

/*!
  \class QXmlReader qxml.h
  \brief The QXmlReader class provides an interface for XML readers (i.e.
  parsers).

  \module XML

  This abstract class describes an interface for all XML readers in Qt. At the
  moment there is only one implementation of a reader included in the XML
  module of Qt (QXmlSimpleReader). In future releases there might be more
  readers with different properties available (e.g. a validating parser).

  The design of the XML classes follow the
  <a href="http://www.megginson.com/SAX/">SAX2 java interface</a>.
  It was adopted to fit into the Qt naming conventions; so it should be very
  easy for anybody who has worked with SAX2 to get started with the Qt XML
  classes.

  All readers use the class QXmlInputSource to read the input document from.
  Since you are normally interested in certain contents of the XML document,
  the reader reports those contents through special handler classes
  (QXmlDTDHandler, QXmlDeclHandler, QXmlContentHandler, QXmlEntityResolver,
  QXmlErrorHandler and QXmlLexicalHandler).

  You have to subclass these classes. Since the handler classes describe only
  interfaces you must implement all functions; there is a class
  (QXmlDefaultHandler) to make this easier; it implements a default behaviour
  (do nothing) for all functions.

  For getting started see also the
  <a href="xml-sax.html#quickStart">Quick start</a>.

  \sa QXmlSimpleReader
*/
/*!
  \fn bool QXmlReader::feature( const QString& name, bool *ok ) const

  If the reader has the feature \a name, this function returns the value of the
  feature.

  If the reader has not the feature \a name, the return value may be anything.

  If \a ok is not 0, then \a ok  is set to TRUE if the reader has the feature
  \a name, otherwise \a ok is set to FALSE.

  \sa setFeature() hasFeature()
*/
/*!
  \fn void QXmlReader::setFeature( const QString& name, bool value )

  Sets the feature \a name to \a value. If the reader has not the feature \a
  name, this value is ignored.

  \sa feature() hasFeature()
*/
/*!
  \fn bool QXmlReader::hasFeature( const QString& name ) const

  Returns \c TRUE if the reader has the feature \a name, otherwise FALSE.

  \sa feature() setFeature()
*/
/*!
  \fn void* QXmlReader::property( const QString& name, bool *ok ) const

  If the reader has the property \a name, this function returns the value of
  the property.

  If the reader has not the property \a name, the return value is 0.

  If \a ok is not 0, then \a ok  is set to TRUE if the reader has the property
  \a name, otherwise \a ok is set to FALSE.

  \sa setProperty() hasProperty()
*/
/*!
  \fn void QXmlReader::setProperty( const QString& name, void* value )

  Sets the property \a name to \a value. If the reader has not the property \a
  name, this value is ignored.

  \sa property() hasProperty()
*/
/*!
  \fn bool QXmlReader::hasProperty( const QString& name ) const

  Returns TRUE if the reader has the property \a name, otherwise FALSE.

  \sa property() setProperty()
*/
/*!
  \fn void QXmlReader::setEntityResolver( QXmlEntityResolver* handler )

  Sets the entity resolver to \a handler.

  \sa entityResolver()
*/
/*!
  \fn QXmlEntityResolver* QXmlReader::entityResolver() const

  Returns the entity resolver or 0 if none was set.

  \sa setEntityResolver()
*/
/*!
  \fn void QXmlReader::setDTDHandler( QXmlDTDHandler* handler )

  Sets the DTD handler to \a handler.

  \sa DTDHandler()
*/
/*!
  \fn QXmlDTDHandler* QXmlReader::DTDHandler() const

  Returns the DTD handler or 0 if none was set.

  \sa setDTDHandler()
*/
/*!
  \fn void QXmlReader::setContentHandler( QXmlContentHandler* handler )

  Sets the content handler to \a handler.

  \sa contentHandler()
*/
/*!
  \fn QXmlContentHandler* QXmlReader::contentHandler() const

  Returns the content handler or 0 if none was set.

  \sa setContentHandler()
*/
/*!
  \fn void QXmlReader::setErrorHandler( QXmlErrorHandler* handler )

  Sets the error handler to \a handler.

  \sa errorHandler()
*/
/*!
  \fn QXmlErrorHandler* QXmlReader::errorHandler() const

  Returns the error handler or 0 if none was set

  \sa setErrorHandler()
*/
/*!
  \fn void QXmlReader::setLexicalHandler( QXmlLexicalHandler* handler )

  Sets the lexical handler to \a handler.

  \sa lexicalHandler()
*/
/*!
  \fn QXmlLexicalHandler* QXmlReader::lexicalHandler() const

  Returns the lexical handler or 0 if none was set.

  \sa setLexicalHandler()
*/
/*!
  \fn void QXmlReader::setDeclHandler( QXmlDeclHandler* handler )

  Sets the declaration handler to \a handler.

  \sa declHandler()
*/
/*!
  \fn QXmlDeclHandler* QXmlReader::declHandler() const

  Returns the declaration handler or 0 if none was set.

  \sa setDeclHandler()
*/
/*!
  \fn bool QXmlReader::parse( const QXmlInputSource& input )

  Parses the XML document \a input. Returns TRUE if the parsing was successful,
  otherwise FALSE.
*/
/*!
  \fn bool QXmlReader::parse( const QString& systemId )

  Parses the XML document at the location \a systemId. Returns TRUE if the
  parsing was successful, otherwise FALSE.
*/


/*!
  \class QXmlSimpleReader qxml.h
  \brief The QXmlSimpleReader class provides an implementation of a simple XML
  reader (i.e. parser).

  \module XML

  This XML reader is sufficient for simple parsing tasks. Here is a short list
  of the properties of this reader:
  <ul>
  <li> well-formed parser
  <li> does not parse any external entities
  <li> can do namespace processing
  </ul>

  For getting started see also the
  <a href="xml-sax.html#quickStart">Quick start</a>.
*/

//guaranteed not to be a characater
const QChar QXmlSimpleReader::QEOF = QChar((ushort)0xffff);

/*!
  Constructs a simple XML reader.
*/
QXmlSimpleReader::QXmlSimpleReader()
{
    d = new QXmlSimpleReaderPrivate();
    d->locator = new QXmlLocator( this );

    entityRes  = 0;
    dtdHnd     = 0;
    contentHnd = 0;
    errorHnd   = 0;
    lexicalHnd = 0;
    declHnd    = 0;

    // default feature settings
    d->useNamespaces = TRUE;
    d->useNamespacePrefixes = FALSE;
    d->reportWhitespaceCharData = TRUE;
}

/*!
  Destroys a simple XML reader.
*/
QXmlSimpleReader::~QXmlSimpleReader()
{
    delete d->locator;
    delete d;
}

/*!
  Gets the state of a feature.

  \sa setFeature() hasFeature()
*/
bool QXmlSimpleReader::feature( const QString& name, bool *ok ) const
{
    if ( ok != 0 )
	*ok = TRUE;
    if        ( name == "http://xml.org/sax/features/namespaces" ) {
	return d->useNamespaces;
    } else if ( name == "http://xml.org/sax/features/namespace-prefixes" ) {
	return d->useNamespacePrefixes;
    } else if ( name == "http://trolltech.com/xml/features/report-whitespace-only-CharData" ) {
	return d->reportWhitespaceCharData;
    } else {
	qWarning( "Unknown feature %s", name.ascii() );
	if ( ok != 0 )
	    *ok = FALSE;
    }
    return FALSE;
}

/*!
  Sets the state of a feature.

  Supported features are:
  <ul>
  <li> http://xml.org/sax/features/namespaces:
       if this feature is TRUE, namespace processing is performed
  <li> http://xml.org/sax/features/namespace-prefixes:
       if this feature is TRUE, the the original prefixed names and attributes
       used for namespace declarations are reported
  <li> http://trolltech.com/xml/features/report-whitespace-only-CharData:
       if this feature is TRUE, CharData that consists only of whitespace (and
       no other characters) is not reported via
       QXmlContentHandler::characters()
  </ul>

  \sa feature() hasFeature()
*/
void QXmlSimpleReader::setFeature( const QString& name, bool value )
{
    if        ( name == "http://xml.org/sax/features/namespaces" ) {
	d->useNamespaces = value;
    } else if ( name == "http://xml.org/sax/features/namespace-prefixes" ) {
	d->useNamespacePrefixes = value;
    } else if ( name == "http://trolltech.com/xml/features/report-whitespace-only-CharData" ) {
	d->reportWhitespaceCharData = value;
    } else {
	qWarning( "Unknown feature %s", name.ascii() );
    }
}

/*!
  Returns TRUE if the class has a feature named \a feature, otherwise FALSE.

  \sa setFeature() feature()
*/
bool QXmlSimpleReader::hasFeature( const QString& name ) const
{
    if (    name == "http://xml.org/sax/features/namespaces" ||
	    name == "http://xml.org/sax/features/namespace-prefixes" ||
	    name == "http://trolltech.com/xml/features/report-whitespace-only-CharData" ) {
	return TRUE;
    } else {
	return FALSE;
    }
}

/*!
  Returns 0 since this class does not support any properties.
*/
void* QXmlSimpleReader::property( const QString&, bool *ok ) const
{
    if ( ok != 0 )
	*ok = FALSE;
    return 0;
}

/*!
  Does nothing since this class does not support any properties.
*/
void QXmlSimpleReader::setProperty( const QString&, void* )
{
}

/*!
  Returns FALSE since this class does not support any properties.
*/
bool QXmlSimpleReader::hasProperty( const QString& ) const
{
    return FALSE;
}

/*! \reimp */
void QXmlSimpleReader::setEntityResolver( QXmlEntityResolver* handler )
{ entityRes = handler; }

/*! \reimp */
QXmlEntityResolver* QXmlSimpleReader::entityResolver() const
{ return entityRes; }

/*! \reimp */
void QXmlSimpleReader::setDTDHandler( QXmlDTDHandler* handler )
{ dtdHnd = handler; }

/*! \reimp */
QXmlDTDHandler* QXmlSimpleReader::DTDHandler() const
{ return dtdHnd; }

/*! \reimp */
void QXmlSimpleReader::setContentHandler( QXmlContentHandler* handler )
{ contentHnd = handler; }

/*! \reimp */
QXmlContentHandler* QXmlSimpleReader::contentHandler() const
{ return contentHnd; }

/*! \reimp */
void QXmlSimpleReader::setErrorHandler( QXmlErrorHandler* handler )
{ errorHnd = handler; }

/*! \reimp */
QXmlErrorHandler* QXmlSimpleReader::errorHandler() const
{ return errorHnd; }

/*! \reimp */
void QXmlSimpleReader::setLexicalHandler( QXmlLexicalHandler* handler )
{ lexicalHnd = handler; }

/*! \reimp */
QXmlLexicalHandler* QXmlSimpleReader::lexicalHandler() const
{ return lexicalHnd; }

/*! \reimp */
void QXmlSimpleReader::setDeclHandler( QXmlDeclHandler* handler )
{ declHnd = handler; }

/*! \reimp */
QXmlDeclHandler* QXmlSimpleReader::declHandler() const
{ return declHnd; }



/*! \reimp */
bool QXmlSimpleReader::parse( const QXmlInputSource& input )
{
    init( input );
    // call the handler
    if ( contentHnd ) {
	contentHnd->setDocumentLocator( d->locator );
	if ( !contentHnd->startDocument() ) {
	    d->error = contentHnd->errorString();
	    goto parseError;
	}
    }
    // parse prolog
    if ( !parseProlog() ) {
	d->error = XMLERR_ERRORPARSINGPROLOG;
	goto parseError;
    }
    // parse element
    if ( !parseElement() ) {
	d->error = XMLERR_ERRORPARSINGMAINELEMENT;
	goto parseError;
    }
    // parse Misc*
    while ( !atEnd() ) {
	if ( !parseMisc() ) {
	    d->error = XMLERR_ERRORPARSINGMISC;
	    goto parseError;
	}
    }
    // is stack empty?
    if ( !tags.isEmpty() ) {
	d->error = XMLERR_UNEXPECTEDEOF;
	goto parseError;
    }
    // call the handler
    if ( contentHnd ) {
	if ( !contentHnd->endDocument() ) {
	    d->error = contentHnd->errorString();
	    goto parseError;
	}
    }

    return TRUE;

    // error handling

parseError:
    reportParseError();
    tags.clear();
    return FALSE;
}

/*!
  Parses the prolog [22].
*/
bool QXmlSimpleReader::parseProlog()
{
    bool xmldecl_possible = TRUE;
    bool doctype_read = FALSE;

    const signed char Init             = 0;
    const signed char EatWS            = 1; // eat white spaces
    const signed char Lt               = 2; // '<' read
    const signed char Em               = 3; // '!' read
    const signed char DocType          = 4; // read doctype
    const signed char Comment          = 5; // read comment
    const signed char PI               = 6; // read PI
    const signed char Done             = 7;

    const signed char InpWs            = 0;
    const signed char InpLt            = 1; // <
    const signed char InpQm            = 2; // ?
    const signed char InpEm            = 3; // !
    const signed char InpD             = 4; // D
    const signed char InpDash          = 5; // -
    const signed char InpUnknown       = 6;

    // use some kind of state machine for parsing
    static signed char table[7][7] = {
     /*  InpWs   InpLt  InpQm  InpEm  InpD      InpDash  InpUnknown */
	{ EatWS,  Lt,    -1,    -1,    -1,       -1,       -1      }, // Init
	{ -1,     Lt,    -1,    -1,    -1,       -1,       -1      }, // EatWS
	{ -1,     -1,    PI,    Em,    Done,     -1,       Done    }, // Lt
	{ -1,     -1,    -1,    -1,    DocType,  Comment,  -1      }, // Em
	{ EatWS,  Lt,    -1,    -1,    -1,       -1,       -1      }, // DocType
	{ EatWS,  Lt,    -1,    -1,    -1,       -1,       -1      }, // Comment
	{ EatWS,  Lt,    -1,    -1,    -1,       -1,       -1      }  // PI
    };
    signed char state = Init;
    signed char input;
    bool parseOk = TRUE;

    while ( TRUE ) {

	// read input
	if ( atEnd() ) {
	    d->error = XMLERR_UNEXPECTEDEOF;
	    goto parseError;
	}
	if        ( is_S(c) ) {
	    input = InpWs;
	} else if ( c == '<' ) {
	    input = InpLt;
	} else if ( c == '?' ) {
	    input = InpQm;
	} else if ( c == '!' ) {
	    input = InpEm;
	} else if ( c == 'D' ) {
	    input = InpD;
	} else if ( c == '-' ) {
	    input = InpDash;
	} else {
	    input = InpUnknown;
	}
	// get new state
	state = table[state][input];

	// in some cases do special actions depending on state
	switch ( state ) {
	    case EatWS:
		// XML declaration only on first position possible
		xmldecl_possible = FALSE;
		// eat white spaces
		eat_ws();
		break;
	    case Lt:
		// next character
		next();
		break;
	    case Em:
		// XML declaration only on first position possible
		xmldecl_possible = FALSE;
		// next character
		next();
		break;
	    case DocType:
		parseOk = parseDoctype();
		break;
	    case Comment:
		parseOk = parseComment();
		brea