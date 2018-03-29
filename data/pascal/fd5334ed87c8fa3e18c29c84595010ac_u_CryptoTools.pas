unit u_CryptoTools;

interface

uses
  Windows;

type
  HCRYPTPROV = ULONG;
  PHCRYPTPROV = ^HCRYPTPROV;
  HCRYPTKEY = ULONG;
  PHCRYPTKEY = ^HCRYPTKEY;
  HCRYPTHASH = ULONG;
  PHCRYPTHASH = ^HCRYPTHASH;
  ALG_ID = type of ULONG;
  PVOID = Pointer;
  HCERTSTORE = Pointer;

  HCRYPTPROV_LEGACY = HCRYPTPROV;

  PCRYPTOAPI_BLOB = ^CRYPTOAPI_BLOB;
  CRYPTOAPI_BLOB = record
    cbData: DWORD;
    pbData: PBYTE;
  end;

  CRYPT_OBJID_BLOB = CRYPTOAPI_BLOB;
  CRYPT_INTEGER_BLOB = CRYPTOAPI_BLOB;
  CERT_NAME_BLOB = CRYPTOAPI_BLOB;

  PCRYPT_BIT_BLOB = ^CRYPT_BIT_BLOB;
  CRYPT_BIT_BLOB = record
    cbData: DWORD;
    pbData: PBYTE;
    cUnusedBits: DWORD;
  end;

  PCRYPT_ALGORITHM_IDENTIFIER = ^CRYPT_ALGORITHM_IDENTIFIER;
  CRYPT_ALGORITHM_IDENTIFIER = packed record
    pszObjId: LPSTR;
    Parameters: CRYPT_OBJID_BLOB;
  end;


  PCRYPT_ENCRYPT_MESSAGE_PARA = ^CRYPT_ENCRYPT_MESSAGE_PARA;
  CRYPT_ENCRYPT_MESSAGE_PARA = packed record
    cbSize: DWORD;
    dwMsgEncodingType: DWORD;
    hCryptProv: HCRYPTPROV_LEGACY;
    ContentEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvEncryptionAuxInfo: PVOID;
    dwFlags: DWORD;
    dwInnerContentType: DWORD;
  end;

  PCERT_PUBLIC_KEY_INFO = ^CERT_PUBLIC_KEY_INFO;
  CERT_PUBLIC_KEY_INFO = packed record
    Algorithm: CRYPT_ALGORITHM_IDENTIFIER;
    PublicKey: CRYPT_BIT_BLOB;
  end;

  PCERT_EXTENSION = ^CERT_EXTENSION;
  CERT_EXTENSION = packed record
    pszObjId: LPSTR;
    fCritical: BOOL;
    Value: CRYPT_OBJID_BLOB;
  end;

  PCERT_INFO = ^CERT_INFO;
  CERT_INFO = packed record
    dwVersion: DWORD;
    SerialNumber: CRYPT_INTEGER_BLOB;
    SignatureAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    Issuer: CERT_NAME_BLOB;
    NotBefore: FILETIME;
    NotAfter: FILETIME;
    Subject: CERT_NAME_BLOB;
    SubjectPublicKeyInfo: CERT_PUBLIC_KEY_INFO;
    IssuerUniqueId: CRYPT_BIT_BLOB;
    SubjectUniqueId: CRYPT_BIT_BLOB;
    cExtension: DWORD;
    rgExtension: PCERT_EXTENSION;
  end;

  PCCERT_CONTEXT = ^CERT_CONTEXT;
  CERT_CONTEXT = packed record
    dwCertEncodingType: DWORD;
    pbCertEncoded: PBYTE;
    cbCertEncoded: DWORD;
    pCertInfo: PCERT_INFO;
    hCertStore: HCERTSTORE;
  end;
  ARR_PCCERT_CONTEXT = array [0..0] of PCCERT_CONTEXT;


TCryptAcquireContextA = function(
  const phProv: PHCRYPTPROV;
  const pszContainer: PAnsiChar;
  const pszProvider: PAnsiChar;
  const dwProvType: DWORD;
  const dwFlags: DWORD
): BOOL; stdcall; (*external advapi32;*)

TCryptAcquireContextW = function(
  const phProv: PHCRYPTPROV;
  const pszContainer: PWideChar;
  const pszProvider: PWideChar;
  const dwProvType: DWORD;
  const dwFlags: DWORD
): BOOL; stdcall; (*external advapi32;*)

{$IFDEF UNICODE}
TCryptAcquireContext = TCryptAcquireContextW;
{$ELSE}
TCryptAcquireContext = TCryptAcquireContextA;
{$ENDIF}


TCryptReleaseContext = function(
  hProv: HCRYPTPROV;
  dwFlags: DWORD
): BOOL; stdcall; (*external advapi32;*)


TCryptCreateHash = function(
  hProv: HCRYPTPROV;
  Algid: ALG_ID;
  hKey: HCRYPTKEY;
  dwFlags: DWORD;
  phHash: PHCRYPTHASH
): BOOL; stdcall; (*external advapi32;*)

TCryptHashData = function(
  hHash: HCRYPTHASH;
  pbData: PBYTE;
  dwDataLen: DWORD;
  dwFlags: DWORD
): BOOL; stdcall; (*external advapi32;*)

TCryptDeriveKey = function(
  hProv: HCRYPTPROV;
  Algid: ALG_ID;
  hBaseData: HCRYPTHASH;
  dwFlags: DWORD;
  phKey: PHCRYPTKEY
): BOOL; stdcall; (*external advapi32;*)

TCryptDestroyKey = function(
  hKey: HCRYPTKEY
): BOOL; stdcall; (*external advapi32;*)

TCryptDestroyHash = function(
  hHash: HCRYPTHASH
): BOOL; stdcall; (*external advapi32;*)


TCryptEncrypt = function(
  hKey: HCRYPTKEY;
  hHash: HCRYPTHASH;
  Final_: BOOL;
  dwFlags: DWORD;
  pbData: PBYTE;
  pdwDataLen: PDWORD;
  dwBufLen: DWORD
): BOOL; stdcall; (*external advapi32;*)

TCryptDecrypt = function(
  hKey: HCRYPTKEY;
  hHash: HCRYPTHASH;
  Final_: BOOL;
  dwFlags: DWORD;
  pbData: PBYTE;
  pdwDataLen: PDWORD
): BOOL; stdcall; (*external advapi32;*)

TCryptGenKey = function(
  hProv: HCRYPTPROV;
  Algid: ALG_ID;
  dwFlags: DWORD;
  phKey: PHCRYPTKEY
): BOOL; stdcall; (*external advapi32;*)

TCryptImportKey = function(
  hProv: HCRYPTPROV;
  pbData: PBYTE;
  dwDataLen: DWORD;
  hPubKey: HCRYPTKEY;
  dwFlags: DWORD;
  phKey: PHCRYPTKEY
): BOOL; stdcall; (*external advapi32;*)

TCryptGetProvParam = function(
  hProv: HCRYPTPROV;
  dwParam: DWORD;
  pbData: PBYTE;
  pdwDataLen: PDWORD;
  dwFlags: DWORD
): BOOL; stdcall; (*external advapi32;*)

TCryptGetKeyParam = function(
  hKey: HCRYPTKEY;
  dwParam: DWORD;
  pbData: PBYTE;
  pdwDataLen: PDWORD;
  dwFlags: DWORD
): BOOL; stdcall; (*external advapi32;*)



TCryptEncryptMessage = function(
  pEncryptPara: PCRYPT_ENCRYPT_MESSAGE_PARA;
  cRecipientCert: DWORD;
  rgpRecipientCert: ARR_PCCERT_CONTEXT;
  pbToBeEncrypted: PBYTE;
  cbToBeEncrypted: DWORD;
  pbEncryptedBlob: PBYTE;
  pcbEncryptedBlob: DWORD
): BOOL; stdcall; (*external advapi32;*)




TCryptEnumProvidersA = function(
  dwIndex: DWORD;
  pdwReserved: PDWORD;
  dwFlags: DWORD;
  pdwProvType: PDWORD;
  pszProvName: PAnsiChar;
  pcbProvName: PDWORD
): BOOL; stdcall; (*external advapi32;*)

TCryptEnumProvidersW = function(
  dwIndex: DWORD;
  pdwReserved: PDWORD;
  dwFlags: DWORD;
  pdwProvType: PDWORD;
  pszProvName: PWideChar;
  pcbProvName: PDWORD
): BOOL; stdcall; (*external advapi32;*)

{$IFDEF UNICODE}
TCryptEnumProviders = TCryptEnumProvidersW;
{$ELSE}
TCryptEnumProviders = TCryptEnumProvidersA;
{$ENDIF}

const
  // HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Cryptography\Defaults
  
  PROV_RSA_FULL     =  1;
  PROV_RSA_AES      = -1;
  PROV_RSA_SIG      =  2;
  PROV_RSA_SCHANNEL = 12;
  PROV_DSS          =  3;
  PROV_DSS_DH       = 13;
  PROV_DH_SCHANNEL  = -1;
  PROV_FORTEZZA     =  4;
  PROV_MS_EXCHANGE  =  5;
  PROV_SSL          =  6;

  // 'Infotecs Cryptographic Service Provider'; // GOST R3410/11 Provider
  MS_DEF_PROV              = 'Microsoft Base Cryptographic Provider v1.0';
  MS_DEF_DSS_DH_PROV       = 'Microsoft Base DSS and Diffie-Hellman Cryptographic Provider';
  MS_DEF_DSS_PROV          = 'Microsoft Base DSS Cryptographic Provider';
  MS_SCARD_PROV            = 'Microsoft Base Smart Card Crypto Provider';
  MS_DEF_DH_SCHANNEL_PROV  = 'Microsoft DH Schannel Cryptographic Provider';
  MS_ENHANCED_PROV         = 'Microsoft Enhanced Cryptographic Provider v1.0';
  MS_ENH_DSS_DH_PROV       = 'Microsoft Enhanced DSS and Diffie-Hellman Cryptographic Provider';
  MS_ENH_RSA_AES_PROV      = 'Microsoft Enhanced RSA and AES Cryptographic Provider';
  // 'Microsoft Exchange Cryptographic Provider v1.0';
  MS_DEF_RSA_SCHANNEL_PROV = 'Microsoft RSA Schannel Cryptographic Provider';
  MS_DEF_RSA_SIG_PROV      = 'Microsoft RSA Signature Cryptographic Provider';
  MS_STRONG_PROV           = 'Microsoft Strong Cryptographic Provider';

  CRYPT_VERIFYCONTEXT  = $F0000000;
  CRYPT_NEWKEYSET      = $00000008;
  CRYPT_DELETEKEYSET   = $00000010;
  CRYPT_MACHINE_KEYSET = $00000020;

  CRYPT_FIRST = 1;
  CRYPT_NEXT = 2;

  CRYPT_IMPL_HARDWARE = 1;
  CRYPT_IMPL_SOFTWARE = 2;
  CRYPT_IMPL_MIXED = 3;
  CRYPT_IMPL_UNKNOWN = 4;


const
  // http://msdn.microsoft.com/ru-ru/library/windows/desktop/aa375549(v=vs.85).aspx
CALG_3DES      = $00006603; // Triple DES encryption algorithm.
CALG_3DES_112  = $00006609; // Two-key triple DES encryption with effective key length equal to 112 bits.

CALG_AES       = $00006611; // Advanced Encryption Standard (AES).
// This algorithm is supported by the Microsoft AES Cryptographic Provider.
// Windows 2000/NT:  This algorithm is not supported.
CALG_AES_128   = $0000660e; // 128 bit AES. This algorithm is supported by the Microsoft AES Cryptographic Provider.
// Windows 2000/NT:  This algorithm is not supported.
CALG_AES_192   = $0000660f; // 192 bit AES. This algorithm is supported by the Microsoft AES Cryptographic Provider.
// Windows 2000/NT:  This algorithm is not supported.
CALG_AES_256   = $00006610; // 256 bit AES. This algorithm is supported by the Microsoft AES Cryptographic Provider.
// Windows 2000/NT:  This algorithm is not supported.

CALG_AGREEDKEY_ANY = $0000aa03; // Temporary algorithm identifier for handles of Diffie-Hellman–agreed keys.
CALG_CYLINK_MEK    = $0000660c; // An algorithm to create a 40-bit DES key that has parity bits and zeroed key bits to make its key length 64 bits. This algorithm is supported by the Microsoft Base Cryptographic Provider.

CALG_DES  = $00006601; // DES encryption algorithm.
CALG_DESX = $00006604; // DESX encryption algorithm.

CALG_DH_EPHEM  = $0000aa02; // Diffie-Hellman ephemeral key exchange algorithm.
CALG_DH_SF     = $0000aa01; // Diffie-Hellman store and forward key exchange algorithm.
CALG_DSS_SIGN  = $00002200; // DSA public key signature algorithm.

CALG_ECDH  = $0000aa05; // Elliptical curve Diffie-Hellman key exchange algorithm.
// Note  This algorithm is supported only through Cryptography API: Next Generation.
// Windows Server 2003, Windows XP, and Windows 2000/NT:  This algorithm is not supported.
CALG_ECDSA = $00002203; // Elliptical curve digital signature algorithm.
// Note  This algorithm is supported only through Cryptography API: Next Generation.
// Windows Server 2003, Windows XP, and Windows 2000/NT:  This algorithm is not supported.
CALG_ECMQV = $0000a001; // Elliptical curve Menezes, Qu, and Vanstone (MQV) key exchange algorithm.
// Windows Server 2003, Windows XP, and Windows 2000/NT:  This algorithm is not supported.

CALG_HASH_REPLACE_OWF = $0000800b; // One way function hashing algorithm.
// Windows 2000/NT:  This algorithm is not supported. 

CALG_HUGHES_MD5  = $0000a003; // Hughes MD5 hashing algorithm.
CALG_HMAC        = $00008009; // HMAC keyed hash algorithm. This algorithm is supported by the Microsoft Base Cryptographic Provider.
CALG_KEA_KEYX    = $0000aa04; // KEA key exchange algorithm (FORTEZZA).

CALG_MAC = $00008005; // MAC keyed hash algorithm. This algorithm is supported by the Microsoft Base Cryptographic Provider.
CALG_MD2 = $00008001; // MD2 hashing algorithm. This algorithm is supported by the Microsoft Base Cryptographic Provider.
CALG_MD4 = $00008002; // MD4 hashing algorithm.
CALG_MD5 = $00008003; // MD5 hashing algorithm. This algorithm is supported by the Microsoft Base Cryptographic Provider.

CALG_NO_SIGN = $00002000; // No signature algorithm.
// Windows 2000/NT:  This algorithm is not supported.

CALG_OID_INFO_CNG_ONLY   = $ffffffff; // The algorithm is only implemented in CNG. The macro, IS_SPECIAL_OID_INFO_ALGID, can be used to determine whether a cryptography algorithm is only supported by using the CNG functions.
CALG_OID_INFO_PARAMETERS = $fffffffe; // The algorithm is defined in the encoded parameters. The algorithm is only supported by using CNG. The macro, IS_SPECIAL_OID_INFO_ALGID, can be used to determine whether a cryptography algorithm is only supported by using the CNG functions.

(*
CALG_PCT1_MASTER 0x00004c04 Used by the Schannel.dll operations system. This ALG_ID should not be used by applications.
*)

CALG_RC2 = $00006602; // RC2 block encryption algorithm. This algorithm is supported by the Microsoft Base Cryptographic Provider.
CALG_RC4 = $00006801; // RC4 stream encryption algorithm. This algorithm is supported by the Microsoft Base Cryptographic Provider.
CALG_RC5 = $0000660d; // RC5 block encryption algorithm. 

CALG_RSA_KEYX = $0000a400; // RSA public key exchange algorithm. This algorithm is supported by the Microsoft Base Cryptographic Provider.
CALG_RSA_SIGN = $00002400; // RSA public key signature algorithm. This algorithm is supported by the Microsoft Base Cryptographic Provider.

(*
CALG_SCHANNEL_ENC_KEY 0x00004c07 Used by the Schannel.dll operations system. This ALG_ID should not be used by applications.
CALG_SCHANNEL_MAC_KEY 0x00004c03 Used by the Schannel.dll operations system. This ALG_ID should not be used by applications. 
CALG_SCHANNEL_MASTER_HASH 0x00004c02 Used by the Schannel.dll operations system. This ALG_ID should not be used by applications. 
*)

CALG_SEAL = $00006802; // SEAL encryption algorithm. This algorithm is not supported.

CALG_SHA      = $00008004; // SHA hashing algorithm. This algorithm is supported by the Microsoft Base Cryptographic Provider.
CALG_SHA1     = $00008004; // Same as CALG_SHA. This algorithm is supported by the Microsoft Base Cryptographic Provider.

CALG_SHA_256  = $0000800c; // 256 bit SHA hashing algorithm.
// This algorithm is supported by Microsoft Enhanced RSA and AES Cryptographic Provider..
// Windows XP with SP3:  This algorithm is supported by the Microsoft Enhanced RSA and AES Cryptographic Provider (Prototype).
// Windows XP with SP2, Windows XP with SP1, and Windows XP:  This algorithm is not supported.

CALG_SHA_384  = $0000800d; // 384 bit SHA hashing algorithm.
// This algorithm is supported by Microsoft Enhanced RSA and AES Cryptographic Provider.
// Windows XP with SP3:  This algorithm is supported by the Microsoft Enhanced RSA and AES Cryptographic Provider (Prototype).
// Windows XP with SP2, Windows XP with SP1, and Windows XP:  This algorithm is not supported.

CALG_SHA_512  = $0000800e; // 512 bit SHA hashing algorithm.
// This algorithm is supported by Microsoft Enhanced RSA and AES Cryptographic Provider.
// Windows XP with SP3:  This algorithm is supported by the Microsoft Enhanced RSA and AES Cryptographic Provider (Prototype).
// Windows XP with SP2, Windows XP with SP1, and Windows XP:  This algorithm is not supported.

CALG_SKIPJACK = $0000660a; // Skipjack block encryption algorithm (FORTEZZA). This algorithm is not supported.

(*
CALG_SSL2_MASTER 0x00004c05 Used by the Schannel.dll operations system. This ALG_ID should not be used by applications.
CALG_SSL3_MASTER 0x00004c01 Used by the Schannel.dll operations system. This ALG_ID should not be used by applications. 
CALG_SSL3_SHAMD5 0x00008008 Used by the Schannel.dll operations system. This ALG_ID should not be used by applications. 
*)

CALG_TEK = $0000660b; // TEK (FORTEZZA). This algorithm is not supported.

(*
CALG_TLS1_MASTER 0x00004c06 Used by the Schannel.dll operations system. This ALG_ID should not be used by applications. 
CALG_TLS1PRF 0x0000800a Used by the Schannel.dll operations system. This ALG_ID should not be used by applications. 
*)


  KP_IV = 1; // Initialization vector
  KP_SALT = 2; // Salt value
  KP_PADDING = 3; // Padding values
  KP_MODE = 4; // Mode of the cipher
  KP_MODE_BITS = 5; // Number of bits to feedback
  KP_PERMISSIONS = 6; // Key permissions DWORD
  KP_ALGID = 7; // Key algorithm
  KP_BLOCKLEN = 8; // Block size of the cipher
  KP_KEYLEN = 9; // Length of key in bits
  KP_SALT_EX = 10; // Length of salt in bytes
  KP_P = 11; // DSS/Diffie-Hellman P value
  KP_G = 12; // DSS/Diffie-Hellman G value
  KP_Q = 13; // DSS Q value
  KP_X = 14; // Diffie-Hellman X value
  KP_Y = 15; // Y value
  KP_RA = 16; // Fortezza RA value
  KP_RB = 17; // Fortezza RB value
  KP_INFO = 18; // for putting information into an RSA envelope
  KP_EFFECTIVE_KEYLEN = 19; // setting and getting RC2 effective key length
  KP_SCHANNEL_ALG = 20; // for setting the Secure Channel algorithms
  KP_CLIENT_RANDOM = 21; // for setting the Secure Channel client random data
  KP_SERVER_RANDOM = 22; // for setting the Secure Channel server random data
  KP_RP = 23;
  KP_PRECOMP_MD5 = 24;
  KP_PRECOMP_SHA = 25;
  KP_CERTIFICATE = 26; // for setting Secure Channel certificate data (PCT1)
  KP_CLEAR_KEY = 27; // for setting Secure Channel clear key data (PCT1)
  KP_PUB_EX_LEN = 28;
  KP_PUB_EX_VAL = 29;

implementation

end.
