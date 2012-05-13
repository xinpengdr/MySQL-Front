////////////////////////////////////////////////////////////////////////////////
//
//                        Borland Delphi Runtime Library
//                     Credential Manager API interface unit
//
// Portions created by Microsoft are
// Copyright (c) 2000, Microsoft Corporation
// All Rights Reserved.
//
// The original file is: wincred.h, released 2004-04-15.
// The original Pascal code is: WinCred.pas, released 2004-07-11.
// The initial developer of the Pascal code is: Nico Bendlin <nicode@gmx.net>.
//
// Portions created by Nico Bendlin are
// Copyright (c) 2004 Nico Bendlin.
//
// The contents of this file are used with permission, subject to the Mozilla
// Public License Version 1.1 (the "License"); you may not use this file except
// in compliance with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1.1.html
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
// the specific language governing rights and limitations under the License.
//
// Alternatively, the contents of this file may be used under the terms of
// either the GNU General Public License Version 2 or later (the "GPL"), or
// the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
// in which case the provisions of the GPL or the LGPL are applicable instead
// of those above. If you wish to allow use of your version of this file only
// under the terms of either the GPL or the LGPL, and not to allow others to
// use your version of this file under the terms of the MPL, indicate your
// decision by deleting the provisions above and replace them with the notice
// and other provisions required by the GPL or the LGPL. If you do not delete
// the provisions above, a recipient may use your version of this file under
// the terms of any one of the MPL, the GPL or the LGPL.
//
////////////////////////////////////////////////////////////////////////////////

{.$DEFINE UNICODE}
{$WEAKPACKAGEUNIT}

unit WinCred;


interface


uses
  Windows;

{$HPPEMIT '#include <wownt32.h>'}


//FIXME: [NicoDE] missing type declarations in Windows.pas
type
  ULONG_PTR = Cardinal;
  {$NODEFINE ULONG_PTR}
  LPBYTE = Pointer;    // PByteArray
  {$NODEFINE LPBYTE}
  PCSTR = PAnsiChar;   // const
  {$NODEFINE PCSTR}
  PCWSTR = PWideChar;  // const
  {$NODEFINE PCWSTR}
  PWSTR = PWideChar;
  {$NODEFINE PWSTR}
  PSTR = PAnsiChar;
  {$NODEFINE PSTR}
  PVOID = Pointer;
  {$NODEFINE PVOID}

//FIXME: [NicoDE] doubleref param
type
  PPCredentialW = PPointer;
  PPCredentialA = PPointer;


//
// Ensure PCtxtHandle is defined
//

type
  PSecHandle = ^TSecHandle;
  {$EXTERNALSYM PSecHandle}
  _SecHandle = record
    dwLower: ULONG_PTR;
    dwUpper: ULONG_PTR;
  end;
  {$EXTERNALSYM _SecHandle}
  TSecHandle = _SecHandle;
  SecHandle = _SecHandle;
  {$EXTERNALSYM SecHandle}

type
  PCtxtHandle = PSecHandle;
  {$EXTERNALSYM PCtxtHandle}


//-----------------------------------------------------------------------------
// Macros
//-----------------------------------------------------------------------------

//
// Macro to determine whether CredUIPromptForCredentials should be called upon a failed
//      authentication attempt.
//
// Implemented as a macro so that the caller can delay load credui.dll only if this
//      macro returns TRUE.
//
// Include only status codes that imply the username/password are wrong or that the
//      password is expired.  In the former case, asking for a another username or password
//      is appropriate.  In the later case, we put up a different dialog asking the
//      user to change the password on the server.
//
// Don't include status codes such as ERROR_ACCOUNT_DISABLED, ERROR_ACCOUNT_RESTRICTION,
//      ERROR_ACCOUNT_LOCKED_OUT, ERROR_ACCOUNT_EXPIRED, ERROR_LOGON_TYPE_NOT_GRANTED.
//      For those, the user isn't going to have another account so prompting him
//      won't help.
//
// STATUS_DOWNGRADE_DETECTED is included to handle the case where a corporate laptop
//      is brought to another LAN.  A downgrade attack will indeed be detected,
//      but we want to popup UI to allow the user to connect to resources in the
//      other LAN.
//
// Don't use the CREDUIP_* macros directly.  Their definition is private to credui.dll.
//

function CredUIPIsUserPasswordError(Status: HRESULT): Boolean;
{$NODEFINE CredUIPIsUserPasswordError}
{$HPPEMIT '#define CredUIPIsUserPasswordError CREDUIP_IS_USER_PASSWORD_ERROR'}
function CredUIPIsDowngradeError(Status: HRESULT): Boolean;
{$NODEFINE CredUIPIsDowngradeError}
{$HPPEMIT '#define CredUIPIsDowngradeError CREDUIP_IS_DOWNGRADE_ERROR'}
function CredUIPIsExpiredError(Status: HRESULT): Boolean;
{$NODEFINE CredUIPIsExpiredError}
{$HPPEMIT '#define CredUIPIsExpiredError CREDUIP_IS_EXPIRED_ERROR'}
function CredUIIsAuthenticationError(Status: HRESULT): Boolean;
{$NODEFINE CredUIIsAuthenticationError}
{$HPPEMIT '#define CredUIIsAuthenticationError CREDUI_IS_AUTHENTICATION_ERROR'}
function CredUINoPromptAuthenticationError(Status: HRESULT): Boolean;
{$NODEFINE CredUINoPromptAuthenticationError}
{$HPPEMIT '#define CredUINoPromptAuthenticationError CREDUI_NO_PROMPT_AUTHENTICATION_ERROR'}


//-----------------------------------------------------------------------------
// Structures
//-----------------------------------------------------------------------------

//
// Credential Attribute
//

// Maximum length of the various credential string fields (in characters)
const
  CRED_MAX_STRING_LENGTH = 256;
  {$EXTERNALSYM CRED_MAX_STRING_LENGTH}

// Maximum length of the UserName field.  The worst case is <User>@<DnsDomain>
const
  CRED_MAX_USERNAME_LENGTH = 256 + 1 + 256;
  {$EXTERNALSYM CRED_MAX_USERNAME_LENGTH}

// Maximum length of the TargetName field for CRED_TYPE_GENERIC (in characters)
const
  CRED_MAX_GENERIC_TARGET_NAME_LENGTH = 32767;
  {$EXTERNALSYM CRED_MAX_GENERIC_TARGET_NAME_LENGTH}

// Maximum length of the TargetName field for CRED_TYPE_DOMAIN_* (in characters)
//      Largest one is <DfsRoot>\<DfsShare>
const
  CRED_MAX_DOMAIN_TARGET_NAME_LENGTH = 256 + 1 + 80;
  {$EXTERNALSYM CRED_MAX_DOMAIN_TARGET_NAME_LENGTH}

// Maximum size of the Credential Attribute Value field (in bytes)
const
  CRED_MAX_VALUE_SIZE = 256;
  {$EXTERNALSYM CRED_MAX_VALUE_SIZE}

// Maximum number of attributes per credential
const
  CRED_MAX_ATTRIBUTES = 64;
  {$EXTERNALSYM CRED_MAX_ATTRIBUTES}


type
  PCredentialAttributeA = ^TCredentialAttributeA;
  PCREDENTIAL_ATTRIBUTEA = PCredentialAttributeA;
  {$EXTERNALSYM PCREDENTIAL_ATTRIBUTEA}
  _CREDENTIAL_ATTRIBUTEA = record
    Keyword  : LPSTR;
    Flags    : DWORD;
    ValueSize: DWORD;
    Value    : LPBYTE;
  end;
  {$EXTERNALSYM _CREDENTIAL_ATTRIBUTEA}
  TCredentialAttributeA = _CREDENTIAL_ATTRIBUTEA;
  CREDENTIAL_ATTRIBUTEA = _CREDENTIAL_ATTRIBUTEA;
  {$EXTERNALSYM CREDENTIAL_ATTRIBUTEA}

  PCredentialAttributeW = ^TCredentialAttributeW;
  PCREDENTIAL_ATTRIBUTEW = PCredentialAttributeW;
  {$EXTERNALSYM PCREDENTIAL_ATTRIBUTEW}
  _CREDENTIAL_ATTRIBUTEW = record
    Keyword  : LPWSTR;
    Flags    : DWORD;
    ValueSize: DWORD;  // 0..CRED_MAX_VALUE_SIZE
    Value    : LPBYTE;
  end;
  {$EXTERNALSYM _CREDENTIAL_ATTRIBUTEW}
  TCredentialAttributeW = _CREDENTIAL_ATTRIBUTEW;
  CREDENTIAL_ATTRIBUTEW = _CREDENTIAL_ATTRIBUTEW;
  {$EXTERNALSYM CREDENTIAL_ATTRIBUTEA}

{$IFDEF UNICODE}
  CREDENTIAL_ATTRIBUTE = _CREDENTIAL_ATTRIBUTEW;
  {$EXTERNALSYM CREDENTIAL_ATTRIBUTE}
  TCredentialAttribute = _CREDENTIAL_ATTRIBUTEW;
  PCREDENTIAL_ATTRIBUTE = PCredentialAttributeW;
  {$EXTERNALSYM PCREDENTIAL_ATTRIBUTE}
  PCredentialAttribute = PCredentialAttributeW;
{$ELSE}
  CREDENTIAL_ATTRIBUTE = _CREDENTIAL_ATTRIBUTEA;
  {$EXTERNALSYM CREDENTIAL_ATTRIBUTE}
  TCredentialAttribute = _CREDENTIAL_ATTRIBUTEA;
  PCREDENTIAL_ATTRIBUTE = PCredentialAttributeA;
  {$EXTERNALSYM PCREDENTIAL_ATTRIBUTE}
  PCredentialAttribute = PCredentialAttributeA;
{$ENDIF}

//
// Special values of the TargetName field
//
const
  CRED_SESSION_WILDCARD_NAME_W = WideString('*Session');
  {$EXTERNALSYM CRED_SESSION_WILDCARD_NAME_W}
  CRED_SESSION_WILDCARD_NAME_A = AnsiString('*Session');
  {$EXTERNALSYM CRED_SESSION_WILDCARD_NAME_A}
  CRED_SESSION_WILDCARD_NAME_LENGTH = Length(CRED_SESSION_WILDCARD_NAME_A) - 1;
  {$EXTERNALSYM CRED_SESSION_WILDCARD_NAME_LENGTH}

{$IFDEF UNICODE}
  CRED_SESSION_WILDCARD_NAME = CRED_SESSION_WILDCARD_NAME_W;
  {$EXTERNALSYM CRED_SESSION_WILDCARD_NAME}
{$ELSE}
  CRED_SESSION_WILDCARD_NAME = CRED_SESSION_WILDCARD_NAME_A;
  {$EXTERNALSYM CRED_SESSION_WILDCARD_NAME}
{$ENDIF}

//
// Values of the Credential Flags field.
//
const
  CRED_FLAGS_PASSWORD_FOR_CERT = $0001;
  {$EXTERNALSYM CRED_FLAGS_PASSWORD_FOR_CERT}
  CRED_FLAGS_PROMPT_NOW        = $0002;
  {$EXTERNALSYM CRED_FLAGS_PROMPT_NOW}
  CRED_FLAGS_USERNAME_TARGET   = $0004;
  {$EXTERNALSYM CRED_FLAGS_USERNAME_TARGET}
  CRED_FLAGS_OWF_CRED_BLOB     = $0008;
  {$EXTERNALSYM CRED_FLAGS_OWF_CRED_BLOB}
  CRED_FLAGS_VALID_FLAGS       = $000F;  // Mask of all valid flags
  {$EXTERNALSYM CRED_FLAGS_VALID_FLAGS}

//
// Values of the Credential Type field.
//
const
  CRED_TYPE_GENERIC                 = 1;
  {$EXTERNALSYM CRED_TYPE_GENERIC}
  CRED_TYPE_DOMAIN_PASSWORD         = 2;
  {$EXTERNALSYM CRED_TYPE_DOMAIN_PASSWORD}
  CRED_TYPE_DOMAIN_CERTIFICATE      = 3;
  {$EXTERNALSYM CRED_TYPE_DOMAIN_CERTIFICATE}
  CRED_TYPE_DOMAIN_VISIBLE_PASSWORD = 4;
  {$EXTERNALSYM CRED_TYPE_DOMAIN_VISIBLE_PASSWORD}
  CRED_TYPE_MAXIMUM                 = 5;  // Maximum supported cred type
  {$EXTERNALSYM CRED_TYPE_MAXIMUM}
  CRED_TYPE_MAXIMUM_EX              = CRED_TYPE_MAXIMUM + 1000;  // Allow new applications to run on old OSes
  {$EXTERNALSYM CRED_TYPE_MAXIMUM_EX}

//
// Maximum size of the CredBlob field (in bytes)
//
const
  CRED_MAX_CREDENTIAL_BLOB_SIZE = 512;
  {$EXTERNALSYM CRED_MAX_CREDENTIAL_BLOB_SIZE}

//
// Values of the Credential Persist field
//
const
  CRED_PERSIST_NONE          = 0;
  {$EXTERNALSYM CRED_PERSIST_NONE}
  CRED_PERSIST_SESSION       = 1;
  {$EXTERNALSYM CRED_PERSIST_SESSION}
  CRED_PERSIST_LOCAL_MACHINE = 2;
  {$EXTERNALSYM CRED_PERSIST_LOCAL_MACHINE}
  CRED_PERSIST_ENTERPRISE    = 3;
  {$EXTERNALSYM CRED_PERSIST_ENTERPRISE}


//
// A credential
//
type
  PCredentialA = ^TCredentialA;
  {$HPPEMIT 'typedef PCREDENTIALA PCredentialA;'}
  _CREDENTIALA = record
    Flags             : DWORD;
    Type_             : DWORD;
    TargetName        : LPSTR;
    Comment           : LPSTR;
    LastWritten       : FILETIME;
    CredentialBlobSize: DWORD;
    CredentialBlob    : LPBYTE;
    Persist           : DWORD;
    AttributeCount    : DWORD;
    Attributes        : PCredentialAttributeA;
    TargetAlias       : LPSTR;
    UserName          : LPSTR;
  end;
  {$EXTERNALSYM _CREDENTIALA}
  TCredentialA = _CREDENTIALA;
  CREDENTIALA = _CREDENTIALA;
  {$EXTERNALSYM CREDENTIALA}

  PCredentialW = ^TCredentialW;
  {$HPPEMIT 'typedef PCREDENTIALW PCredentialW;'}
  _CREDENTIALW = record
    Flags             : DWORD;
    Type_             : DWORD;
    TargetName        : LPWSTR;
    Comment           : LPWSTR;
    LastWritten       : FILETIME;
    CredentialBlobSize: DWORD;  // 0..CRED_MAX_CREDENTIAL_BLOB_SIZE
    CredentialBlob    : LPBYTE;
    Persist           : DWORD;
    AttributeCount    : DWORD;
    Attributes        : PCredentialAttributeW;
    TargetAlias       : LPWSTR;
    UserName          : LPWSTR;
  end;
  {$EXTERNALSYM _CREDENTIALW}
  TCredentialW = _CREDENTIALW;
  CREDENTIALW = _CREDENTIALW;
  {$EXTERNALSYM CREDENTIALW}

{$IFDEF UNICODE}
  CREDENTIAL = _CREDENTIALW;
  {$EXTERNALSYM CREDENTIAL}
  TCredential = _CREDENTIALW;
  PCredential = PCredentialW;
  {$HPPEMIT 'typedef PCREDENTIAL PCredentialW'}
{$ELSE}
  CREDENTIAL = _CREDENTIALA;
  {$EXTERNALSYM CREDENTIAL}
  TCredential = _CREDENTIALA;
  PCredential = PCredentialA;
  {$HPPEMIT 'typedef PCREDENTIAL PCredentialA'}
{$ENDIF}


//
// Value of the Flags field in CREDENTIAL_TARGET_INFORMATION
//
const
  CRED_TI_SERVER_FORMAT_UNKNOWN  = $0001;  // Don't know if server name is DNS or netbios format
  {$EXTERNALSYM CRED_TI_SERVER_FORMAT_UNKNOWN}
  CRED_TI_DOMAIN_FORMAT_UNKNOWN  = $0002;  // Don't know if domain name is DNS or netbios format
  {$EXTERNALSYM CRED_TI_DOMAIN_FORMAT_UNKNOWN}
  CRED_TI_ONLY_PASSWORD_REQUIRED = $0004;  // Server only requires a password and not a username
  {$EXTERNALSYM CRED_TI_ONLY_PASSWORD_REQUIRED}
  CRED_TI_USERNAME_TARGET        = $0008;  // TargetName is username
  {$EXTERNALSYM CRED_TI_USERNAME_TARGET}
  CRED_TI_CREATE_EXPLICIT_CRED   = $0010;  // When creating a cred, create one named TargetInfo->TargetName
  {$EXTERNALSYM CRED_TI_CREATE_EXPLICIT_CRED}
  CRED_TI_WORKGROUP_MEMBER       = $0020;  // Indicates the machine is a member of a workgroup
  {$EXTERNALSYM CRED_TI_WORKGROUP_MEMBER}
  CRED_TI_VALID_FLAGS            = $003F;
  {$EXTERNALSYM CRED_TI_VALID_FLAGS}


//
// A credential target
//

type
  PCredentialTargetInformationA = ^TCredentialTargetInformationA;
  PCREDENTIAL_TARGET_INFORMATIONA = PCredentialTargetInformationA;
  {$EXTERNALSYM PCREDENTIAL_TARGET_INFORMATIONA}
  _CREDENTIAL_TARGET_INFORMATIONA = record
    TargetName       : LPSTR;
    NetbiosServerName: LPSTR;
    DnsServerName    : LPSTR;
    NetbiosDomainName: LPSTR;
    DnsDomainName    : LPSTR;
    DnsTreeName      : LPSTR;
    PackageName      : LPSTR;
    Flags            : ULONG;
    CredTypeCount    : DWORD;
    CredTypes        : LPDWORD;
  end;
  {$EXTERNALSYM _CREDENTIAL_TARGET_INFORMATIONA}
  TCredentialTargetInformationA = _CREDENTIAL_TARGET_INFORMATIONA;
  CREDENTIAL_TARGET_INFORMATIONA = _CREDENTIAL_TARGET_INFORMATIONA;
  {$EXTERNALSYM CREDENTIAL_TARGET_INFORMATIONA}

  PCredentialTargetInformationW = ^TCredentialTargetInformationW;
  PCREDENTIAL_TARGET_INFORMATIONW = PCredentialTargetInformationW;
  {$EXTERNALSYM PCREDENTIAL_TARGET_INFORMATIONA}
  _CREDENTIAL_TARGET_INFORMATIONW = record
    TargetName       : LPSTR;
    NetbiosServerName: LPSTR;
    DnsServerName    : LPSTR;
    NetbiosDomainName: LPSTR;
    DnsDomainName    : LPSTR;
    DnsTreeName      : LPSTR;
    PackageName      : LPSTR;
    Flags            : ULONG;
    CredTypeCount    : DWORD;  // 0..CRED_TYPE_MAXIMUM_EX
    CredTypes        : LPDWORD;
  end;
  {$EXTERNALSYM _CREDENTIAL_TARGET_INFORMATIONW}
  TCredentialTargetInformationW = _CREDENTIAL_TARGET_INFORMATIONW;
  CREDENTIAL_TARGET_INFORMATIONW = _CREDENTIAL_TARGET_INFORMATIONW;
  {$EXTERNALSYM CREDENTIAL_TARGET_INFORMATIONW}

{$IFDEF UNICODE}
  CREDENTIAL_TARGET_INFORMATION = _CREDENTIAL_TARGET_INFORMATIONW;
  {$EXTERNALSYM CREDENTIAL_TARGET_INFORMATION}
  TCredentialTargetInformation = _CREDENTIAL_TARGET_INFORMATIONW;
  PCREDENTIAL_TARGET_INFORMATION = PCredentialTargetInformationW;
  {$EXTERNALSYM PCREDENTIAL_TARGET_INFORMATION}
  PCredentialTargetInformation = PCredentialTargetInformationW;
{$ELSE}
  CREDENTIAL_TARGET_INFORMATION = _CREDENTIAL_TARGET_INFORMATIONA;
  {$EXTERNALSYM CREDENTIAL_TARGET_INFORMATION}
  TCredentialTargetInformation = _CREDENTIAL_TARGET_INFORMATIONA;
  PCREDENTIAL_TARGET_INFORMATION = PCredentialTargetInformationA;
  {$EXTERNALSYM PCREDENTIAL_TARGET_INFORMATION}
  PCredentialTargetInformation = PCredentialTargetInformationA;
{$ENDIF}

//
// Certificate credential information
//
// The cbSize should be the size of the structure, sizeof(CERT_CREDENTIAL_INFO),
// rgbHashofCert is the hash of the cert which is to be used as the credential.
//

const
  CERT_HASH_LENGTH = 20;  // SHA1 hashes are used for cert hashes
  {$EXTERNALSYM CERT_HASH_LENGTH}

type
  PCertCredentialInfo = ^TCertCredentialInfo;
  PCERT_CREDENTIAL_INFO = PCertCredentialInfo;
  {$EXTERNALSYM PCERT_CREDENTIAL_INFO}
  _CERT_CREDENTIAL_INFO = record
    cbSize       : ULONG;
    rgbHashOfCert: array [0..CERT_HASH_LENGTH-1] of UCHAR;
  end;
  {$EXTERNALSYM _CERT_CREDENTIAL_INFO}
  TCertCredentialInfo = _CERT_CREDENTIAL_INFO;
  CERT_CREDENTIAL_INFO = _CERT_CREDENTIAL_INFO;
  {$EXTERNALSYM CERT_CREDENTIAL_INFO}

//
// Username Target credential information
//
// This credential can be pass to LsaLogonUser to ask it to find a credential with a
// TargetName of UserName.
//

type
  PUsernameTargetCredentialInfo = ^TUsernameTargetCredentialInfo;
  PUSERNAME_TARGET_CREDENTIAL_INFO = PUsernameTargetCredentialInfo;
  {$EXTERNALSYM PUSERNAME_TARGET_CREDENTIAL_INFO}
  _USERNAME_TARGET_CREDENTIAL_INFO = record
    UserName: LPWSTR;
  end;
  {$EXTERNALSYM _USERNAME_TARGET_CREDENTIAL_INFO}
  TUsernameTargetCredentialInfo = _USERNAME_TARGET_CREDENTIAL_INFO;
  USERNAME_TARGET_CREDENTIAL_INFO = _USERNAME_TARGET_CREDENTIAL_INFO;
  {$EXTERNALSYM USERNAME_TARGET_CREDENTIAL_INFO}

//
// Credential type for credential marshaling routines
//

type
  _CRED_MARSHAL_TYPE = Longint;  // enum
  {$EXTERNALSYM _CRED_MARSHAL_TYPE}
  TCredMarshalType = _CRED_MARSHAL_TYPE;
  CRED_MARSHAL_TYPE = _CRED_MARSHAL_TYPE;
  {$EXTERNALSYM CRED_MARSHAL_TYPE}
  PCredMarshalType = ^TCredMarshalType;
  PCRED_MARSHAL_TYPE = PCredMarshalType;
  {$EXTERNALSYM PCRED_MARSHAL_TYPE}
const
  CertCredential           = TCredMarshalType(1);
  {$EXTERNALSYM CertCredential}
  UsernameTargetCredential = TCredMarshalType(CertCredential + 1);
  {$EXTERNALSYM UsernameTargetCredential}


//
// Credential UI info
//

type
  PCredUIInfoA = ^TCredUIInfoA;
  PCREDUI_INFOA = PCredUIInfoA;
  {$EXTERNALSYM PCREDUI_INFOA}
  _CREDUI_INFOA = record
    cbSize        : DWORD;
    hwndParent    : HWND;
    pszMessageText: PCSTR;
    pszCaptionText: PCSTR;
    hbmBanner     : HBITMAP;
  end;
  {$EXTERNALSYM _CREDUI_INFOA}
  TCredUIInfoA = _CREDUI_INFOA;
  CREDUI_INFOA = _CREDUI_INFOA;
  {$EXTERNALSYM CREDUI_INFOA}

  PCredUIInfoW = ^TCredUIInfoW;
  PCREDUI_INFOW = PCredUIInfoW;
  {$EXTERNALSYM PCREDUI_INFOW}
  _CREDUI_INFOW = record
    cbSize        : DWORD;
    hwndParent    : HWND;
    pszMessageText: PCWSTR;
    pszCaptionText: PCWSTR;
    hbmBanner     : HBITMAP;
  end;
  {$EXTERNALSYM _CREDUI_INFOW}
  TCredUIInfoW = _CREDUI_INFOW;
  CREDUI_INFOW = _CREDUI_INFOW;
  {$EXTERNALSYM CREDUI_INFOW}

{$IFDEF UNICODE}
  CREDUI_INFO = _CREDUI_INFOW;
  {$EXTERNALSYM CREDUI_INFO}
  TCredUIInfo = _CREDUI_INFOW;
  PCREDUI_INFO = PCredUIInfoW;
  {$EXTERNALSYM PCREDUI_INFO}
  PCredUIInfo = PCredUIInfoW;
{$ELSE}
  CREDUI_INFO = _CREDUI_INFOA;
  {$EXTERNALSYM CREDUI_INFO}
  TCredUIInfo = _CREDUI_INFOA;
  PCREDUI_INFO = PCredUIInfoA;
  {$EXTERNALSYM PCREDUI_INFO}
  PCredUIInfo = PCredUIInfoA;
{$ENDIF}

//-----------------------------------------------------------------------------
// Values
//-----------------------------------------------------------------------------

// String length limits:
const
  CREDUI_MAX_MESSAGE_LENGTH        = 32767;
  {$EXTERNALSYM CREDUI_MAX_MESSAGE_LENGTH}
  CREDUI_MAX_CAPTION_LENGTH        = 128;
  {$EXTERNALSYM CREDUI_MAX_CAPTION_LENGTH}
  CREDUI_MAX_GENERIC_TARGET_LENGTH = CRED_MAX_GENERIC_TARGET_NAME_LENGTH;
  {$EXTERNALSYM CREDUI_MAX_GENERIC_TARGET_LENGTH}
  CREDUI_MAX_DOMAIN_TARGET_LENGTH  = CRED_MAX_DOMAIN_TARGET_NAME_LENGTH;
  {$EXTERNALSYM CREDUI_MAX_DOMAIN_TARGET_LENGTH}
  CREDUI_MAX_USERNAME_LENGTH       = CRED_MAX_USERNAME_LENGTH;
  {$EXTERNALSYM CREDUI_MAX_USERNAME_LENGTH}
  CREDUI_MAX_PASSWORD_LENGTH       = CRED_MAX_CREDENTIAL_BLOB_SIZE div 2;
  {$EXTERNALSYM CREDUI_MAX_PASSWORD_LENGTH}

//
// Flags for CredUIPromptForCredentials and/or CredUICmdLinePromptForCredentials
//
const
  CREDUI_FLAGS_INCORRECT_PASSWORD          = $00001;   // indicates the username is valid, but password is not
  {$EXTERNALSYM CREDUI_FLAGS_INCORRECT_PASSWORD}
  CREDUI_FLAGS_DO_NOT_PERSIST              = $00002;   // Do not show "Save" checkbox, and do not persist credentials
  {$EXTERNALSYM CREDUI_FLAGS_DO_NOT_PERSIST}
  CREDUI_FLAGS_REQUEST_ADMINISTRATOR       = $00004;   // Populate list box with admin accounts
  {$EXTERNALSYM CREDUI_FLAGS_REQUEST_ADMINISTRATOR}
  CREDUI_FLAGS_EXCLUDE_CERTIFICATES        = $00008;   // do not include certificates in the drop list
  {$EXTERNALSYM CREDUI_FLAGS_EXCLUDE_CERTIFICATES}
  CREDUI_FLAGS_REQUIRE_CERTIFICATE         = $00010;
  {$EXTERNALSYM CREDUI_FLAGS_REQUIRE_CERTIFICATE}
  CREDUI_FLAGS_SHOW_SAVE_CHECK_BOX         = $00040;
  {$EXTERNALSYM CREDUI_FLAGS_SHOW_SAVE_CHECK_BOX}
  CREDUI_FLAGS_ALWAYS_SHOW_UI              = $00080;
  {$EXTERNALSYM CREDUI_FLAGS_ALWAYS_SHOW_UI}
  CREDUI_FLAGS_REQUIRE_SMARTCARD           = $00100;
  {$EXTERNALSYM CREDUI_FLAGS_REQUIRE_SMARTCARD}
  CREDUI_FLAGS_PASSWORD_ONLY_OK            = $00200;
  {$EXTERNALSYM CREDUI_FLAGS_PASSWORD_ONLY_OK}
  CREDUI_FLAGS_VALIDATE_USERNAME           = $00400;
  {$EXTERNALSYM CREDUI_FLAGS_VALIDATE_USERNAME}
  CREDUI_FLAGS_COMPLETE_USERNAME           = $00800;
  {$EXTERNALSYM CREDUI_FLAGS_COMPLETE_USERNAME}
  CREDUI_FLAGS_PERSIST                     = $01000;   // Do not show "Save" checkbox, but persist credentials anyway
  {$EXTERNALSYM CREDUI_FLAGS_PERSIST}
  CREDUI_FLAGS_SERVER_CREDENTIAL           = $04000;
  {$EXTERNALSYM CREDUI_FLAGS_SERVER_CREDENTIAL}
  CREDUI_FLAGS_EXPECT_CONFIRMATION         = $20000;   // do not persist unless caller later confirms credential via CredUIConfirmCredential() api
  {$EXTERNALSYM CREDUI_FLAGS_EXPECT_CONFIRMATION}
  CREDUI_FLAGS_GENERIC_CREDENTIALS         = $40000;   // Credential is a generic credential
  {$EXTERNALSYM CREDUI_FLAGS_GENERIC_CREDENTIALS}
  CREDUI_FLAGS_USERNAME_TARGET_CREDENTIALS = $80000;   // Credential has a username as the target
  {$EXTERNALSYM CREDUI_FLAGS_USERNAME_TARGET_CREDENTIALS}
  CREDUI_FLAGS_KEEP_USERNAME               = $100000;  // don't allow the user to change the supplied username
  {$EXTERNALSYM CREDUI_FLAGS_KEEP_USERNAME}


//
// Mask of flags valid for CredUIPromptForCredentials
//
const
  CREDUI_FLAGS_PROMPT_VALID =
    CREDUI_FLAGS_INCORRECT_PASSWORD          or
    CREDUI_FLAGS_DO_NOT_PERSIST              or
    CREDUI_FLAGS_REQUEST_ADMINISTRATOR       or
    CREDUI_FLAGS_EXCLUDE_CERTIFICATES        or
    CREDUI_FLAGS_REQUIRE_CERTIFICATE         or
    CREDUI_FLAGS_SHOW_SAVE_CHECK_BOX         or
    CREDUI_FLAGS_ALWAYS_SHOW_UI              or
    CREDUI_FLAGS_REQUIRE_SMARTCARD           or
    CREDUI_FLAGS_PASSWORD_ONLY_OK            or
    CREDUI_FLAGS_VALIDATE_USERNAME           or
    CREDUI_FLAGS_COMPLETE_USERNAME           or
    CREDUI_FLAGS_PERSIST                     or
    CREDUI_FLAGS_SERVER_CREDENTIAL           or
    CREDUI_FLAGS_EXPECT_CONFIRMATION         or
    CREDUI_FLAGS_GENERIC_CREDENTIALS         or
    CREDUI_FLAGS_USERNAME_TARGET_CREDENTIALS or
    CREDUI_FLAGS_KEEP_USERNAME;
  {$EXTERNALSYM CREDUI_FLAGS_PROMPT_VALID}

//-----------------------------------------------------------------------------
// Functions
//-----------------------------------------------------------------------------


//
// Values of flags to CredWrite and CredWriteDomainCredentials
//

const
  CRED_PRESERVE_CREDENTIAL_BLOB = $1;
  {$EXTERNALSYM CRED_PRESERVE_CREDENTIAL_BLOB}

function CredWriteW(Credential: PCredentialW; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredWriteW}
function CredWriteA(Credential: PCredentialA; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredWriteA}
{$IFDEF UNICODE}
function CredWrite(Credential: PCredentialW; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredWrite}
{$ELSE}
function CredWrite(Credential: PCredentialA; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredWrite}
{$ENDIF}

function CredReadW(TargetName: LPCWSTR; Type_, Flags: DWORD;
  out Credential: PCredentialW): BOOL; stdcall;
{$EXTERNALSYM CredReadW}
function CredReadA(TargetName: LPCSTR; Type_, Flags: DWORD;
  out Credential: PCredentialA): BOOL; stdcall;
{$EXTERNALSYM CredReadA}
{$IFDEF UNICODE}
function CredRead(TargetName: LPCWSTR; Type_, Flags: DWORD;
  out Credential: PCredentialW): BOOL; stdcall;
{$EXTERNALSYM CredRead}
{$ELSE}
function CredRead(TargetName: LPCSTR; Type_, Flags: DWORD;
  out Credential: PCredentialA): BOOL; stdcall;
{$EXTERNALSYM CredRead}
{$ENDIF}

function CredEnumerateW(Filter: LPCWSTR; Flags: DWORD; out Count: DWORD;
  out Credential: PPCredentialW): BOOL; stdcall;
{$EXTERNALSYM CredEnumerateW}
function CredEnumerateA(Filter: LPCSTR; Flags: DWORD; out Count: DWORD;
  out Credential: PPCredentialA): BOOL; stdcall;
{$EXTERNALSYM CredEnumerateA}
{$IFDEF UNICODE}
function CredEnumerate(Filter: LPCWSTR; Flags: DWORD; out Count: DWORD;
  out Credential: PPCredentialW): BOOL; stdcall;
{$EXTERNALSYM CredEnumerate}
{$ELSE}
function CredEnumerate(Filter: LPCSTR; Flags: DWORD; out Count: DWORD;
  out Credential: PPCredentialA): BOOL; stdcall;
{$EXTERNALSYM CredEnumerate}
{$ENDIF}

function CredWriteDomainCredentialsW(TargetInfo: PCredentialTargetInformationW;
  Credential: PCredentialW; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredWriteDomainCredentialsW}
function CredWriteDomainCredentialsA(TargetInfo: PCredentialTargetInformationA;
  Credential: PCredentialA; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredWriteDomainCredentialsA}
{$IFDEF UNICODE}
function CredWriteDomainCredentials(TargetInfo: PCredentialTargetInformationW;
  Credential: PCredentialW; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredWriteDomainCredentials}
{$ELSE}
function CredWriteDomainCredentials(TargetInfo: PCredentialTargetInformationA;
  Credential: PCredentialA; Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredWriteDomainCredentials}
{$ENDIF}

//
// Values of flags to CredReadDomainCredentials
//

const
  CRED_CACHE_TARGET_INFORMATION = $1;
  {$EXTERNALSYM CRED_CACHE_TARGET_INFORMATION}

function CredReadDomainCredentialsW(TargetInfo: PCredentialTargetInformationW;
  Flags: DWORD; out Count: DWORD; out Credential: PPCredentialW): BOOL; stdcall;
{$EXTERNALSYM CredReadDomainCredentialsW}
function CredReadDomainCredentialsA(TargetInfo: PCredentialTargetInformationA;
  Flags: DWORD; out Count: DWORD; out Credential: PPCredentialA): BOOL; stdcall;
{$EXTERNALSYM CredReadDomainCredentialsA}
{$IFDEF UNICODE}
function CredReadDomainCredentials(TargetInfo: PCredentialTargetInformationW;
  Flags: DWORD; out Count: DWORD; out Credential: PPCredentialW): BOOL; stdcall;
{$EXTERNALSYM CredReadDomainCredentials}
{$ELSE}
function CredReadDomainCredentials(TargetInfo: PCredentialTargetInformationA;
  Flags: DWORD; out Count: DWORD; out Credential: PPCredentialA): BOOL; stdcall;
{$EXTERNALSYM CredReadDomainCredentials}
{$ENDIF}

function CredDeleteW(TargetName: LPCWSTR; Type_, Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredDeleteW}
function CredDeleteA(TargetName: LPCSTR; Type_, Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredDeleteA}
{$IFDEF UNICODE}
function CredDelete(TargetName: LPCWSTR; Type_, Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredDelete}
{$ELSE}
function CredDelete(TargetName: LPCSTR; Type_, Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredDelete}
{$ENDIF}

function CredRenameW(OldTargetName, NewTargetName: LPCWSTR;
  Type_, Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredRenameW}
function CredRenameA(OldTargetName, NewTargetName: LPCSTR;
  Type_, Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredRenameA}
{$IFDEF UNICODE}
function CredRename(OldTargetName, NewTargetName: LPCWSTR;
  Type_, Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredRename}
{$ELSE}
function CredRename(OldTargetName, NewTargetName: LPCSTR;
  Type_, Flags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CredRename}
{$ENDIF}

//
// Values of flags to CredGetTargetInfo
//

const
  CRED_ALLOW_NAME_RESOLUTION = $1;
  {$EXTERNALSYM CRED_ALLOW_NAME_RESOLUTION}

function CredGetTargetInfoW(TargetName: LPCWSTR; Flags: DWORD;
  out TargetInfo: PCredentialTargetInformationW): BOOL; stdcall;
{$EXTERNALSYM CredGetTargetInfoW}
function CredGetTargetInfoA(TargetName: LPCSTR; Flags: DWORD;
  out TargetInfo: PCredentialTargetInformationA): BOOL; stdcall;
{$EXTERNALSYM CredGetTargetInfoA}
{$IFDEF UNICODE}
function CredGetTargetInfo(TargetName: LPCWSTR; Flags: DWORD;
  out TargetInfo: PCredentialTargetInformationW): BOOL; stdcall;
{$EXTERNALSYM CredGetTargetInfo}
{$ELSE}
function CredGetTargetInfo(TargetName: LPCSTR; Flags: DWORD;
  out TargetInfo: PCredentialTargetInformationA): BOOL; stdcall;
{$EXTERNALSYM CredGetTargetInfo}
{$ENDIF}

function CredMarshalCredentialW(CredType: TCredMarshalType; Credential: PVOID;
  out MarshaledCredential: LPWSTR): BOOL; stdcall;
{$EXTERNALSYM CredMarshalCredentialW}
function CredMarshalCredentialA(CredType: TCredMarshalType; Credential: PVOID;
  out MarshaledCredential: LPSTR): BOOL; stdcall;
{$EXTERNALSYM CredMarshalCredentialA}
{$IFDEF UNICODE}
function CredMarshalCredential(CredType: TCredMarshalType; Credential: PVOID;
  out MarshaledCredential: LPWSTR): BOOL; stdcall;
{$EXTERNALSYM CredMarshalCredential}
{$ELSE}
function CredMarshalCredential(CredType: TCredMarshalType; Credential: PVOID;
  out MarshaledCredential: LPSTR): BOOL; stdcall;
{$EXTERNALSYM CredMarshalCredential}
{$ENDIF}

function CredUnmarshalCredentialW(MarshaledCredential: LPCWSTR;
  CredType: PCredMarshalType; out Credential: PVOID): BOOL; stdcall;
{$EXTERNALSYM CredUnmarshalCredentialW}
function CredUnmarshalCredentialA(MarshaledCredential: LPCSTR;
  CredType: PCredMarshalType; out Credential: PVOID): BOOL; stdcall;
{$EXTERNALSYM CredUnmarshalCredentialA}
{$IFDEF UNICODE}
function CredUnmarshalCredential(MarshaledCredential: LPCWSTR;
  CredType: PCredMarshalType; out Credential: PVOID): BOOL; stdcall;
{$EXTERNALSYM CredUnmarshalCredential}
{$ELSE}
function CredUnmarshalCredential(MarshaledCredential: LPCSTR;
  CredType: PCredMarshalType; out Credential: PVOID): BOOL; stdcall;
{$EXTERNALSYM CredUnmarshalCredential}
{$ENDIF}

function CredIsMarshaledCredentialW(MarshaledCredential: LPCWSTR): BOOL; stdcall;
{$EXTERNALSYM CredIsMarshaledCredentialW}
function CredIsMarshaledCredentialA(MarshaledCredential: LPCSTR): BOOL; stdcall;
{$EXTERNALSYM CredIsMarshaledCredentialA}
{$IFDEF UNICODE}
function CredIsMarshaledCredential(MarshaledCredential: LPCWSTR): BOOL; stdcall;
{$EXTERNALSYM CredIsMarshaledCredential}
{$ELSE}
function CredIsMarshaledCredential(MarshaledCredential: LPCSTR): BOOL; stdcall;
{$EXTERNALSYM CredIsMarshaledCredential}
{$ENDIF}

function CredGetSessionTypes(MaximumPersistCount: DWORD;
  MaximumPersist: LPDWORD): BOOL; stdcall;
{$EXTERNALSYM CredGetSessionTypes}
procedure CredFree(Buffer: PVOID); stdcall;
{$EXTERNALSYM CredFree}


function CredUIPromptForCredentialsW(pUiInfo: PCredUIInfoW;
  pszTargetName: PCWSTR; pContext: PCtxtHandle; dwAuthError: DWORD;
  pszUserName: PWSTR; ulUserNameBufferSize: ULONG; pszPassword: PWSTR;
  ulPasswordBufferSize: ULONG; var save: BOOL; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM CredUIPromptForCredentialsW}
function CredUIPromptForCredentialsA(pUiInfo: PCredUIInfoA;
  pszTargetName: PCSTR; pContext: PCtxtHandle; dwAuthError: DWORD;
  pszUserName: PSTR; ulUserNameBufferSize: ULONG; pszPassword: PSTR;
  ulPasswordBufferSize: ULONG; var save: BOOL; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM CredUIPromptForCredentialsA}
{$IFDEF UNICODE}
function CredUIPromptForCredentials(pUiInfo: PCredUIInfoW;
  pszTargetName: PCWSTR; pContext: PCtxtHandle; dwAuthError: DWORD;
  pszUserName: PWSTR; ulUserNameBufferSize: ULONG; pszPassword: PWSTR;
  ulPasswordBufferSize: ULONG; var save: BOOL; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM CredUIPromptForCredentials}
{$ELSE}
function CredUIPromptForCredentials(pUiInfo: PCredUIInfoA;
  pszTargetName: PCSTR; pContext: PCtxtHandle; dwAuthError: DWORD;
  pszUserName: PSTR; ulUserNameBufferSize: ULONG; pszPassword: PSTR;
  ulPasswordBufferSize: ULONG; var save: BOOL; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM CredUIPromptForCredentials}
{$ENDIF}

function CredUIParseUserNameW(pszUserName: PCWSTR; pszUser: PWSTR;
  ulUserBufferSize: ULONG; pszDomain: PWSTR; ulDomainBufferSize: ULONG): DWORD; stdcall;
{$EXTERNALSYM CredUIParseUserNameW}
function CredUIParseUserNameA(pszUserName: PCSTR; pszUser: PSTR;
  ulUserBufferSize: ULONG; pszDomain: PSTR; ulDomainBufferSize: ULONG): DWORD; stdcall;
  {$EXTERNALSYM CredUIParseUserNameA}
{$IFDEF UNICODE}
function CredUIParseUserName(pszUserName: PCWSTR; pszUser: PWSTR;
  ulUserBufferSize: ULONG; pszDomain: PWSTR; ulDomainBufferSize: ULONG): DWORD; stdcall;
{$EXTERNALSYM CredUIParseUserName}
{$ELSE}
function CredUIParseUserName(pszUserName: PCSTR; pszUser: PSTR;
  ulUserBufferSize: ULONG; pszDomain: PSTR; ulDomainBufferSize: ULONG): DWORD; stdcall;
{$EXTERNALSYM CredUIParseUserName}
{$ENDIF}

function CredUICmdLinePromptForCredentialsW(pszTargetName: PCWSTR;
  pContext: PCtxtHandle; dwAuthError: DWORD; UserName: PWSTR;
  ulUserBufferSize: ULONG; pszPassword: PWSTR; ulPasswordBufferSize: ULONG;
  pfSave: PBOOL; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM CredUICmdLinePromptForCredentialsW}
function CredUICmdLinePromptForCredentialsA(pszTargetName: PCSTR;
  pContext: PCtxtHandle; dwAuthError: DWORD; UserName: PSTR;
  ulUserBufferSize: ULONG; pszPassword: PSTR; ulPasswordBufferSize: ULONG;
  pfSave: PBOOL; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM CredUICmdLinePromptForCredentialsA}
{$IFDEF UNICODE}
function CredUICmdLinePromptForCredentials(pszTargetName: PCWSTR;
  pContext: PCtxtHandle; dwAuthError: DWORD; UserName: PWSTR;
  ulUserBufferSize: ULONG; pszPassword: PWSTR; ulPasswordBufferSize: ULONG;
  pfSave: PBOOL; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM CredUICmdLinePromptForCredentials}
{$ELSE}
function CredUICmdLinePromptForCredentials(pszTargetName: PCSTR;
  pContext: PCtxtHandle; dwAuthError: DWORD; UserName: PSTR;
  ulUserBufferSize: ULONG; pszPassword: PSTR; ulPasswordBufferSize: ULONG;
  pfSave: PBOOL; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM CredUICmdLinePromptForCredentials}
{$ENDIF}

//
// Call this API with bConfirm set to TRUE to confirm that the credential (previously created
// via CredUIGetCredentials or CredUIPromptForCredentials worked, or with bConfirm set to FALSE
// to indicate it didn't

function CredUIConfirmCredentialsW(pszTargetName: PCWSTR; bConfirm: BOOL): DWORD; stdcall;
{$EXTERNALSYM CredUIConfirmCredentialsW}
function CredUIConfirmCredentialsA(pszTargetName: PCSTR; bConfirm: BOOL): DWORD; stdcall;
{$EXTERNALSYM CredUIConfirmCredentialsA}
{$IFDEF UNICODE}
function CredUIConfirmCredentials(pszTargetName: PCWSTR; bConfirm: BOOL): DWORD; stdcall;
{$EXTERNALSYM CredUIConfirmCredentials}
{$ELSE}
function CredUIConfirmCredentials(pszTargetName: PCSTR; bConfirm: BOOL): DWORD; stdcall;
{$EXTERNALSYM CredUIConfirmCredentials}
{$ENDIF}

function CredUIStoreSSOCredW(pszRealm, pszUsername, pszPassword: PCWSTR;
  bPersist: BOOL): DWORD; stdcall;
{$EXTERNALSYM CredUIStoreSSOCredW}
function CredUIReadSSOCredW(pszRealm: PCWSTR; var ppszUsername: PWSTR): DWORD; stdcall;
{$EXTERNALSYM CredUIReadSSOCredW}


implementation


function CredUIPIsUserPasswordError(Status: HRESULT): Boolean;
type
  NTSTATUS = Longint;
const
  STATUS_LOGON_FAILURE  = NTSTATUS($C000006D);
  STATUS_ACCESS_DENIED  = NTSTATUS($C0000022);
  STATUS_WRONG_PASSWORD = NTSTATUS($C000006A);
  SEC_E_NO_CREDENTIALS = HRESULT($8009030E);
  SEC_E_LOGON_DENIED   = HRESULT($8009030C);
begin
  case Status of
    HRESULT(ERROR_LOGON_FAILURE),
    HRESULT(ERROR_LOGON_FAILURE and $0000FFFF or FACILITY_WIN32 shl 16 or $80000000),
    HRESULT(STATUS_LOGON_FAILURE),
    HRESULT(STATUS_LOGON_FAILURE or FACILITY_NT_BIT),
    HRESULT(ERROR_ACCESS_DENIED),
    HRESULT(ERROR_ACCESS_DENIED and $0000FFFF or FACILITY_WIN32 shl 16 or $80000000),
    HRESULT(STATUS_ACCESS_DENIED),
    HRESULT(STATUS_ACCESS_DENIED or FACILITY_NT_BIT),
    HRESULT(ERROR_INVALID_PASSWORD),
    HRESULT(ERROR_INVALID_PASSWORD and $0000FFFF or FACILITY_WIN32 shl 16 or $80000000),
    HRESULT(STATUS_WRONG_PASSWORD),
    HRESULT(STATUS_WRONG_PASSWORD or FACILITY_NT_BIT),
    HRESULT(SEC_E_NO_CREDENTIALS),
    HRESULT(SEC_E_LOGON_DENIED):
      Result := True;
  else
    Result := False;
  end;
end;

function CredUIPIsDowngradeError(Status: HRESULT): Boolean;
type
  NTSTATUS = Longint;
const
  STATUS_DOWNGRADE_DETECTED = NTSTATUS($C0000388);
  ERROR_DOWNGRADE_DETECTED = 1265;
begin
  case Status of
    HRESULT(ERROR_DOWNGRADE_DETECTED),
    HRESULT(ERROR_DOWNGRADE_DETECTED and $0000FFFF or FACILITY_WIN32 shl 16 or $80000000),
    HRESULT(STATUS_DOWNGRADE_DETECTED),
    HRESULT(STATUS_DOWNGRADE_DETECTED or FACILITY_NT_BIT):
      Result := True;
  else
    Result := False;
  end;
end;

function CredUIPIsExpiredError(Status: HRESULT): Boolean;
type
  NTSTATUS = Longint;
const
  STATUS_PASSWORD_EXPIRED     = NTSTATUS($C0000071);
  STATUS_PASSWORD_MUST_CHANGE = NTSTATUS($C0000224);
  NERR_BASE            = 2100;
  NERR_PasswordExpired = NERR_BASE + 142;
begin
  case Status of
    HRESULT(ERROR_PASSWORD_EXPIRED),
    HRESULT(ERROR_PASSWORD_EXPIRED and $0000FFFF or FACILITY_WIN32 shl 16 or $80000000),
    HRESULT(STATUS_PASSWORD_EXPIRED),
    HRESULT(STATUS_PASSWORD_EXPIRED or FACILITY_NT_BIT),
    HRESULT(ERROR_PASSWORD_MUST_CHANGE),
    HRESULT(ERROR_PASSWORD_MUST_CHANGE and $0000FFFF or FACILITY_WIN32 shl 16 or $80000000),
    HRESULT(STATUS_PASSWORD_MUST_CHANGE),
    HRESULT(STATUS_PASSWORD_MUST_CHANGE or FACILITY_NT_BIT),
    HRESULT(NERR_PasswordExpired),
    HRESULT(NERR_PasswordExpired and $0000FFFF or FACILITY_WIN32 shl 16 or $80000000):
      Result := True;
  else
    Result := False;
  end;
end;

function CredUIIsAuthenticationError(Status: HRESULT): Boolean;
begin
  Result :=
    CredUIPIsUserPasswordError(Status) or
    CredUIPIsDowngradeError(Status) or
    CredUIPIsExpiredError(Status);
end;

function CredUINoPromptAuthenticationError(Status: HRESULT): Boolean;
type
  NTSTATUS = Longint;
const
  ERROR_AUTHENTICATION_FIREWALL_FAILED = 1935;
  STATUS_AUTHENTICATION_FIREWALL_FAILED = NTSTATUS($C0000413);
  STATUS_ACCOUNT_DISABLED               = NTSTATUS($C0000072);
  STATUS_ACCOUNT_RESTRICTION            = NTSTATUS($C000006E);
  STATUS_ACCOUNT_LOCKED_OUT             = NTSTATUS($C0000234);
  STATUS_ACCOUNT_EXPIRED                = NTSTATUS($C0000193);
  STATUS_LOGON_TYPE_NOT_GRANTED         = NTSTATUS($C000015B);
begin
  case Status of
    HRESULT(ERROR_AUTHENTICATION_FIREWALL_FAILED),
    HRESULT(ERROR_AUTHENTICATION_FIREWALL_FAILED and $0000FFFF or FACILITY_WIN32 shl 16 or $80000000),
    HRESULT(STATUS_AUTHENTICATION_FIREWALL_FAILED),
    HRESULT(STATUS_AUTHENTICATION_FIREWALL_FAILED or FACILITY_NT_BIT),
    HRESULT(ERROR_ACCOUNT_DISABLED),
    HRESULT(ERROR_ACCOUNT_DISABLED and $0000FFFF or FACILITY_WIN32 shl 16 or $80000000),
    HRESULT(STATUS_ACCOUNT_DISABLED),
    HRESULT(STATUS_ACCOUNT_DISABLED or FACILITY_NT_BIT),
    HRESULT(ERROR_ACCOUNT_RESTRICTION),
    HRESULT(ERROR_ACCOUNT_RESTRICTION and $0000FFFF or FACILITY_WIN32 shl 16 or $80000000),
    HRESULT(STATUS_ACCOUNT_RESTRICTION),
    HRESULT(STATUS_ACCOUNT_RESTRICTION or FACILITY_NT_BIT),
    HRESULT(ERROR_ACCOUNT_LOCKED_OUT),
    HRESULT(ERROR_ACCOUNT_LOCKED_OUT and $0000FFFF or FACILITY_WIN32 shl 16 or $80000000),
    HRESULT(STATUS_ACCOUNT_LOCKED_OUT),
    HRESULT(STATUS_ACCOUNT_LOCKED_OUT or FACILITY_NT_BIT),
    HRESULT(ERROR_ACCOUNT_EXPIRED),
    HRESULT(ERROR_ACCOUNT_EXPIRED and $0000FFFF or FACILITY_WIN32 shl 16 or $80000000),
    HRESULT(STATUS_ACCOUNT_EXPIRED),
    HRESULT(STATUS_ACCOUNT_EXPIRED or FACILITY_NT_BIT),
    HRESULT(ERROR_LOGON_TYPE_NOT_GRANTED),
    HRESULT(ERROR_LOGON_TYPE_NOT_GRANTED and $0000FFFF or FACILITY_WIN32 shl 16 or $80000000),
    HRESULT(STATUS_LOGON_TYPE_NOT_GRANTED),
    HRESULT(STATUS_LOGON_TYPE_NOT_GRANTED or FACILITY_NT_BIT):
      Result := True;
  else
    Result := False;
  end;
end;


const
  advapi32 = 'advapi32.dll';

function CredWriteW; external advapi32 name 'CredWriteW';
function CredWriteA; external advapi32 name 'CredWriteA';
function CredReadW; external advapi32 name 'CredReadW';
function CredReadA; external advapi32 name 'CredReadA';
function CredEnumerateW; external advapi32 name 'CredEnumerateW';
function CredEnumerateA; external advapi32 name 'CredEnumerateA';
function CredWriteDomainCredentialsW; external advapi32 name 'CredWriteDomainCredentialsW';
function CredWriteDomainCredentialsA; external advapi32 name 'CredWriteDomainCredentialsA';
function CredReadDomainCredentialsW; external advapi32 name 'CredReadDomainCredentialsW';
function CredReadDomainCredentialsA; external advapi32 name 'CredReadDomainCredentialsA';
function CredDeleteW; external advapi32 name 'CredDeleteW';
function CredDeleteA; external advapi32 name 'CredDeleteA';
function CredRenameW; external advapi32 name 'CredRenameW';
function CredRenameA; external advapi32 name 'CredRenameA';
function CredGetTargetInfoW; external advapi32 name 'CredGetTargetInfoW';
function CredGetTargetInfoA; external advapi32 name 'CredGetTargetInfoA';
function CredMarshalCredentialW; external advapi32 name 'CredMarshalCredentialW';
function CredMarshalCredentialA; external advapi32 name 'CredMarshalCredentialA';
function CredUnmarshalCredentialW; external advapi32 name 'CredUnmarshalCredentialW';
function CredUnmarshalCredentialA; external advapi32 name 'CredUnmarshalCredentialA';
function CredIsMarshaledCredentialW; external advapi32 name 'CredIsMarshaledCredentialW';
function CredIsMarshaledCredentialA; external advapi32 name 'CredIsMarshaledCredentialA';
{$IFDEF UNICODE}
function CredWrite; external advapi32 name 'CredWriteW';
function CredRead; external advapi32 name 'CredReadW';
function CredEnumerate; external advapi32 name 'CredEnumerateW';
function CredWriteDomainCredentials; external advapi32 name 'CredWriteDomainCredentialsW';
function CredReadDomainCredentials; external advapi32 name 'CredReadDomainCredentialsW';
function CredDelete; external advapi32 name 'CredDeleteW';
function CredRename; external advapi32 name 'CredRenameW';
function CredGetTargetInfo; external advapi32 name 'CredGetTargetInfoW';
function CredMarshalCredential; external advapi32 name 'CredMarshalCredentialW';
function CredUnmarshalCredential; external advapi32 name 'CredUnmarshalCredentialW';
function CredIsMarshaledCredential; external advapi32 name 'CredIsMarshaledCredentialW';
{$ELSE}
function CredWrite; external advapi32 name 'CredWriteA';
function CredRead; external advapi32 name 'CredReadA';
function CredEnumerate; external advapi32 name 'CredEnumerateA';
function CredWriteDomainCredentials; external advapi32 name 'CredWriteDomainCredentialsA';
function CredReadDomainCredentials; external advapi32 name 'CredReadDomainCredentialsA';
function CredDelete; external advapi32 name 'CredDeleteA';
function CredRename; external advapi32 name 'CredRenameA';
function CredGetTargetInfo; external advapi32 name 'CredGetTargetInfoA';
function CredMarshalCredential; external advapi32 name 'CredMarshalCredentialA';
function CredUnmarshalCredential; external advapi32 name 'CredUnmarshalCredentialA';
function CredIsMarshaledCredential; external advapi32 name 'CredIsMarshaledCredentialA';
{$ENDIF}
function CredGetSessionTypes; external advapi32 name 'CredGetSessionTypes';
procedure CredFree; external advapi32 name 'CredFree';


const
  credui = 'credui.dll';

function CredUIPromptForCredentialsW; external credui name 'CredUIPromptForCredentialsW';
function CredUIPromptForCredentialsA; external credui name 'CredUIPromptForCredentialsA';
function CredUIParseUserNameW; external credui name 'CredUIParseUserNameW';
function CredUIParseUserNameA; external credui name 'CredUIParseUserNameA';
function CredUICmdLinePromptForCredentialsW; external credui name 'CredUICmdLinePromptForCredentialsW';
function CredUICmdLinePromptForCredentialsA; external credui name 'CredUICmdLinePromptForCredentialsA';
function CredUIConfirmCredentialsW; external credui name 'CredUIConfirmCredentialsW';
function CredUIConfirmCredentialsA; external credui name 'CredUIConfirmCredentialsA';
{$IFDEF UNICODE}
function CredUIPromptForCredentials; external credui name 'CredUIPromptForCredentialsW';
function CredUIParseUserName; external credui name 'CredUIParseUserNameW';
function CredUICmdLinePromptForCredentials; external credui name 'CredUICmdLinePromptForCredentialsW';
function CredUIConfirmCredentials; external credui name 'CredUIConfirmCredentialsW';
{$ELSE}
function CredUIPromptForCredentials; external credui name 'CredUIPromptForCredentialsA';
function CredUIParseUserName; external credui name 'CredUIParseUserNameA';
function CredUICmdLinePromptForCredentials; external credui name 'CredUICmdLinePromptForCredentialsA';
function CredUIConfirmCredentials; external credui name 'CredUIConfirmCredentialsA';
{$ENDIF}
function CredUIStoreSSOCredW; external credui name 'CredUIStoreSSOCredW';
function CredUIReadSSOCredW; external credui name 'CredUIReadSSOCredW';


end.
