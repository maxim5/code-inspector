{******************************************************************************}
{                                                                              }
{ Interface unit for the Windows NT Native API                                 }
{ Copyright (C) 1999, 2000, 2005 Marcel van Brakel (brakelm)                   }
{ Copyright (C) 2000-2001, 2005 Oliver Schneider (assarbad)                    }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project JEDI         }
{ APILIB home page, located at http://jedi-apilib.sourceforge.net              }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

// $Id: JwaNative.pas,v 1.24 2007/09/14 06:48:46 marquardt Exp $

{******************************************************************************}
{** WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING  **}
{******************************************************************************}
{**                                                                          **}
{** The prototypes, declarations and information in this file has been       **}
{** compiled from various sources as well as through reverse engineering     **}
{** techniques. We make no guarantee as to the correctness of the contents.  **}
{** Caution is recommended, USE AT YOUR OWN RISK.                            **}
{**                                                                          **}
{******************************************************************************}
{** About the Native API						     **                                          }
{******************************************************************************}
{**                                                                          **}
{** The functions herein are usually referred to as the NT Native API.       **}
{** The Native API is subdivided into several functional categories, which   **}
{** you can distinguish by the function name prefix:                         **}
{**                                                                          **}
{**   Cc   = Cache Controller                                                **}
{**   Cm   = Configuration Manager                                           **}
{**   Csr  = Client Server support functions (LPC; related: CSRSS.EXE)       **}
{**   Dbg  = Debugger support functions                                      **}
{**   Etw  = Event Tracing for Windows                                       **}
{**   Ex   = Executive                                                       **}
{**   Fs   = File system support functions                                   **}
{**   Hal  = Hardware abstraction layer functions                            **}
{**   Inbv = Something like: _In_itial _B_oot _V_ideo functions              **}
{**   Io   = I/O manager support functions                                   **}
{**   Kd   = Kernel debugger support functions                               **}
{**   Ke   = General Kernel                                                  **}
{**   Ki   = Kernel internal support functions (???)                         **}
{**   Ldr  = PE image loader support functions                               **}
{**   Lpc  = LPC support functions                                           **}
{**   Lsa  = Local security authority support functions                      **}
{**   Mm   = Memory manager support functions                                **}
{**   Nls  = National Language Support                                       **}
{**   Nt   = Generic Native APIs                                             **}
{**   Ob   = Object manager functions                                        **}
{**   Pfx  = Name prefix support functions (???)                             **}
{**   Po   = Power management support functions                              **}
{**   Ps   = Process management support functions                            **}
{**   Rtl  = Runtime library functions                                       **}
{**   Rtlp = Private runtime library functions 1)                            **}
{**   Se   = Security support functions                                      **}
{**   Wmi  = Windows management instrumentation support functions            **}
{**   Vf   = Driver Verifier                                                 **}
{**   Zw   = Nt* counterparts. Zw == "Zero Warranty"???                      **}
{**          1) "p" after the prefix means "private"                         **}
{**                                                                          **}
{** The Native API is split into a user mode component (mainly NTDLL.DLL)    **}
{** and a kernel mode component (mainly NTOSKRNL.EXE). While a large part of **}
{** the Native API is available both from usermode and kernelmode, some      **}
{** functions are exclusive to either mode. This unit only deals with 	     **}
{** functions that are available to usermode code.                           **}
{**                                                                          **}
{** Note that the functions prefixed with "Nt" and "Zw" usually appear in    **}
{** pairs, though not always! For details see http://assarbad.net    			**}
{**                                                                          **}
{** Most of the Native API is undocumented. However, Microsoft recently      **}
{** started to document a subset of the API in "winternl.h" in the Platform  **}
{** SDK. A small part of the Native API functions, specifically those useful **}
{** for kernel mode development (device drivers) are documented in the DDK.  **}
{**                                                                          **}
{******************************************************************************}
{** Special notes                                                            **}
{******************************************************************************}
{**                                                                          **}
{** Some functions herein have been implemented instead of being imported.   **}
{** That's due to the fact, that the FASTCALL calling convention is not      **}
{** available in Delphi. These functions include:                            **}
{** - RtlUshortByteSwap()                                                    **}
{** - RtlUlongByteSwap()                                                     **}
{** - RtlUlonglongByteSwap()                                                 **}
{**                                                                          **}
{** Other functions are implemented and also imported, but have been made    **}
{** available for OS versions not actually supporting them. These are:       **}
{** - RtlGetLastWin32Error()                                                 **}
{** - RtlSetLastWin32Error()                                                 **}
{**                                                                          **}
{** Starting with Windows XP, the Kernel32!GetLastError() and counterpart    **}
{** Kernel32!SetLastError() have been moved into NTDLL and are only function **}
{** forwarders to the NTDLL functions with the names above.                  **}
{** By importing them directly from Kernel32.dll via their old names we      **}
{** enable you to use these functions transparently without regard to the OS **}
{** version.                                                                 **}
{**                                                                          **}
{** RtlCopyMemory() had to be implemented via Delphi means because it's only **}
{** a macro (pointing to memcpy) in the C-world.                             **}
{**                                                                          **}
{** Last but not least we've implemented our own versions of some of the     **}
{** functions available through NTDLL [declared private Ntp*, Rtlp*]:        **}
{** - NtpGetProcessHeap()                                                    **}
{** - NtpCurrentTeb()                                                        **}
{** - RtlpGetCurrentPeb()                                                    **}
{**                                                                          **}
{** Plus a function that is available starting from Windows XP, but could be **}
{** useful on earlier versions as well [declared private Rtlp*]:             **}
{** - RtlpValidateUnicodeString()                                            **}
{**                                                                          **}
{** And our own flavor of it, omitting the first (currently unused)          **}
{** parameter:                                                               **}
{** - RtlpValidateUnicodeString2()                                           **}
{**                                                                          **}
{******************************************************************************}
{** References, Tools, Docs                                                  **}
{******************************************************************************}
{**                                                                          **}
{** - Windows NT/2000 Native API References (Gary Nebbett)                   **}
{**   ISBN 1-57870-199-6                                                     **}
{** - Undocumented Windows 2000 Secrets (Sven B. Schreiber)                  **}
{**   ISBN 0-201-72187-2                                                     **}
{** - Undocumented Windows NT (Prasad Dabak, Sandeep Phadke, Milind Borate)  **}
{**   ISBN 0-7645-4569-8                                                     **}
{** - Platform SDK for Windows 2003 Server (or later version)                **}
{**   http://www.microsoft.com/msdownload/platformsdk/sdkupdate/             **}
{** - Windows 2003 DDK (or similar DDK version                               **}
{**   http://www.microsoft.com/whdc/ddk/                                     **}
{** - WinDbg - a debugger that is usually badly underestimated!              **}
{**   http://www.microsoft.com/whdc/devtools/debugging                       **}
{** - IDA Pro Standard 4.7 (or later) - world's best disassembler            **}
{**   http://www.datarescue.com/idabase/                                     **}
{** - NTDEV, NTFSD, WINDBG mailing lists and more ...                        **}
{**   http://www.osronline.com/                                              **}
{** - Sysinternals tools and documentation of some "Windows secrets"         **}
{**   http://www.sysinternals.com/                                           **}
{** - A nicely done online compilation of NT Native APIs                     **}
{**   http://undocumented.ntinternals.net/                                   **}
{** - ReactOS (to cross-check own assumptions with those of other smart guys)**}
{**   http://www.reactos.com/                                                **}
{**                                                                          **}
{******************************************************************************}
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaNative;

interface
{$INCLUDE jediapilib.inc}

uses
  JwaWinType, JwaWinNT, JwaWinBase, JwaNtStatus, JwaBitFields;

{$WEAKPACKAGEUNIT}

// For native APIs we consider RTDL the better method of importing
{.$DEFINE RTDL}
{$IFDEF RTDL}{$DEFINE DYNAMIC_LINK}{$ENDIF}
const
  ntdll = 'ntdll.dll';

//------------------------------------------------------------------------------

{$ENDIF JWA_OMIT_SECTIONS}
{$IFNDEF JWA_IMPLEMENTATIONSECTION}


  
type
 
  _CLIENT_ID = record
    UniqueProcess: HANDLE;
    UniqueThread: HANDLE;
  end;
  CLIENT_ID = _CLIENT_ID;
  PCLIENT_ID = ^CLIENT_ID;
  TClientID = CLIENT_ID;
  PClientID = ^TClientID;

  KPRIORITY = LONG;

  _KWAIT_REASON = (
    Executive,
    FreePage,
    PageIn,
    PoolAllocation,
    DelayExecution,
    Suspended,
    UserRequest,
    WrExecutive,
    WrFreePage,
    WrPageIn,
    WrPoolAllocation,
    WrDelayExecution,
    WrSuspended,
    WrUserRequest,
    WrEventPair,
    WrQueue,
    WrLpcReceive,
    WrLpcReply,
    WrVirtualMemory,
    WrPageOut,
    WrRendezvous,
    Spare2,
    Spare3,
    Spare4,
    Spare5,
    Spare6,
    WrKernel,
    MaximumWaitReason);
  KWAIT_REASON = _KWAIT_REASON;
  TKWaitReason = KWAIT_REASON;

  _VM_COUNTERS = record
    PeakVirtualSize: SIZE_T;
    VirtualSize: SIZE_T;
    PageFaultCount: ULONG;
    PeakWorkingSetSize: SIZE_T;
    WorkingSetSize: SIZE_T;
    QuotaPeakPagedPoolUsage: SIZE_T;
    QuotaPagedPoolUsage: SIZE_T;
    QuotaPeakNonPagedPoolUsage: SIZE_T;
    QuotaNonPagedPoolUsage: SIZE_T;
    PagefileUsage: SIZE_T;
    PeakPagefileUsage: SIZE_T;
  end;
  VM_COUNTERS = _VM_COUNTERS;
  PVM_COUNTERS = ^VM_COUNTERS;
  TVmCounters = VM_COUNTERS;
  PVmCounters = ^TVmCounters;

const
  NonPagedPool = 0;
  PagedPool = 1;
  NonPagedPoolMustSucceed = 2;
  DontUseThisType = 3;
  NonPagedPoolCacheAligned = 4;
  PagedPoolCacheAligned = 5;
  NonPagedPoolCacheAlignedMustS = 6;
  MaxPoolType = 7;
  NonPagedPoolSession = 32;
  PagedPoolSession = NonPagedPoolSession + 1;
  NonPagedPoolMustSucceedSession = PagedPoolSession + 1;
  DontUseThisTypeSession = NonPagedPoolMustSucceedSession + 1;
  NonPagedPoolCacheAlignedSession = DontUseThisTypeSession + 1;
  PagedPoolCacheAlignedSession = NonPagedPoolCacheAlignedSession + 1;
  NonPagedPoolCacheAlignedMustSSession = PagedPoolCacheAlignedSession + 1;

type
  POOL_TYPE = NonPagedPool..NonPagedPoolCacheAlignedMustSSession;

  _IO_STATUS_BLOCK = record
    //union {
    Status: NTSTATUS;
    //    PVOID Pointer;
    //}
    Information: ULONG_PTR;
  end;
  IO_STATUS_BLOCK = _IO_STATUS_BLOCK;
  PIO_STATUS_BLOCK = ^IO_STATUS_BLOCK;
  TIoStatusBlock = IO_STATUS_BLOCK;
  PIoStatusBlock = ^TIoStatusBlock;


const
  ViewShare = 1;
  ViewUnmap = 2;

type
  SECTION_INHERIT = ViewShare..ViewUnmap;

  {.$IFNDEF JWA_INCLUDEMODE}
  _THREADINFOCLASS = (
    ThreadBasicInformation,
    ThreadTimes,
    ThreadPriority,
    ThreadBasePriority,
    ThreadAffinityMask,
    ThreadImpersonationToken,
    ThreadDescriptorTableEntry,
    ThreadEnableAlignmentFaultFixup,
    ThreadEventPair_Reusable,
    ThreadQuerySetWin32StartAddress,
    ThreadZeroTlsCell,
    ThreadPerformanceCount,
    ThreadAmILastThread,
    ThreadIdealProcessor,
    ThreadPriorityBoost,
    ThreadSetTlsArrayAddress,
    ThreadIsIoPending,
    ThreadHideFromDebugger,
    ThreadBreakOnTermination, // was added in XP - used by RtlSetThreadIsCritical()
    MaxThreadInfoClass);
  THREADINFOCLASS = _THREADINFOCLASS;
  {.$ENDIF JWA_INCLUDEMODE}
  THREAD_INFORMATION_CLASS = THREADINFOCLASS;


  TThreadInfoClass = THREADINFOCLASS;

{$IFNDEF JWA_INCLUDEMODE}
  KAFFINITY = ULONG;
  PKAFFINITY = ^KAFFINITY;
{$ENDIF JWA_INCLUDEMODE}

  PKNORMAL_ROUTINE = procedure(NormalContext, SystemArgument1, SystemArgument2: PVOID); stdcall;


  _PROCESSINFOCLASS = (
    ProcessBasicInformation,
    ProcessQuotaLimits,
    ProcessIoCounters,
    ProcessVmCounters,
    ProcessTimes,
    ProcessBasePriority,
    ProcessRaisePriority,
    ProcessDebugPort,
    ProcessExceptionPort,
    ProcessAccessToken,
    ProcessLdtInformation,
    ProcessLdtSize,
    ProcessDefaultHardErrorMode,
    ProcessIoPortHandlers, // Note: this is kernel mode only
    ProcessPooledUsageAndLimits,
    ProcessWorkingSetWatch,
    ProcessUserModeIOPL,
    ProcessEnableAlignmentFaultFixup,
    ProcessPriorityClass,
    ProcessWx86Information,
    ProcessHandleCount,
    ProcessAffinityMask,
    ProcessPriorityBoost,
    ProcessDeviceMap,
    ProcessSessionInformation,
    ProcessForegroundInformation,
    ProcessWow64Information, // = 26
    ProcessImageFileName, // added after W2K
    ProcessLUIDDeviceMapsEnabled,
    ProcessBreakOnTermination, // used by RtlSetProcessIsCritical()
    ProcessDebugObjectHandle,
    ProcessDebugFlags,
    ProcessHandleTracing,
    MaxProcessInfoClass);
  PROCESSINFOCLASS = _PROCESSINFOCLASS;
  PROCESS_INFORMATION_CLASS = PROCESSINFOCLASS;
  TProcessInfoClass = PROCESSINFOCLASS;

  _KPROFILE_SOURCE = (
    ProfileTime,
    ProfileAlignmentFixup,
    ProfileTotalIssues,
    ProfilePipelineDry,
    ProfileLoadInstructions,
    ProfilePipelineFrozen,
    ProfileBranchInstructions,
    ProfileTotalNonissues,
    ProfileDcacheMisses,
    ProfileIcacheMisses,
    ProfileCacheMisses,
    ProfileBranchMispredictions,
    ProfileStoreInstructions,
    ProfileFpInstructions,
    ProfileIntegerInstructions,
    Profile2Issue,
    Profile3Issue,
    Profile4Issue,
    ProfileSpecialInstructions,
    ProfileTotalCycles,
    ProfileIcacheIssues,
    ProfileDcacheAccesses,
    ProfileMemoryBarrierCycles,
    ProfileLoadLinkedIssues,
    ProfileMaximum);
  KPROFILE_SOURCE = _KPROFILE_SOURCE;
  TKProfileSource = KPROFILE_SOURCE;

  PIO_APC_ROUTINE = procedure(ApcContext: PVOID; IoStatusBlock: PIO_STATUS_BLOCK; Reserved: ULONG); stdcall;

  _FILE_FULL_EA_INFORMATION = record
    NextEntryOffset: ULONG;
    Flags: UCHAR;
    EaNameLength: UCHAR;
    EaValueLength: USHORT;
    EaName: array[0..0] of AnsiChar;
  end;
  FILE_FULL_EA_INFORMATION = _FILE_FULL_EA_INFORMATION;
  PFILE_FULL_EA_INFORMATION = ^FILE_FULL_EA_INFORMATION;
  TFileFullEaInformation = FILE_FULL_EA_INFORMATION;
  PFileFullEaInformation = ^TFileFullEaInformation;

  _FSINFOCLASS = (
    FileFsFiller0,
    FileFsVolumeInformation, // 1
    FileFsLabelInformation, // 2
    FileFsSizeInformation, // 3
    FileFsDeviceInformation, // 4
    FileFsAttributeInformation, // 5
    FileFsControlInformation, // 6
    FileFsFullSizeInformation, // 7
    FileFsObjectIdInformation, // 8
    FileFsMaximumInformation);
  FS_INFORMATION_CLASS = _FSINFOCLASS;
  PFS_INFORMATION_CLASS = ^FS_INFORMATION_CLASS;
  TFsInformationClass = FS_INFORMATION_CLASS;
  PFsInformationClass = ^TFsInformationClass;


{$IFNDEF JWA_INCLUDEMODE} //defined in jwaWindows.pas
  UUID = GUID;
{$ENDIF JWA_INCLUDEMODE}


  _FILE_BASIC_INFORMATION = record
    CreationTime: LARGE_INTEGER;
    LastAccessTime: LARGE_INTEGER;
    LastWriteTime: LARGE_INTEGER;
    ChangeTime: LARGE_INTEGER;
    FileAttributes: ULONG;
  end;
  FILE_BASIC_INFORMATION = _FILE_BASIC_INFORMATION;
  PFILE_BASIC_INFORMATION = ^FILE_BASIC_INFORMATION;
  TFileBasicInformation = FILE_BASIC_INFORMATION;
  PFileBasicInformation = ^TFileBasicInformation;

  _FILE_NETWORK_OPEN_INFORMATION = record
    CreationTime: LARGE_INTEGER;
    LastAccessTime: LARGE_INTEGER;
    LastWriteTime: LARGE_INTEGER;
    ChangeTime: LARGE_INTEGER;
    AllocationSize: LARGE_INTEGER;
    EndOfFile: LARGE_INTEGER;
    FileAttributes: ULONG;
  end;
  FILE_NETWORK_OPEN_INFORMATION = _FILE_NETWORK_OPEN_INFORMATION;
  PFILE_NETWORK_OPEN_INFORMATION = ^FILE_NETWORK_OPEN_INFORMATION;
  TFileNetworkOpenInformation = FILE_NETWORK_OPEN_INFORMATION;
  PFileNetworkOpenInformation = ^TFileNetworkOpenInformation;

  
  _FILE_INFORMATION_CLASS = (
    FileFiller0,
    FileDirectoryInformation, // 1
    FileFullDirectoryInformation, // 2
    FileBothDirectoryInformation, // 3
    FileBasicInformation, // 4  wdm
    FileStandardInformation, // 5  wdm
    FileInternalInformation, // 6
    FileEaInformation, // 7
    FileAccessInformation, // 8
    FileNameInformation, // 9
    FileRenameInformation, // 10
    FileLinkInformation, // 11
    FileNamesInformation, // 12
    FileDispositionInformation, // 13
    FilePositionInformation, // 14 wdm
    FileFullEaInformation, // 15
    FileModeInformation, // 16
    FileAlignmentInformation, // 17
    FileAllInformation, // 18
    FileAllocationInformation, // 19
    FileEndOfFileInformation, // 20 wdm
    FileAlternateNameInformation, // 21
    FileStreamInformation, // 22
    FilePipeInformation, // 23
    FilePipeLocalInformation, // 24
    FilePipeRemoteInformation, // 25
    FileMailslotQueryInformation, // 26
    FileMailslotSetInformation, // 27
    FileCompressionInformation, // 28
    FileObjectIdInformation, // 29
    FileCompletionInformation, // 30
    FileMoveClusterInformation, // 31
    FileQuotaInformation, // 32
    FileReparsePointInformation, // 33
    FileNetworkOpenInformation, // 34
    FileAttributeTagInformation, // 35
    FileTrackingInformation, // 36
    FileMaximumInformation);
  FILE_INFORMATION_CLASS = _FILE_INFORMATION_CLASS;
  PFILE_INFORMATION_CLASS = ^FILE_INFORMATION_CLASS;
  TFileInformationClass = FILE_INFORMATION_CLASS;
  PFileInformationClass = ^TFileInformationClass;

  _FILE_STANDARD_INFORMATION = record
    AllocationSize: LARGE_INTEGER;
    EndOfFile: LARGE_INTEGER;
    NumberOfLinks: ULONG;
    DeletePending: ByteBool;
    Directory: ByteBool;
  end;
  FILE_STANDARD_INFORMATION = _FILE_STANDARD_INFORMATION;
  PFILE_STANDARD_INFORMATION = ^FILE_STANDARD_INFORMATION;
  TFileStandardInformation = FILE_STANDARD_INFORMATION;
  PFileStandardInformation = ^TFileStandardInformation;

  _FILE_POSITION_INFORMATION = record
    CurrentByteOffset: LARGE_INTEGER;
  end;
  FILE_POSITION_INFORMATION = _FILE_POSITION_INFORMATION;
  PFILE_POSITION_INFORMATION = ^FILE_POSITION_INFORMATION;
  TFilePositionInformation = FILE_POSITION_INFORMATION;
  PFilePositionInformation = ^TFilePositionInformation;

  _FILE_ALIGNMENT_INFORMATION = record
    AlignmentRequirement: ULONG;
  end;
  FILE_ALIGNMENT_INFORMATION = _FILE_ALIGNMENT_INFORMATION;
  PFILE_ALIGNMENT_INFORMATION = ^FILE_ALIGNMENT_INFORMATION;
  TFileAlignmentInformation = FILE_ALIGNMENT_INFORMATION;
  PFileAlignmentInformation = ^TFileAlignmentInformation;

  _KEY_SET_INFORMATION_CLASS = (KeyWriteTimeInformation);
  KEY_SET_INFORMATION_CLASS = _KEY_SET_INFORMATION_CLASS;

  _KEY_INFORMATION_CLASS = (
    KeyBasicInformation,
    KeyNodeInformation,
    KeyFullInformation,
    KeyNameInformation);
  KEY_INFORMATION_CLASS = _KEY_INFORMATION_CLASS;
  TKeyInformationClass = KEY_INFORMATION_CLASS;

  _KEY_VALUE_INFORMATION_CLASS = (
    KeyValueBasicInformation,
    KeyValueFullInformation,
    KeyValuePartialInformation,
    KeyValueFullInformationAlign64,
    KeyValuePartialInformationAlign64);
  KEY_VALUE_INFORMATION_CLASS = _KEY_VALUE_INFORMATION_CLASS;
  TKeyValueInformationClass = KEY_VALUE_INFORMATION_CLASS;

  _KEY_VALUE_ENTRY = record
    ValueName: PUNICODE_STRING;
    DataLength: ULONG;
    DataOffset: ULONG;
    Type_: ULONG;
  end;
  KEY_VALUE_ENTRY = _KEY_VALUE_ENTRY;
  PKEY_VALUE_ENTRY = ^KEY_VALUE_ENTRY;
  TKeyValueEntry = KEY_VALUE_ENTRY;
  PKeyValueEntry = ^TKeyValueEntry;

  _KEY_FULL_INFORMATION = record
    LastWriteTime: LARGE_INTEGER;
    TitleIndex: ULONG;
    ClassOffset: ULONG;
    ClassLength: ULONG;
    SubKeys: ULONG;
    MaxNameLen: ULONG;
    MaxClassLen: ULONG;
    Values: ULONG;
    MaxValueNameLen: ULONG;
    MaxValueDataLen: ULONG;
    Class_: Array[0..0] of WCHAR;
  end;
  PKEY_FULL_INFORMATION = ^_KEY_FULL_INFORMATION;
  PKeyFullInformation = PKEY_FULL_INFORMATION;
  TKeyFullInformation = _KEY_FULL_INFORMATION;

  _KEY_VALUE_FULL_INFORMATION = record
    TitleIndex: ULONG;
    Type_: ULONG;
    DataOffset: ULONG;
    DataLength: ULONG;
    NameLength: ULONG;
    Name: Array[0..0] of WCHAR;
  end;
  PKEY_VALUE_FULL_INFORMATION = ^_KEY_VALUE_FULL_INFORMATION;
  TKeyValueFullInformation = _KEY_VALUE_FULL_INFORMATION;
  PKeyValueFullInformation = PKEY_VALUE_FULL_INFORMATION;


  {$IFNDEF JWA_INCLUDEMODE}
  _DEVICE_POWER_STATE = (
    PowerDeviceUnspecified,
    PowerDeviceD0,
    PowerDeviceD1,
    PowerDeviceD2,
    PowerDeviceD3,
    PowerDeviceMaximum);

  DEVICE_POWER_STATE = _DEVICE_POWER_STATE;
  PDEVICE_POWER_STATE = ^DEVICE_POWER_STATE;
  TDevicePowerState = DEVICE_POWER_STATE;

  POWER_ACTION = (
    PowerActionNone,
    PowerActionReserved,
    PowerActionSleep,
    PowerActionHibernate,
    PowerActionShutdown,
    PowerActionShutdownReset,
    PowerActionShutdownOff,
    PowerActionWarmEject);
  PPOWER_ACTION = ^POWER_ACTION;
  TPowerAction = POWER_ACTION;

  _SYSTEM_POWER_STATE = (
    PowerSystemUnspecified,
    PowerSystemWorking,
    PowerSystemSleeping1,
    PowerSystemSleeping2,
    PowerSystemSleeping3,
    PowerSystemHibernate,
    PowerSystemShutdown,
    PowerSystemMaximum);
  SYSTEM_POWER_STATE = _SYSTEM_POWER_STATE;
  PSYSTEM_POWER_STATE = ^SYSTEM_POWER_STATE;
  TSystemPowerState = SYSTEM_POWER_STATE;

  POWER_INFORMATION_LEVEL = (
    SystemPowerPolicyAc,
    SystemPowerPolicyDc,
    VerifySystemPolicyAc,
    VerifySystemPolicyDc,
    SystemPowerCapabilities,
    SystemBatteryState,
    SystemPowerStateHandler,
    ProcessorStateHandler,
    SystemPowerPolicyCurrent,
    AdministratorPowerPolicy,
    SystemReserveHiberFile,
    ProcessorInformation,
    SystemPowerInformation);
  TPowerInformationLevel = POWER_INFORMATION_LEVEL;
  {$ENDIF JWA_INCLUDEMODE}

  _RTL_RANGE = record
    // The start of the range
    Start: ULONGLONG; // Read only
    // The end of the range
    End_: ULONGLONG; // Read only
    // Data the user passed in when they created the range
    UserData: PVOID; // Read/Write
    // The owner of the range
    Owner: PVOID; // Read/Write
    // User defined flags the user specified when they created the range
    Attributes: UCHAR; // Read/Write
    // Flags (RTL_RANGE_*)
    Flags: UCHAR; // Read only
  end;
  RTL_RANGE = _RTL_RANGE;
  PRTL_RANGE = ^RTL_RANGE;
  TRtlRange = RTL_RANGE;
  PRtlRange = ^TRtlRange;

const
  RTL_RANGE_SHARED = $01;
  RTL_RANGE_CONFLICT = $02;

type
  _RTL_RANGE_LIST = record
    // The list of ranges
    ListHead: LIST_ENTRY;
    // These always come in useful
    Flags: ULONG; // use RANGE_LIST_FLAG_*
    // The number of entries in the list
    Count: ULONG;
    // Every time an add/delete operation is performed on the list this is
    // incremented.  It is checked during iteration to ensure that the list
    // hasn't changed between GetFirst/GetNext or GetNext/GetNext calls
    Stamp: ULONG;
  end;
  RTL_RANGE_LIST = _RTL_RANGE_LIST;
  PRTL_RANGE_LIST = ^RTL_RANGE_LIST;
  TRtlRangeList = RTL_RANGE_LIST;
  PRtlRangeList = ^TRtlRangeList;

  _RANGE_LIST_ITERATOR = record
    RangeListHead: PLIST_ENTRY;
    MergedHead: PLIST_ENTRY;
    Current: PVOID;
    Stamp: ULONG;
  end;
  RTL_RANGE_LIST_ITERATOR = _RANGE_LIST_ITERATOR;
  PRTL_RANGE_LIST_ITERATOR = ^RTL_RANGE_LIST_ITERATOR;
  TRtlRangeListIterator = RTL_RANGE_LIST_ITERATOR;
  PRtlRangeListIterator = ^TRtlRangeListIterator;

// End of NTDDK.H

//==============================================================================
// NT System Services
//==============================================================================

type
  _SYSTEM_INFORMATION_CLASS = (
    SystemBasicInformation,
    SystemProcessorInformation,
    SystemPerformanceInformation,
    SystemTimeOfDayInformation,
    SystemNotImplemented1,
    SystemProcessesAndThreadsInformation,
    SystemCallCounts,
    SystemConfigurationInformation,
    SystemProcessorTimes,
    SystemGlobalFlag,
    SystemNotImplemented2,
    SystemModuleInformation,
    SystemLockInformation,
    SystemNotImplemented3,
    SystemNotImplemented4,
    SystemNotImplemented5,
    SystemHandleInformation,
    SystemObjectInformation,
    SystemPagefileInformation,
    SystemInstructionEmulationCounts,
    SystemInvalidInfoClass1,
    SystemCacheInformation,
    SystemPoolTagInformation,
    SystemProcessorStatistics,
    SystemDpcInformation,
    SystemNotImplemented6,
    SystemLoadImage,
    SystemUnloadImage,
    SystemTimeAdjustment,
    SystemNotImplemented7,
    SystemNotImplemented8,
    SystemNotImplemented9,
    SystemCrashDumpInformation,
    SystemExceptionInformation,
    SystemCrashDumpStateInformation,
    SystemKernelDebuggerInformation,
    SystemContextSwitchInformation,
    SystemRegistryQuotaInformation,
    SystemLoadAndCallImage,
    SystemPrioritySeparation,
    SystemNotImplemented10,
    SystemNotImplemented11,
    SystemInvalidInfoClass2,
    SystemInvalidInfoClass3,
    SystemTimeZoneInformation,
    SystemLookasideInformation,
    SystemSetTimeSlipEvent,
    SystemCreateSession,
    SystemDeleteSession,
    SystemInvalidInfoClass4,
    SystemRangeStartInformation,
    SystemVerifierInformation,
    SystemAddVerifier,
    SystemSessionProcessesInformation);
  SYSTEM_INFORMATION_CLASS = _SYSTEM_INFORMATION_CLASS;
  TSystemInformationClass = SYSTEM_INFORMATION_CLASS;

type
  {.$IFNDEF JWA_INCLUDEMODE}
  _SYSTEM_BASIC_INFORMATION = record // Information Class 0
    Unknown: ULONG;
    MaximumIncrement: ULONG;
    PhysicalPageSize: ULONG;
    NumberOfPhysicalPages: ULONG;
    LowestPhysicalPage: ULONG;
    HighestPhysicalPage: ULONG;
    AllocationGranularity: ULONG;
    LowestUserAddress: ULONG;
    HighestUserAddress: ULONG;
    ActiveProcessors: ULONG;
    NumberProcessors: UCHAR;
  end;
  SYSTEM_BASIC_INFORMATION = _SYSTEM_BASIC_INFORMATION;
  PSYSTEM_BASIC_INFORMATION = ^SYSTEM_BASIC_INFORMATION;
  TSystemBasicInformation = SYSTEM_BASIC_INFORMATION;
  PSystemBasicInformation = ^TSystemBasicInformation;
  {.$ENDIF JWA_INCLUDEMODE}

  _SYSTEM_PROCESSOR_INFORMATION = record // Information Class 1
    ProcessorArchitecture: USHORT;
    ProcessorLevel: USHORT;
    ProcessorRevision: USHORT;
    Unknown: USHORT;
    FeatureBits: ULONG;
  end;
  SYSTEM_PROCESSOR_INFORMATION = _SYSTEM_PROCESSOR_INFORMATION;
  PSYSTEM_PROCESSOR_INFORMATION = ^SYSTEM_PROCESSOR_INFORMATION;

  {.$IFNDEF JWA_INCLUDEMODE}
  TSystemProcessorInformation = SYSTEM_PROCESSOR_INFORMATION;
  PSystemProcessorInformation = ^TSystemProcessorInformation;

  _SYSTEM_PERFORMANCE_INFORMATION = record // Information Class 2
    IdleTime: LARGE_INTEGER;
    ReadTransferCount: LARGE_INTEGER;
    WriteTransferCount: LARGE_INTEGER;
    OtherTransferCount: LARGE_INTEGER;
    ReadOperationCount: ULONG;
    WriteOperationCount: ULONG;
    OtherOperationCount: ULONG;
    AvailablePages: ULONG;
    TotalCommittedPages: ULONG;
    TotalCommitLimit: ULONG;
    PeakCommitment: ULONG;
    PageFaults: ULONG;
    WriteCopyFaults: ULONG;
    TransistionFaults: ULONG;
    Reserved1: ULONG;
    DemandZeroFaults: ULONG;
    PagesRead: ULONG;
    PageReadIos: ULONG;
    Reserved2: array[0..1] of ULONG;
    PagefilePagesWritten: ULONG;
    PagefilePageWriteIos: ULONG;
    MappedFilePagesWritten: ULONG;
    MappedFilePageWriteIos: ULONG;
    PagedPoolUsage: ULONG;
    NonPagedPoolUsage: ULONG;
    PagedPoolAllocs: ULONG;
    PagedPoolFrees: ULONG;
    NonPagedPoolAllocs: ULONG;
    NonPagedPoolFrees: ULONG;
    TotalFreeSystemPtes: ULONG;
    SystemCodePage: ULONG;
    TotalSystemDriverPages: ULONG;
    TotalSystemCodePages: ULONG;
    SmallNonPagedLookasideListAllocateHits: ULONG;
    SmallPagedLookasideListAllocateHits: ULONG;
    Reserved3: ULONG;
    MmSystemCachePage: ULONG;
    PagedPoolPage: ULONG;
    SystemDriverPage: ULONG;
    FastReadNoWait: ULONG;
    FastReadWait: ULONG;
    FastReadResourceMiss: ULONG;
    FastReadNotPossible: ULONG;
    FastMdlReadNoWait: ULONG;
    FastMdlReadWait: ULONG;
    FastMdlReadResourceMiss: ULONG;
    FastMdlReadNotPossible: ULONG;
    MapDataNoWait: ULONG;
    MapDataWait: ULONG;
    MapDataNoWaitMiss: ULONG;
    MapDataWaitMiss: ULONG;
    PinMappedDataCount: ULONG;
    PinReadNoWait: ULONG;
    PinReadWait: ULONG;
    PinReadNoWaitMiss: ULONG;
    PinReadWaitMiss: ULONG;
    CopyReadNoWait: ULONG;
    CopyReadWait: ULONG;
    CopyReadNoWaitMiss: ULONG;
    CopyReadWaitMiss: ULONG;
    MdlReadNoWait: ULONG;
    MdlReadWait: ULONG;
    MdlReadNoWaitMiss: ULONG;
    MdlReadWaitMiss: ULONG;
    ReadAheadIos: ULONG;
    LazyWriteIos: ULONG;
    LazyWritePages: ULONG;
    DataFlushes: ULONG;
    DataPages: ULONG;
    ContextSwitches: ULONG;
    FirstLevelTbFills: ULONG;
    SecondLevelTbFills: ULONG;
    SystemCalls: ULONG;
  end;
  SYSTEM_PERFORMANCE_INFORMATION = _SYSTEM_PERFORMANCE_INFORMATION;
  PSYSTEM_PERFORMANCE_INFORMATION = ^SYSTEM_PERFORMANCE_INFORMATION;
  TSystemPerformanceInformation = SYSTEM_PERFORMANCE_INFORMATION;
  PSystemPerformanceInformation = ^TSystemPerformanceInformation;
  {.$ENDIF JWA_INCLUDEMODE}

  _SYSTEM_TIME_OF_DAY_INFORMATION = record // Information Class 3
    BootTime: LARGE_INTEGER;
    CurrentTime: LARGE_INTEGER;
    TimeZoneBias: LARGE_INTEGER;
    CurrentTimeZoneId: ULONG;
  end;
  SYSTEM_TIME_OF_DAY_INFORMATION = _SYSTEM_TIME_OF_DAY_INFORMATION;
  PSYSTEM_TIME_OF_DAY_INFORMATION = ^SYSTEM_TIME_OF_DAY_INFORMATION;

  {.$IFNDEF JWA_INCLUDEMODE}
  TSystemTimeOfDayInformation = SYSTEM_TIME_OF_DAY_INFORMATION;
  PSystemTimeOfDayInformation = ^TSystemTimeOfDayInformation;
  {.$ENDIF JWA_INCLUDEMODE}

  _IO_COUNTERSEX = record
    ReadOperationCount: LARGE_INTEGER;
    WriteOperationCount: LARGE_INTEGER;
    OtherOperationCount: LARGE_INTEGER;
    ReadTransferCount: LARGE_INTEGER;
    WriteTransferCount: LARGE_INTEGER;
    OtherTransferCount: LARGE_INTEGER;
  end;
  IO_COUNTERSEX = _IO_COUNTERSEX;
  PIO_COUNTERSEX = ^IO_COUNTERSEX;
  TIoCountersEx = IO_COUNTERSEX;
  PIoCountersEx = ^TIoCountersEx;

  THREAD_STATE = (
    StateInitialized,
    StateReady,
    StateRunning,
    StateStandby,
    StateTerminated,
    StateWait,
    StateTransition,
    StateUnknown);
  TThreadState = THREAD_STATE;

  _SYSTEM_THREADS = record
    KernelTime: LARGE_INTEGER;
    UserTime: LARGE_INTEGER;
    CreateTime: LARGE_INTEGER;
    WaitTime: ULONG;
    StartAddress: PVOID;
    ClientId: CLIENT_ID;
    Priority: KPRIORITY;
    BasePriority: KPRIORITY;
    ContextSwitchCount: ULONG;
    State: THREAD_STATE;
    WaitReason: KWAIT_REASON;
  end;
  SYSTEM_THREADS = _SYSTEM_THREADS;
  PSYSTEM_THREADS = ^SYSTEM_THREADS;
  TSystemThreads = SYSTEM_THREADS;
  PSystemThreads = PSYSTEM_THREADS;

  _SYSTEM_PROCESSES = record // Information Class 5
    NextEntryDelta: ULONG;
    ThreadCount: ULONG;
    Reserved1: array[0..5] of ULONG;
    CreateTime: LARGE_INTEGER;
    UserTime: LARGE_INTEGER;
    KernelTime: LARGE_INTEGER;
    ProcessName: UNICODE_STRING;
    BasePriority: KPRIORITY;
    ProcessId: ULONG;
    InheritedFromProcessId: ULONG;
    HandleCount: ULONG;
    // next two were Reserved2: array [0..1] of ULONG; thanks to Nico Bendlin
    SessionId: ULONG;
    Reserved2: ULONG;
    VmCounters: VM_COUNTERS;
    PrivatePageCount: ULONG;
    IoCounters: IO_COUNTERSEX; // Windows 2000 only
    Threads: array[0..0] of SYSTEM_THREADS;
  end;
  SYSTEM_PROCESSES = _SYSTEM_PROCESSES;
  PSYSTEM_PROCESSES = ^SYSTEM_PROCESSES;
  TSystemProcesses = SYSTEM_PROCESSES;
  PSystemProcesses = PSYSTEM_PROCESSES;

  _SYSTEM_CALLS_INFORMATION = record // Information Class 6
    Size: ULONG;
    NumberOfDescriptorTables: ULONG;
    NumberOfRoutinesInTable: array[0..0] of ULONG;
    // ULONG CallCounts[];
  end;
  SYSTEM_CALLS_INFORMATION = _SYSTEM_CALLS_INFORMATION;
  PSYSTEM_CALLS_INFORMATION = ^SYSTEM_CALLS_INFORMATION;
  TSystemCallsInformation = SYSTEM_CALLS_INFORMATION;
  PSystemCallsInformation = ^TSystemCallsInformation;

  _SYSTEM_CONFIGURATION_INFORMATION = record // Information Class 7
    DiskCount: ULONG;
    FloppyCount: ULONG;
    CdRomCount: ULONG;
    TapeCount: ULONG;
    SerialCount: ULONG;
    ParallelCount: ULONG;
  end;
  SYSTEM_CONFIGURATION_INFORMATION = _SYSTEM_CONFIGURATION_INFORMATION;
  PSYSTEM_CONFIGURATION_INFORMATION = ^SYSTEM_CONFIGURATION_INFORMATION;
  TSystemConfigurationInformation = SYSTEM_CONFIGURATION_INFORMATION;
  PSystemConfigurationInformation = ^TSystemConfigurationInformation;

  _SYSTEM_PROCESSOR_TIMES = record // Information Class 8
    IdleTime: LARGE_INTEGER;
    KernelTime: LARGE_INTEGER;
    UserTime: LARGE_INTEGER;
    DpcTime: LARGE_INTEGER;
    InterruptTime: LARGE_INTEGER;
    InterruptCount: ULONG;
  end;
  SYSTEM_PROCESSOR_TIMES = _SYSTEM_PROCESSOR_TIMES;
  PSYSTEM_PROCESSOR_TIMES = ^SYSTEM_PROCESSOR_TIMES;
  TSystemProcessorTimes = SYSTEM_PROCESSOR_TIMES;
  PSystemProcessorTimes = ^TSystemProcessorTimes;

  _SYSTEM_GLOBAL_FLAG = record // Information Class 9
    GlobalFlag: ULONG;
  end;
  SYSTEM_GLOBAL_FLAG = _SYSTEM_GLOBAL_FLAG;
  PSYSTEM_GLOBAL_FLAG = ^SYSTEM_GLOBAL_FLAG;
  TSystemGlobalFlag = SYSTEM_GLOBAL_FLAG;
  PSystemGlobalFlag = ^TSystemGlobalFlag;

  _SYSTEM_MODULE_INFORMATION = record // Information Class 11
    Reserved: array[0..1] of ULONG;
    Base: PVOID;
    Size: ULONG;
    Flags: ULONG;
    Index: USHORT;
    Unknown: USHORT;
    LoadCount: USHORT;
    ModuleNameOffset: USHORT;
    ImageName: array[0..255] of AnsiChar;
  end;
  SYSTEM_MODULE_INFORMATION = _SYSTEM_MODULE_INFORMATION;
  PSYSTEM_MODULE_INFORMATION = ^SYSTEM_MODULE_INFORMATION;
  TSystemModuleInformation = SYSTEM_MODULE_INFORMATION;
  PSystemModuleInformation = PSYSTEM_MODULE_INFORMATION;

  _SYSTEM_LOCK_INFORMATION = record // Information Class 12
    Address: PVOID;
    Type_: USHORT;
    Reserved1: USHORT;
    ExclusiveOwnerThreadId: ULONG;
    ActiveCount: ULONG;
    ContentionCount: ULONG;
    Reserved2: array[0..1] of ULONG;
    NumberOfSharedWaiters: ULONG;
    NumberOfExclusiveWaiters: ULONG;
  end;
  SYSTEM_LOCK_INFORMATION = _SYSTEM_LOCK_INFORMATION;
  PSYSTEM_LOCK_INFORMATION = ^SYSTEM_LOCK_INFORMATION;
  TSystemLockInformation = SYSTEM_LOCK_INFORMATION;
  PSystemLockInformation = ^TSystemLockInformation;

  _SYSTEM_HANDLE_INFORMATION = record // Information Class 16
    ProcessId: ULONG;
    ObjectTypeNumber: UCHAR;
    Flags: UCHAR; // 0x01 = PROTECT_FROM_CLOSE, 0x02 = INHERIT
    Handle: USHORT;
    Object_: PVOID;
    GrantedAccess: ACCESS_MASK;
  end;
  SYSTEM_HANDLE_INFORMATION = _SYSTEM_HANDLE_INFORMATION;
  PSYSTEM_HANDLE_INFORMATION = ^SYSTEM_HANDLE_INFORMATION;
  TSystemHandleInformation = SYSTEM_HANDLE_INFORMATION;
  PSystemHandleInformation = ^TSystemHandleInformation;

  _SYSTEM_OBJECT_TYPE_INFORMATION = record // Information Class 17
    NextEntryOffset: ULONG;
    ObjectCount: ULONG;
    HandleCount: ULONG;
    TypeNumber: ULONG;
    InvalidAttributes: ULONG;
    GenericMapping: GENERIC_MAPPING;
    ValidAccessMask: ACCESS_MASK;
    PoolType: POOL_TYPE;
    Unknown: UCHAR;
    Name: UNICODE_STRING;
  end;
  SYSTEM_OBJECT_TYPE_INFORMATION = _SYSTEM_OBJECT_TYPE_INFORMATION;
  PSYSTEM_OBJECT_TYPE_INFORMATION = ^SYSTEM_OBJECT_TYPE_INFORMATION;
  TSystemObjectTypeInformation = SYSTEM_OBJECT_TYPE_INFORMATION;
  PSystemObjectTypeInformation = ^TSystemObjectTypeInformation;

  _SYSTEM_OBJECT_INFORMATION = record
    NextEntryOffset: ULONG;
    Object_: PVOID;
    CreatorProcessId: ULONG;
    Unknown: USHORT;
    Flags: USHORT;
    PointerCount: ULONG;
    HandleCount: ULONG;
    PagedPoolUsage: ULONG;
    NonPagedPoolUsage: ULONG;
    ExclusiveProcessId: ULONG;
    SecurityDescriptor: PSECURITY_DESCRIPTOR;
    Name: UNICODE_STRING;
  end;
  SYSTEM_OBJECT_INFORMATION = _SYSTEM_OBJECT_INFORMATION;
  PSYSTEM_OBJECT_INFORMATION = ^SYSTEM_OBJECT_INFORMATION;
  TSystemObjectInformation = SYSTEM_OBJECT_INFORMATION;
  PSystemObjectInformation = ^TSystemObjectInformation;

  _SYSTEM_PAGEFILE_INFORMATION = record // Information Class 18
    NextEntryOffset: ULONG;
    CurrentSize: ULONG;
    TotalUsed: ULONG;
    PeakUsed: ULONG;
    FileName: UNICODE_STRING;
  end;
  SYSTEM_PAGEFILE_INFORMATION = _SYSTEM_PAGEFILE_INFORMATION;
  PSYSTEM_PAGEFILE_INFORMATION = ^SYSTEM_PAGEFILE_INFORMATION;
  TSystemPageFileInformation = SYSTEM_PAGEFILE_INFORMATION;
  PSystemPageFileInformation = PSYSTEM_PAGEFILE_INFORMATION;

  _SYSTEM_INSTRUCTION_EMULATION_INFORMATION = record // Info Class 19
    GenericInvalidOpcode: ULONG;
    TwoByteOpcode: ULONG;
    ESprefix: ULONG;
    CSprefix: ULONG;
    SSprefix: ULONG;
    DSprefix: ULONG;
    FSPrefix: ULONG;
    GSprefix: ULONG;
    OPER32prefix: ULONG;
    ADDR32prefix: ULONG;
    INSB: ULONG;
    INSW: ULONG;
    OUTSB: ULONG;
    OUTSW: ULONG;
    PUSHFD: ULONG;
    POPFD: ULONG;
    INTnn: ULONG;
    INTO: ULONG;
    IRETD: ULONG;
    FloatingPointOpcode: ULONG;
    INBimm: ULONG;
    INWimm: ULONG;
    OUTBimm: ULONG;
    OUTWimm: ULONG;
    INB: ULONG;
    INW: ULONG;
    OUTB: ULONG;
    OUTW: ULONG;
    LOCKprefix: ULONG;
    REPNEprefix: ULONG;
    REPprefix: ULONG;
    CLI: ULONG;
    STI: ULONG;
    HLT: ULONG;
  end;
  SYSTEM_INSTRUCTION_EMULATION_INFORMATION = _SYSTEM_INSTRUCTION_EMULATION_INFORMATION;
  PSYSTEM_INSTRUCTION_EMULATION_INFORMATION = ^SYSTEM_INSTRUCTION_EMULATION_INFORMATION;
  TSystemInstructionEmulationInformation = SYSTEM_INSTRUCTION_EMULATION_INFORMATION;
  PSystemInstructionEmulationInformation = ^TSystemInstructionEmulationInformation;

  _SYSTEM_CACHE_INFORMATION = record // Information Class 21
    SystemCacheWsSize: ULONG;
    SystemCacheWsPeakSize: ULONG;
    SystemCacheWsFaults: ULONG;
    SystemCacheWsMinimum: ULONG;
    SystemCacheWsMaximum: ULONG;
    TransitionSharedPages: ULONG;
    TransitionSharedPagesPeak: ULONG;
    Reserved: array[0..1] of ULONG;
  end;
  SYSTEM_CACHE_INFORMATION = _SYSTEM_CACHE_INFORMATION;
  PSYSTEM_CACHE_INFORMATION = ^SYSTEM_CACHE_INFORMATION;
  TSystemCacheInformation = SYSTEM_CACHE_INFORMATION;
  PSystemCacheInformation = ^TSystemCacheInformation;

  _SYSTEM_POOL_TAG_INFORMATION = record // Information Class 22
    Tag: array[0..3] of AnsiChar;
    PagedPoolAllocs: ULONG;
    PagedPoolFrees: ULONG;
    PagedPoolUsage: ULONG;
    NonPagedPoolAllocs: ULONG;
    NonPagedPoolFrees: ULONG;
    NonPagedPoolUsage: ULONG;
  end;
  SYSTEM_POOL_TAG_INFORMATION = _SYSTEM_POOL_TAG_INFORMATION;
  PSYSTEM_POOL_TAG_INFORMATION = ^SYSTEM_POOL_TAG_INFORMATION;
  TSystemPoolTagInformation = SYSTEM_POOL_TAG_INFORMATION;
  PSystemPoolTagInformation = ^TSystemPoolTagInformation;

  _SYSTEM_PROCESSOR_STATISTICS = record // Information Class 23
    ContextSwitches: ULONG;
    DpcCount: ULONG;
    DpcRequestRate: ULONG;
    TimeIncrement: ULONG;
    DpcBypassCount: ULONG;
    ApcBypassCount: ULONG;
  end;
  SYSTEM_PROCESSOR_STATISTICS = _SYSTEM_PROCESSOR_STATISTICS;
  PSYSTEM_PROCESSOR_STATISTICS = ^SYSTEM_PROCESSOR_STATISTICS;
  TSystemProcessorStatistics = SYSTEM_PROCESSOR_STATISTICS;
  PSystemProcessorStatistics = ^TSystemProcessorStatistics;

  _SYSTEM_DPC_INFORMATION = record // Information Class 24
    Reserved: ULONG;
    MaximumDpcQueueDepth: ULONG;
    MinimumDpcRate: ULONG;
    AdjustDpcThreshold: ULONG;
    IdealDpcRate: ULONG;
  end;
  SYSTEM_DPC_INFORMATION = _SYSTEM_DPC_INFORMATION;
  PSYSTEM_DPC_INFORMATION = ^SYSTEM_DPC_INFORMATION;
  TSystemDpcInformation = SYSTEM_DPC_INFORMATION;
  PSystemDpcInformation = ^TSystemDpcInformation;

  _SYSTEM_LOAD_IMAGE = record // Information Class 26
    ModuleName: UNICODE_STRING;
    ModuleBase: PVOID;
    Unknown: PVOID;
    EntryPoint: PVOID;
    ExportDirectory: PVOID;
  end;
  SYSTEM_LOAD_IMAGE = _SYSTEM_LOAD_IMAGE;
  PSYSTEM_LOAD_IMAGE = ^SYSTEM_LOAD_IMAGE;
  TSystemLoadImage = SYSTEM_LOAD_IMAGE;
  PSystemLoadImage = ^TSystemLoadImage;

  _SYSTEM_UNLOAD_IMAGE = record // Information Class 27
    ModuleBase: PVOID;
  end;
  SYSTEM_UNLOAD_IMAGE = _SYSTEM_UNLOAD_IMAGE;
  PSYSTEM_UNLOAD_IMAGE = ^SYSTEM_UNLOAD_IMAGE;
  TSystemUnloadImage = SYSTEM_UNLOAD_IMAGE;
  PSystemUnloadImage = ^TSystemUnloadImage;

  _SYSTEM_QUERY_TIME_ADJUSTMENT = record // Information Class 28
    TimeAdjustment: ULONG;
    MaximumIncrement: ULONG;
    TimeSynchronization: ByteBool;
  end;
  SYSTEM_QUERY_TIME_ADJUSTMENT = _SYSTEM_QUERY_TIME_ADJUSTMENT;
  PSYSTEM_QUERY_TIME_ADJUSTMENT = ^SYSTEM_QUERY_TIME_ADJUSTMENT;
  TSystemQueryTimeAdjustment = SYSTEM_QUERY_TIME_ADJUSTMENT;
  PSystemQueryTimeAdjustment = ^TSystemQueryTimeAdjustment;

  _SYSTEM_SET_TIME_ADJUSTMENT = record // Information Class 28
    TimeAdjustment: ULONG;
    TimeSynchronization: ByteBool;
  end;
  SYSTEM_SET_TIME_ADJUSTMENT = _SYSTEM_SET_TIME_ADJUSTMENT;
  PSYSTEM_SET_TIME_ADJUSTMENT = ^SYSTEM_SET_TIME_ADJUSTMENT;
  TSystemSetTimeAdjustment = SYSTEM_SET_TIME_ADJUSTMENT;
  PSystemSetTimeAdjustment = ^TSystemSetTimeAdjustment;

  _SYSTEM_CRASH_DUMP_INFORMATION = record // Information Class 32
    CrashDumpSectionHandle: HANDLE;
    Unknown: HANDLE; // Windows 2000 only
  end;
  SYSTEM_CRASH_DUMP_INFORMATION = _SYSTEM_CRASH_DUMP_INFORMATION;
  PSYSTEM_CRASH_DUMP_INFORMATION = ^SYSTEM_CRASH_DUMP_INFORMATION;
  TSystemCrashDumpInformation = SYSTEM_CRASH_DUMP_INFORMATION;
  PSystemCrashDumpInformation = ^TSystemCrashDumpInformation;

  {.$IFNDEF JWA_INCLUDEMODE}
  _SYSTEM_EXCEPTION_INFORMATION = record // Information Class 33
    AlignmentFixupCount: ULONG;
    ExceptionDispatchCount: ULONG;
    FloatingEmulationCount: ULONG;
    Reserved: ULONG;
  end;
  SYSTEM_EXCEPTION_INFORMATION = _SYSTEM_EXCEPTION_INFORMATION;
  PSYSTEM_EXCEPTION_INFORMATION = ^SYSTEM_EXCEPTION_INFORMATION;
  TSystemExceptionInformation = SYSTEM_EXCEPTION_INFORMATION;
  PSystemExceptionInformation = ^TSystemExceptionInformation;
  {.$ENDIF JWA_INCLUDEMODE}

  _SYSTEM_CRASH_STATE_INFORMATION = record // Information Class 34
    ValidCrashDump: ULONG;
    Unknown: ULONG; // Windows 2000 only
  end;
  SYSTEM_CRASH_STATE_INFORMATION = _SYSTEM_CRASH_STATE_INFORMATION;
  PSYSTEM_CRASH_STATE_INFORMATION = ^SYSTEM_CRASH_STATE_INFORMATION;
  TSystemCrashStateInformation = SYSTEM_CRASH_STATE_INFORMATION;
  PSystemCrashStateInformation = ^TSystemCrashStateInformation;

  _SYSTEM_KERNEL_DEBUGGER_INFORMATION = record // Information Class 35
    DebuggerEnabled: ByteBool;
    DebuggerNotPresent: ByteBool;
  end;
  SYSTEM_KERNEL_DEBUGGER_INFORMATION = _SYSTEM_KERNEL_DEBUGGER_INFORMATION;
  PSYSTEM_KERNEL_DEBUGGER_INFORMATION = ^SYSTEM_KERNEL_DEBUGGER_INFORMATION;
  TSystemKernelDebuggerInformation = SYSTEM_KERNEL_DEBUGGER_INFORMATION;
  PSystemKernelDebuggerInformation = ^TSystemKernelDebuggerInformation;

  _SYSTEM_CONTEXT_SWITCH_INFORMATION = record // Information Class 36
    ContextSwitches: ULONG;
    ContextSwitchCounters: array[0..10] of ULONG;
  end;
  SYSTEM_CONTEXT_SWITCH_INFORMATION = _SYSTEM_CONTEXT_SWITCH_INFORMATION;
  PSYSTEM_CONTEXT_SWITCH_INFORMATION = ^SYSTEM_CONTEXT_SWITCH_INFORMATION;
  TSystemContextSwitchInformation = SYSTEM_CONTEXT_SWITCH_INFORMATION;
  PSystemContextSwitchInformation = ^TSystemContextSwitchInformation;

  {.$IFNDEF JWA_INCLUDEMODE}
  _SYSTEM_REGISTRY_QUOTA_INFORMATION = record // Information Class 37
    RegistryQuota: ULONG;
    RegistryQuotaInUse: ULONG;
    PagedPoolSize: ULONG;
  end;
  SYSTEM_REGISTRY_QUOTA_INFORMATION = _SYSTEM_REGISTRY_QUOTA_INFORMATION;
  PSYSTEM_REGISTRY_QUOTA_INFORMATION = ^SYSTEM_REGISTRY_QUOTA_INFORMATION;
  TSystemRegistryQuotaInformation = SYSTEM_REGISTRY_QUOTA_INFORMATION;
  PSystemRegistryQuotaInformation = ^TSystemRegistryQuotaInformation;
  {.$ENDIF JWA_INCLUDEMODE}

  _SYSTEM_LOAD_AND_CALL_IMAGE = record // Information Class 38
    ModuleName: UNICODE_STRING;
  end;
  SYSTEM_LOAD_AND_CALL_IMAGE = _SYSTEM_LOAD_AND_CALL_IMAGE;
  PSYSTEM_LOAD_AND_CALL_IMAGE = ^SYSTEM_LOAD_AND_CALL_IMAGE;
  TSystemLoadAndCallImage = SYSTEM_LOAD_AND_CALL_IMAGE;
  PSystemLoadAndCallImage = ^TSystemLoadAndCallImage;

  _SYSTEM_PRIORITY_SEPARATION = record // Information Class 39
    PrioritySeparation: ULONG;
  end;
  SYSTEM_PRIORITY_SEPARATION = _SYSTEM_PRIORITY_SEPARATION;
  PSYSTEM_PRIORITY_SEPARATION = ^SYSTEM_PRIORITY_SEPARATION;
  TSystemPrioritySeparation = SYSTEM_PRIORITY_SEPARATION;
  PSystemPrioritySeparation = ^TSystemPrioritySeparation;

  _SYSTEM_TIME_ZONE_INFORMATION = record // Information Class 44
    Bias: LONG;
    StandardName: array[0..31] of WCHAR;
    StandardDate: SYSTEMTIME;
    StandardBias: LONG;
    DaylightName: array[0..31] of WCHAR;
    DaylightDate: SYSTEMTIME;
    DaylightBias: LONG;
  end;
  SYSTEM_TIME_ZONE_INFORMATION = _SYSTEM_TIME_ZONE_INFORMATION;
  PSYSTEM_TIME_ZONE_INFORMATION = ^SYSTEM_TIME_ZONE_INFORMATION;
  TSystemTimeZoneInformation = SYSTEM_TIME_ZONE_INFORMATION;
  PSystemTimeZoneInformation = ^TSystemTimeZoneInformation;

  {.$IFNDEF JWA_INCLUDEMODE}
  _SYSTEM_LOOKASIDE_INFORMATION = record // Information Class 45
    Depth: USHORT;
    MaximumDepth: USHORT;
    TotalAllocates: ULONG;
    AllocateMisses: ULONG;
    TotalFrees: ULONG;
    FreeMisses: ULONG;
    Type_: POOL_TYPE;
    Tag: ULONG;
    Size: ULONG;
  end;
  SYSTEM_LOOKASIDE_INFORMATION = _SYSTEM_LOOKASIDE_INFORMATION;
  PSYSTEM_LOOKASIDE_INFORMATION = ^SYSTEM_LOOKASIDE_INFORMATION;
  TSystemLookAsideInformation = SYSTEM_LOOKASIDE_INFORMATION;
  PSystemLookAsideInformation = ^TSystemLookAsideInformation;
  {.$ENDIF JWA_INCLUDEMODE}

  _SYSTEM_SET_TIME_SLIP_EVENT = record // Information Class 46
    TimeSlipEvent: HANDLE;
  end;
  SYSTEM_SET_TIME_SLIP_EVENT = _SYSTEM_SET_TIME_SLIP_EVENT;
  PSYSTEM_SET_TIME_SLIP_EVENT = ^SYSTEM_SET_TIME_SLIP_EVENT;
  TSystemSetTimeSlipEvent = SYSTEM_SET_TIME_SLIP_EVENT;
  PSystemSetTimeSlipEvent = ^TSystemSetTimeSlipEvent;

  _SYSTEM_CREATE_SESSION = record // Information Class 47
    Session: ULONG;
  end;
  SYSTEM_CREATE_SESSION = _SYSTEM_CREATE_SESSION;
  PSYSTEM_CREATE_SESSION = ^SYSTEM_CREATE_SESSION;
  TSystemCreateSession = SYSTEM_CREATE_SESSION;
  PSystemCreateSession = ^TSystemCreateSession;

  _SYSTEM_DELETE_SESSION = record // Information Class 48
    Session: ULONG;
  end;
  SYSTEM_DELETE_SESSION = _SYSTEM_DELETE_SESSION;
  PSYSTEM_DELETE_SESSION = ^SYSTEM_DELETE_SESSION;
  TSystemDeleteSession = SYSTEM_DELETE_SESSION;
  PSystemDeleteSession = ^TSystemDeleteSession;

  _SYSTEM_RANGE_START_INFORMATION = record // Information Class 50
    SystemRangeStart: PVOID;
  end;
  SYSTEM_RANGE_START_INFORMATION = _SYSTEM_RANGE_START_INFORMATION;
  PSYSTEM_RANGE_START_INFORMATION = ^SYSTEM_RANGE_START_INFORMATION;
  TSystemRangeStartInformation = SYSTEM_RANGE_START_INFORMATION;
  PSystemRangeStartInformation = ^TSystemRangeStartInformation;

  _SYSTEM_POOL_BLOCK = record
    Allocated: ByteBool;
    Unknown: USHORT;
    Size: ULONG;
    Tag: array[0..3] of AnsiChar;
  end;
  SYSTEM_POOL_BLOCK = _SYSTEM_POOL_BLOCK;
  PSYSTEM_POOL_BLOCK = ^SYSTEM_POOL_BLOCK;
  TSystemPoolBlock = SYSTEM_POOL_BLOCK;
  PSystemPoolBlock = ^TSystemPoolBlock;

  _SYSTEM_POOL_BLOCKS_INFORMATION = record // Info Classes 14 and 15
    PoolSize: ULONG;
    PoolBase: PVOID;
    Unknown: USHORT;
    NumberOfBlocks: ULONG;
    PoolBlocks: array[0..0] of SYSTEM_POOL_BLOCK;
  end;
  SYSTEM_POOL_BLOCKS_INFORMATION = _SYSTEM_POOL_BLOCKS_INFORMATION;
  PSYSTEM_POOL_BLOCKS_INFORMATION = ^SYSTEM_POOL_BLOCKS_INFORMATION;
  TSystemPoolBlocksInformation = SYSTEM_POOL_BLOCKS_INFORMATION;
  PSystemPoolBlocksInformation = ^TSystemPoolBlocksInformation;

  _SYSTEM_MEMORY_USAGE = record
    Name: PVOID;
    Valid: USHORT;
    Standby: USHORT;
    Modified: USHORT;
    PageTables: USHORT;
  end;
  SYSTEM_MEMORY_USAGE = _SYSTEM_MEMORY_USAGE;
  PSYSTEM_MEMORY_USAGE = ^SYSTEM_MEMORY_USAGE;
  TSystemMemoryUsage = SYSTEM_MEMORY_USAGE;
  PSystemMemoryUsage = ^TSystemMemoryUsage;

  _SYSTEM_MEMORY_USAGE_INFORMATION = record // Info Classes 25 and 29
    Reserved: ULONG;
    EndOfData: PVOID;
    MemoryUsage: array[0..0] of SYSTEM_MEMORY_USAGE;
  end;
  SYSTEM_MEMORY_USAGE_INFORMATION = _SYSTEM_MEMORY_USAGE_INFORMATION;
  PSYSTEM_MEMORY_USAGE_INFORMATION = ^SYSTEM_MEMORY_USAGE_INFORMATION;
  TSystemMemoryUsageInformation = SYSTEM_MEMORY_USAGE_INFORMATION;
  PSystemMemoryUsageInformation = ^TSystemMemoryUsageInformation;

type
  _SHUTDOWN_ACTION = (
    ShutdownNoReboot,
    ShutdownReboot,
    ShutdownPowerOff);
  SHUTDOWN_ACTION = _SHUTDOWN_ACTION;
  TShutdownAction = SHUTDOWN_ACTION;

type
  _DEBUG_CONTROL_CODE = (
    DebugFiller0,
    DebugGetTraceInformation,
    DebugSetInternalBreakpoint,
    DebugSetSpecialCall,
    DebugClearSpecialCalls,
    DebugQuerySpecialCalls,
    DebugDbgBreakPoint);
  DEBUG_CONTROL_CODE = _DEBUG_CONTROL_CODE;
  TDebugControlCode = DEBUG_CONTROL_CODE;

type
  _OBJECT_INFORMATION_CLASS = (
    ObjectBasicInformation,
    ObjectNameInformation,
    ObjectTypeInformation,
    ObjectAllTypesInformation,
    ObjectHandleInformation);
  OBJECT_INFORMATION_CLASS = _OBJECT_INFORMATION_CLASS;
  TObjectInformationClass = OBJECT_INFORMATION_CLASS;

type
  _OBJECT_BASIC_INFORMATION = record // Information Class 0
    Attributes: ULONG;
    GrantedAccess: ACCESS_MASK;
    HandleCount: ULONG;
    PointerCount: ULONG;
    PagedPoolUsage: ULONG;
    NonPagedPoolUsage: ULONG;
    Reserved: array[0..2] of ULONG;
    NameInformationLength: ULONG;
    TypeInformationLength: ULONG;
    SecurityDescriptorLength: ULONG;
    CreateTime: LARGE_INTEGER;
  end;
  OBJECT_BASIC_INFORMATION = _OBJECT_BASIC_INFORMATION;
  POBJECT_BASIC_INFORMATION = ^OBJECT_BASIC_INFORMATION;
  TObjectBasicInformation = OBJECT_BASIC_INFORMATION;
  PObjectBasicInformation = ^TObjectBasicInformation;

  _OBJECT_TYPE_INFORMATION = record // Information Class 2
    Name: UNICODE_STRING;
    ObjectCount: ULONG;
    HandleCount: ULONG;
    Reserved1: array[0..3] of ULONG;
    PeakObjectCount: ULONG;
    PeakHandleCount: ULONG;
    Reserved2: array[0..3] of ULONG;
    InvalidAttributes: ULONG;
    GenericMapping: GENERIC_MAPPING;
    ValidAccess: ULONG;
    Unknown: UCHAR;
    MaintainHandleDatabase: ByteBool;
    Reserved3: array[0..1] of UCHAR;
    PoolType: POOL_TYPE;
    PagedPoolUsage: ULONG;
    NonPagedPoolUsage: ULONG;
  end;
  OBJECT_TYPE_INFORMATION = _OBJECT_TYPE_INFORMATION;
  POBJECT_TYPE_INFORMATION = ^OBJECT_TYPE_INFORMATION;
  TObjectTypeInformation = OBJECT_TYPE_INFORMATION;
  PObjectTypeInformation = ^TObjectTypeInformation;

  _OBJECT_ALL_TYPES_INFORMATION = record // Information Class 3
    NumberOfTypes: ULONG;
    TypeInformation: OBJECT_TYPE_INFORMATION;
  end;
  OBJECT_ALL_TYPES_INFORMATION = _OBJECT_ALL_TYPES_INFORMATION;
  POBJECT_ALL_TYPES_INFORMATION = ^OBJECT_ALL_TYPES_INFORMATION;
  TObjectAllTypesInformation = OBJECT_ALL_TYPES_INFORMATION;
  PObjectAllTypesInformation = ^TObjectAllTypesInformation;

  _OBJECT_HANDLE_ATTRIBUTE_INFORMATION = record // Information Class 4
    Inherit: ByteBool;
    ProtectFromClose: ByteBool;
  end;
  OBJECT_HANDLE_ATTRIBUTE_INFORMATION = _OBJECT_HANDLE_ATTRIBUTE_INFORMATION;
  POBJECT_HANDLE_ATTRIBUTE_INFORMATION = ^OBJECT_HANDLE_ATTRIBUTE_INFORMATION;
  TObjectHandleAttributeInformation = OBJECT_HANDLE_ATTRIBUTE_INFORMATION;
  PObjectHandleAttributeInformation = ^TObjectHandleAttributeInformation;

type
  _DIRECTORY_BASIC_INFORMATION = record
    ObjectName: UNICODE_STRING;
    ObjectTypeName: UNICODE_STRING;
  end;
  DIRECTORY_BASIC_INFORMATION = _DIRECTORY_BASIC_INFORMATION;
  PDIRECTORY_BASIC_INFORMATION = ^DIRECTORY_BASIC_INFORMATION;
  TDirectoryBasicInformation = DIRECTORY_BASIC_INFORMATION;
  PDirectoryBasicInformation = ^TDirectoryBasicInformation;

type
  _MEMORY_INFORMATION_CLASS = (
    MemoryBasicInformation,
    MemoryWorkingSetList,
    MemorySectionName,
    MemoryBasicVlmInformation);
  MEMORY_INFORMATION_CLASS = _MEMORY_INFORMATION_CLASS;
  TMemoryInformationClass = MEMORY_INFORMATION_CLASS;
  PMemoryInformationClass = ^TMemoryInformationClass;

type
  {$IFNDEF JWA_INCLUDEMODE}
  _MEMORY_BASIC_INFORMATION = record // Information Class 0
    BaseAddress: PVOID;
    AllocationBase: PVOID;
    AllocationProtect: ULONG;
    RegionSize: ULONG;
    State: ULONG;
    Protect: ULONG;
    Type_: ULONG;
  end;
  MEMORY_BASIC_INFORMATION = _MEMORY_BASIC_INFORMATION;
  PMEMORY_BASIC_INFORMATION = ^MEMORY_BASIC_INFORMATION;
  TMemoryBasicInformation = MEMORY_BASIC_INFORMATION;
  PMemoryBasicInformation = ^TMemoryBasicInformation;
  {$ENDIF JWA_INCLUDEMODE}

  _MEMORY_WORKING_SET_LIST = record // Information Class 1
    NumberOfPages: ULONG;
    WorkingSetList: array[0..0] of ULONG;
  end;
  MEMORY_WORKING_SET_LIST = _MEMORY_WORKING_SET_LIST;
  PMEMORY_WORKING_SET_LIST = ^MEMORY_WORKING_SET_LIST;
  TMemoryWorkingSetList = MEMORY_WORKING_SET_LIST;
  PMemoryWorkingSetList = ^TMemoryWorkingSetList;

  _MEMORY_SECTION_NAME = record // Information Class 2
    SectionFileName: UNICODE_STRING;
  end;
  MEMORY_SECTION_NAME = _MEMORY_SECTION_NAME;
  PMEMORY_SECTION_NAME = ^MEMORY_SECTION_NAME;
  TMemorySectionName = MEMORY_SECTION_NAME;
  PMemorySectionName = ^TMemorySectionName;

type
  _SECTION_INFORMATION_CLASS = (
    SectionBasicInformation,
    SectionImageInformation);
  SECTION_INFORMATION_CLASS = _SECTION_INFORMATION_CLASS;
  TSectionInformationClass = SECTION_INFORMATION_CLASS;

type
  _SECTION_BASIC_INFORMATION = record // Information Class 0
    BaseAddress: PVOID;
    Attributes: ULONG;
    Size: LARGE_INTEGER;
  end;
  SECTION_BASIC_INFORMATION = _SECTION_BASIC_INFORMATION;
  PSECTION_BASIC_INFORMATION = ^SECTION_BASIC_INFORMATION;
  TSectionBasicInformation = SECTION_BASIC_INFORMATION;
  PSectionBasicInformation = ^TSectionBasicInformation;

  _SECTION_IMAGE_INFORMATION = record // Information Class 1
    EntryPoint: PVOID;
    Unknown1: ULONG;
    StackReserve: ULONG;
    StackCommit: ULONG;
    Subsystem: ULONG;
    MinorSubsystemVersion: USHORT;
    MajorSubsystemVersion: USHORT;
    Unknown2: ULONG;
    Characteristics: ULONG;
    ImageNumber: USHORT;
    Executable: ByteBool;
    Unknown3: UCHAR;
    Unknown4: array[0..2] of ULONG;
  end;
  SECTION_IMAGE_INFORMATION = _SECTION_IMAGE_INFORMATION;
  PSECTION_IMAGE_INFORMATION = ^SECTION_IMAGE_INFORMATION;
  TSectionImageInformation = SECTION_IMAGE_INFORMATION;
  PSectionImageInformation = TSectionImageInformation;

type
  _USER_STACK = record
    FixedStackBase: PVOID;
    FixedStackLimit: PVOID;
    ExpandableStackBase: PVOID;
    ExpandableStackLimit: PVOID;
    ExpandableStackBottom: PVOID;
  end;
  USER_STACK = _USER_STACK;
  PUSER_STACK = ^USER_STACK;
  TUserStack = USER_STACK;
  PUserStack = ^TUserStack;

type
  _THREAD_BASIC_INFORMATION = record // Information Class 0
    ExitStatus: NTSTATUS;
    TebBaseAddress: PNT_TIB;
    ClientId: CLIENT_ID;
    AffinityMask: KAFFINITY;
    Priority: KPRIORITY;
    BasePriority: KPRIORITY;
  end;
  THREAD_BASIC_INFORMATION = _THREAD_BASIC_INFORMATION;
  PTHREAD_BASIC_INFORMATION = ^THREAD_BASIC_INFORMATION;
  TThreadBasicInformation = THREAD_BASIC_INFORMATION;
  PThreadBasicInformation = ^TThreadBasicInformation;

type
  _PROCESS_PRIORITY_CLASS = record // Information Class 18
    Foreground: ByteBool;
    PriorityClass: UCHAR;
  end;
  PROCESS_PRIORITY_CLASS = _PROCESS_PRIORITY_CLASS;
  PPROCESS_PRIORITY_CLASS = ^PROCESS_PRIORITY_CLASS;
  TProcessPriorityClass = PROCESS_PRIORITY_CLASS;
  PProcessPriorityClass = ^TProcessPriorityClass;

  _RTL_PROCESS_INFORMATION = record
    Size: ULONG;
    hProcess: HANDLE;
    hThread: HANDLE;
    ClientId: CLIENT_ID;
    ImageInfo: SECTION_IMAGE_INFORMATION;
  end;
  RTL_PROCESS_INFORMATION = _RTL_PROCESS_INFORMATION;
  PRTL_PROCESS_INFORMATION = ^RTL_PROCESS_INFORMATION;
  TRtlProcessInformation = RTL_PROCESS_INFORMATION;
  PRtlProcessInformation = ^RTL_PROCESS_INFORMATION;

type
  _DEBUG_BUFFER = record
    SectionHandle: HANDLE;
    SectionBase: PVOID;
    RemoteSectionBase: PVOID;
    SectionBaseDelta: ULONG;
    EventPairHandle: HANDLE;
    Unknown: array[0..1] of ULONG;
    RemoteThreadHandle: HANDLE;
    InfoClassMask: ULONG;
    SizeOfInfo: ULONG;
    AllocatedSize: ULONG;
    SectionSize: ULONG;
    ModuleInformation: PVOID;
    BackTraceInformation: PVOID;
    HeapInformation: PVOID;
    LockInformation: PVOID;
    Reserved: array[0..7] of PVOID;
  end;
  DEBUG_BUFFER = _DEBUG_BUFFER;
  PDEBUG_BUFFER = ^DEBUG_BUFFER;
  TDebugBuffer = DEBUG_BUFFER;
  PDebugBuffer = ^TDebugBuffer;

const
  PDI_MODULES = $01;
  PDI_BACKTRACE = $02;
  PDI_HEAPS = $04;
  PDI_HEAP_TAGS = $08;
  PDI_HEAP_BLOCKS = $10;
  PDI_LOCKS = $20;

type
  _DEBUG_MODULE_INFORMATION = record // c.f. SYSTEM_MODULE_INFORMATION
    Reserved: array[0..1] of ULONG;
    Base: ULONG;
    Size: ULONG;
    Flags: ULONG;
    Index: USHORT;
    Unknown: USHORT;
    LoadCount: USHORT;
    ModuleNameOffset: USHORT;
    ImageName: array[0..255] of AnsiChar;
  end;
  DEBUG_MODULE_INFORMATION = _DEBUG_MODULE_INFORMATION;
  PDEBUG_MODULE_INFORMATION = ^DEBUG_MODULE_INFORMATION;
  TDebugModuleInformation = DEBUG_MODULE_INFORMATION;
  PDebugModuleInformation = ^TDebugModuleInformation;

  _DEBUG_HEAP_INFORMATION = record
    Base: ULONG;
    Flags: ULONG;
    Granularity: USHORT;
    Unknown: USHORT;
    Allocated: ULONG;
    Committed: ULONG;
    TagCount: ULONG;
    BlockCount: ULONG;
    Reserved: array[0..6] of ULONG;
    Tags: PVOID;
    Blocks: PVOID;
  end;
  DEBUG_HEAP_INFORMATION = _DEBUG_HEAP_INFORMATION;
  PDEBUG_HEAP_INFORMATION = ^DEBUG_HEAP_INFORMATION;
  TDebugHeapInformation = DEBUG_HEAP_INFORMATION;
  PDebugHeapInformation = ^TDebugHeapInformation;

  _DEBUG_LOCK_INFORMATION = record // c.f. SYSTEM_LOCK_INFORMATION
    Address: PVOID;
    Type_: USHORT;
    CreatorBackTraceIndex: USHORT;
    OwnerThreadId: ULONG;
    ActiveCount: ULONG;
    ContentionCount: ULONG;
    EntryCount: ULONG;
    RecursionCount: ULONG;
    NumberOfSharedWaiters: ULONG;
    NumberOfExclusiveWaiters: ULONG;
  end;
  DEBUG_LOCK_INFORMATION = _DEBUG_LOCK_INFORMATION;
  PDEBUG_LOCK_INFORMATION = ^DEBUG_LOCK_INFORMATION;
  TDebugLockInformation = DEBUG_LOCK_INFORMATION;
  PDebugLockInformation = ^TDebugLockInformation;

type
  PTIMER_APC_ROUTINE = procedure(TimerContext: PVOID; TimerLowValue: ULONG; TimerHighValue: LONG); stdcall;

type
  _TIMER_INFORMATION_CLASS = (TimerBasicInformation);
  TIMER_INFORMATION_CLASS = _TIMER_INFORMATION_CLASS;
  TTimerInformationClass = TIMER_INFORMATION_CLASS;

type
  _TIMER_BASIC_INFORMATION = record
    TimeRemaining: LARGE_INTEGER;
    SignalState: ByteBool;
  end;
  TIMER_BASIC_INFORMATION = _TIMER_BASIC_INFORMATION;
  PTIMER_BASIC_INFORMATION = ^TIMER_BASIC_INFORMATION;
  TTimerBasicInformation = TIMER_BASIC_INFORMATION;
  PTimerBasicInformation = ^TTimerBasicInformation;

type
  _EVENT_INFORMATION_CLASS = (EventBasicInformation);
  EVENT_INFORMATION_CLASS = _EVENT_INFORMATION_CLASS;
  TEventInformationClass = EVENT_INFORMATION_CLASS;

type
  