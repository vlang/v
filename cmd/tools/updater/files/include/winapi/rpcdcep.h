/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the w64 mingw-runtime package.
 * No warranty is given; refer to the file DISCLAIMER within this package.
 */
#ifndef __RPCDCEP_H__
#define __RPCDCEP_H__

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct _RPC_VERSION {
    unsigned short MajorVersion;
    unsigned short MinorVersion;
  } RPC_VERSION;

  typedef struct _RPC_SYNTAX_IDENTIFIER {
    GUID SyntaxGUID;
    RPC_VERSION SyntaxVersion;
  } RPC_SYNTAX_IDENTIFIER,*PRPC_SYNTAX_IDENTIFIER;

  typedef struct _RPC_MESSAGE {
    RPC_BINDING_HANDLE Handle;
    unsigned long DataRepresentation;
    void *Buffer;
    unsigned int BufferLength;
    unsigned int ProcNum;
    PRPC_SYNTAX_IDENTIFIER TransferSyntax;
    void *RpcInterfaceInformation;
    void *ReservedForRuntime;
    RPC_MGR_EPV *ManagerEpv;
    void *ImportContext;
    unsigned long RpcFlags;
  } RPC_MESSAGE,*PRPC_MESSAGE;

  typedef RPC_STATUS RPC_ENTRY RPC_FORWARD_FUNCTION(UUID *InterfaceId,RPC_VERSION *InterfaceVersion,UUID *ObjectId,unsigned char *Rpcpro,void **ppDestEndpoint);

  enum RPC_ADDRESS_CHANGE_TYPE {
    PROTOCOL_NOT_LOADED = 1,PROTOCOL_LOADED,PROTOCOL_ADDRESS_CHANGE
  };

  typedef void RPC_ENTRY RPC_ADDRESS_CHANGE_FN(void *arg);

#define RPC_CONTEXT_HANDLE_DEFAULT_GUARD ((void *) -4083)

#define RPC_CONTEXT_HANDLE_DEFAULT_FLAGS 0x00000000UL
#define RPC_CONTEXT_HANDLE_FLAGS 0x30000000UL
#define RPC_CONTEXT_HANDLE_SERIALIZE 0x10000000UL
#define RPC_CONTEXT_HANDLE_DONT_SERIALIZE 0x20000000UL

#define RPC_NCA_FLAGS_DEFAULT 0x00000000
#define RPC_NCA_FLAGS_IDEMPOTENT 0x00000001
#define RPC_NCA_FLAGS_BROADCAST 0x00000002
#define RPC_NCA_FLAGS_MAYBE 0x00000004

#define RPC_BUFFER_COMPLETE 0x00001000
#define RPC_BUFFER_PARTIAL 0x00002000
#define RPC_BUFFER_EXTRA 0x00004000
#define RPC_BUFFER_ASYNC 0x00008000
#define RPC_BUFFER_NONOTIFY 0x00010000

#define RPCFLG_MESSAGE 0x01000000UL
#define RPCFLG_AUTO_COMPLETE 0x08000000UL
#define RPCFLG_LOCAL_CALL 0x10000000UL
#define RPCFLG_INPUT_SYNCHRONOUS 0x20000000UL
#define RPCFLG_ASYNCHRONOUS 0x40000000UL
#define RPCFLG_NON_NDR 0x80000000UL

#define RPCFLG_HAS_MULTI_SYNTAXES 0x02000000UL
#define RPCFLG_HAS_CALLBACK 0x04000000UL

#define RPC_FLAGS_VALID_BIT 0x00008000

  typedef void (__RPC_STUB *RPC_DISPATCH_FUNCTION)(PRPC_MESSAGE Message);

  typedef struct {
    unsigned int DispatchTableCount;
    RPC_DISPATCH_FUNCTION *DispatchTable;
    LONG_PTR Reserved;
  } RPC_DISPATCH_TABLE,*PRPC_DISPATCH_TABLE;

  typedef struct _RPC_PROTSEQ_ENDPOINT {
    unsigned char *RpcProtocolSequence;
    unsigned char *Endpoint;
  } RPC_PROTSEQ_ENDPOINT,*PRPC_PROTSEQ_ENDPOINT;

#define NT351_INTERFACE_SIZE 0x40
#define RPC_INTERFACE_HAS_PIPES 0x0001

  typedef struct _RPC_SERVER_INTERFACE {
    unsigned int Length;
    RPC_SYNTAX_IDENTIFIER InterfaceId;
    RPC_SYNTAX_IDENTIFIER TransferSyntax;
    PRPC_DISPATCH_TABLE DispatchTable;
    unsigned int RpcProtseqEndpointCount;
    PRPC_PROTSEQ_ENDPOINT RpcProtseqEndpoint;
    RPC_MGR_EPV *DefaultManagerEpv;
    void const *InterpreterInfo;
    unsigned int Flags;
  } RPC_SERVER_INTERFACE,*PRPC_SERVER_INTERFACE;

  typedef struct _RPC_CLIENT_INTERFACE {
    unsigned int Length;
    RPC_SYNTAX_IDENTIFIER InterfaceId;
    RPC_SYNTAX_IDENTIFIER TransferSyntax;
    PRPC_DISPATCH_TABLE DispatchTable;
    unsigned int RpcProtseqEndpointCount;
    PRPC_PROTSEQ_ENDPOINT RpcProtseqEndpoint;
    ULONG_PTR Reserved;
    void const *InterpreterInfo;
    unsigned int Flags;
  } RPC_CLIENT_INTERFACE,*PRPC_CLIENT_INTERFACE;

  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcNegotiateTransferSyntax(RPC_MESSAGE *Message);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcGetBuffer(RPC_MESSAGE *Message);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcGetBufferWithObject(RPC_MESSAGE *Message,UUID *ObjectUuid);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcSendReceive(RPC_MESSAGE *Message);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcFreeBuffer(RPC_MESSAGE *Message);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcSend(PRPC_MESSAGE Message);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcReceive(PRPC_MESSAGE Message,unsigned int Size);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcFreePipeBuffer(RPC_MESSAGE *Message);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcReallocPipeBuffer(PRPC_MESSAGE Message,unsigned int NewSize);

  typedef void *I_RPC_MUTEX;

#ifdef UNICODE
#define I_RpcNsBindingSetEntryName I_RpcNsBindingSetEntryNameW
#define I_RpcServerUseProtseqEp2 I_RpcServerUseProtseqEp2W
#define I_RpcServerUseProtseq2 I_RpcServerUseProtseq2W
#define I_RpcBindingInqDynamicEndpoint I_RpcBindingInqDynamicEndpointW
#else
#define I_RpcNsBindingSetEntryName I_RpcNsBindingSetEntryNameA
#define I_RpcServerUseProtseqEp2 I_RpcServerUseProtseqEp2A
#define I_RpcServerUseProtseq2 I_RpcServerUseProtseq2A
#define I_RpcBindingInqDynamicEndpoint I_RpcBindingInqDynamicEndpointA
#endif

  RPCRTAPI void RPC_ENTRY I_RpcRequestMutex(I_RPC_MUTEX *Mutex);
  RPCRTAPI void RPC_ENTRY I_RpcClearMutex(I_RPC_MUTEX Mutex);
  RPCRTAPI void RPC_ENTRY I_RpcDeleteMutex(I_RPC_MUTEX Mutex);
  RPCRTAPI void *RPC_ENTRY I_RpcAllocate(unsigned int Size);
  RPCRTAPI void RPC_ENTRY I_RpcFree(void *Object);
  RPCRTAPI void RPC_ENTRY I_RpcPauseExecution(unsigned long Milliseconds);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcGetExtendedError();

  typedef void (__RPC_API *PRPC_RUNDOWN)(void *AssociationContext);

  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcMonitorAssociation(RPC_BINDING_HANDLE Handle,PRPC_RUNDOWN RundownRoutine,void *Context);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcStopMonitorAssociation(RPC_BINDING_HANDLE Handle);
  RPCRTAPI RPC_BINDING_HANDLE RPC_ENTRY I_RpcGetCurrentCallHandle(void);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcGetAssociationContext(RPC_BINDING_HANDLE BindingHandle,void **AssociationContext);
  RPCRTAPI void *RPC_ENTRY I_RpcGetServerContextList(RPC_BINDING_HANDLE BindingHandle);
  RPCRTAPI void RPC_ENTRY I_RpcSetServerContextList(RPC_BINDING_HANDLE BindingHandle,void *ServerContextList);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcNsInterfaceExported(unsigned long EntryNameSyntax,unsigned short *EntryName,RPC_SERVER_INTERFACE *RpcInterfaceInformation);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcNsInterfaceUnexported(unsigned long EntryNameSyntax,unsigned short *EntryName,RPC_SERVER_INTERFACE *RpcInterfaceInformation);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingToStaticStringBindingW(RPC_BINDING_HANDLE Binding,unsigned short **StringBinding);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingInqSecurityContext(RPC_BINDING_HANDLE Binding,void **SecurityContextHandle);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingInqWireIdForSnego(RPC_BINDING_HANDLE Binding,RPC_CSTR WireId);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingInqMarshalledTargetInfo (RPC_BINDING_HANDLE Binding,unsigned long *MarshalledTargetInfoLength,RPC_CSTR *MarshalledTargetInfo);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingInqLocalClientPID(RPC_BINDING_HANDLE Binding,unsigned long *Pid);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingHandleToAsyncHandle(RPC_BINDING_HANDLE Binding,void **AsyncHandle);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcNsBindingSetEntryNameW(RPC_BINDING_HANDLE Binding,unsigned long EntryNameSyntax,RPC_WSTR EntryName);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcNsBindingSetEntryNameA(RPC_BINDING_HANDLE Binding,unsigned long EntryNameSyntax,RPC_CSTR EntryName);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcServerUseProtseqEp2A(RPC_CSTR NetworkAddress,RPC_CSTR Protseq,unsigned int MaxCalls,RPC_CSTR Endpoint,void *SecurityDescriptor,void *Policy);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcServerUseProtseqEp2W(RPC_WSTR NetworkAddress,RPC_WSTR Protseq,unsigned int MaxCalls,RPC_WSTR Endpoint,void *SecurityDescriptor,void *Policy);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcServerUseProtseq2W(RPC_WSTR NetworkAddress,RPC_WSTR Protseq,unsigned int MaxCalls,void *SecurityDescriptor,void *Policy);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcServerUseProtseq2A(RPC_CSTR NetworkAddress,RPC_CSTR Protseq,unsigned int MaxCalls,void *SecurityDescriptor,void *Policy);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingInqDynamicEndpointW(RPC_BINDING_HANDLE Binding,RPC_WSTR *DynamicEndpoint);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingInqDynamicEndpointA(RPC_BINDING_HANDLE Binding,RPC_CSTR *DynamicEndpoint);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcServerCheckClientRestriction(RPC_BINDING_HANDLE Context);

#define TRANSPORT_TYPE_CN 0x01
#define TRANSPORT_TYPE_DG 0x02
#define TRANSPORT_TYPE_LPC 0x04
#define TRANSPORT_TYPE_WMSG 0x08

  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingInqTransportType(RPC_BINDING_HANDLE Binding,unsigned int *Type);

  typedef struct _RPC_TRANSFER_SYNTAX {
    UUID Uuid;
    unsigned short VersMajor;
    unsigned short VersMinor;
  } RPC_TRANSFER_SYNTAX;

  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcIfInqTransferSyntaxes(RPC_IF_HANDLE RpcIfHandle,RPC_TRANSFER_SYNTAX *TransferSyntaxes,unsigned int TransferSyntaxSize,unsigned int *TransferSyntaxCount);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_UuidCreate(UUID *Uuid);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingCopy(RPC_BINDING_HANDLE SourceBinding,RPC_BINDING_HANDLE *DestinationBinding);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingIsClientLocal(RPC_BINDING_HANDLE BindingHandle,unsigned int *ClientLocalFlag);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingInqConnId(RPC_BINDING_HANDLE Binding,void **ConnId,int *pfFirstCall);
  RPCRTAPI void RPC_ENTRY I_RpcSsDontSerializeContext(void);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcLaunchDatagramReceiveThread(void *pAddress);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcServerRegisterForwardFunction(RPC_FORWARD_FUNCTION *pForwardFunction);
  RPC_ADDRESS_CHANGE_FN *RPC_ENTRY I_RpcServerInqAddressChangeFn();
  RPC_STATUS RPC_ENTRY I_RpcServerSetAddressChangeFn(RPC_ADDRESS_CHANGE_FN *pAddressChangeFn);

#define RPC_P_ADDR_FORMAT_TCP_IPV4 1
#define RPC_P_ADDR_FORMAT_TCP_IPV6 2

  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcServerInqLocalConnAddress(RPC_BINDING_HANDLE Binding,void *Buffer,unsigned long *BufferSize,unsigned long *AddressFormat);
  RPCRTAPI void RPC_ENTRY I_RpcSessionStrictContextHandle();
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcTurnOnEEInfoPropagation(void);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcConnectionInqSockBuffSize(unsigned long *RecvBuffSize,unsigned long *SendBuffSize);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcConnectionSetSockBuffSize(unsigned long RecvBuffSize,unsigned long SendBuffSize);

  typedef void (*RPCLT_PDU_FILTER_FUNC)(void *Buffer,unsigned int BufferLength,int fDatagram);
  typedef void (__cdecl *RPC_SETFILTER_FUNC)(RPCLT_PDU_FILTER_FUNC pfnFilter);

#ifndef WINNT
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcServerStartListening(void *hWnd);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcServerStopListening();

  typedef RPC_STATUS (*RPC_BLOCKING_FN)(void *hWnd,void *Context,void *hSyncEvent);

#ifdef UNICODE
#define I_RpcServerUnregisterEndpoint I_RpcServerUnregisterEndpointW
#else
#define I_RpcServerUnregisterEndpoint I_RpcServerUnregisterEndpointA
#endif

  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcBindingSetAsync(RPC_BINDING_HANDLE Binding,RPC_BLOCKING_FN BlockingFn,unsigned long ServerTid);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcSetThreadParams(int fClientFree,void *Context,void *hWndClient);
  RPCRTAPI unsigned int RPC_ENTRY I_RpcWindowProc(void *hWnd,unsigned int Message,unsigned int wParam,unsigned long lParam);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcServerUnregisterEndpointA(RPC_CSTR Protseq,RPC_CSTR Endpoint);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcServerUnregisterEndpointW(RPC_WSTR Protseq,RPC_WSTR Endpoint);
#endif

  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcServerInqTransportType(unsigned int *Type);
  RPCRTAPI long RPC_ENTRY I_RpcMapWin32Status(RPC_STATUS Status);

  typedef struct _RPC_C_OPT_METADATA_DESCRIPTOR {
    unsigned long BufferSize;
    char *Buffer;
  } RPC_C_OPT_METADATA_DESCRIPTOR;

  typedef struct _RDR_CALLOUT_STATE {
    RPC_STATUS LastError;
    void *LastEEInfo;
    RPC_HTTP_REDIRECTOR_STAGE LastCalledStage;
    unsigned short *ServerName;
    unsigned short *ServerPort;
    unsigned short *RemoteUser;
    unsigned short *AuthType;
    unsigned char ResourceTypePresent;
    unsigned char MetadataPresent;
    unsigned char SessionIdPresent;
    unsigned char InterfacePresent;
    UUID ResourceType;
    RPC_C_OPT_METADATA_DESCRIPTOR Metadata;
    UUID SessionId;
    RPC_SYNTAX_IDENTIFIER Interface;
    void *CertContext;
  } RDR_CALLOUT_STATE;

  typedef RPC_STATUS (RPC_ENTRY *I_RpcProxyIsValidMachineFn)(char *pszMachine,char *pszDotMachine,unsigned long dwPortNumber);
  typedef RPC_STATUS (RPC_ENTRY *I_RpcProxyGetClientAddressFn)(void *Context,char *Buffer,unsigned long *BufferLength);
  typedef RPC_STATUS (RPC_ENTRY *I_RpcProxyGetConnectionTimeoutFn)(unsigned long *ConnectionTimeout);
  typedef RPC_STATUS (RPC_ENTRY *I_RpcPerformCalloutFn)(void *Context,RDR_CALLOUT_STATE *CallOutState,RPC_HTTP_REDIRECTOR_STAGE Stage);
  typedef void (RPC_ENTRY *I_RpcFreeCalloutStateFn)(RDR_CALLOUT_STATE *CallOutState);

  typedef struct tagI_RpcProxyCallbackInterface {
    I_RpcProxyIsValidMachineFn IsValidMachineFn;
    I_RpcProxyGetClientAddressFn GetClientAddressFn;
    I_RpcProxyGetConnectionTimeoutFn GetConnectionTimeoutFn;
    I_RpcPerformCalloutFn PerformCalloutFn;
    I_RpcFreeCalloutStateFn FreeCalloutStateFn;
  } I_RpcProxyCallbackInterface;

#define RPC_PROXY_CONNECTION_TYPE_IN_PROXY 0
#define RPC_PROXY_CONNECTION_TYPE_OUT_PROXY 1

  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcProxyNewConnection(unsigned long ConnectionType,unsigned short *ServerAddress,unsigned short *ServerPort,unsigned short *MinConnTimeout,void *ConnectionParameter,RDR_CALLOUT_STATE *CallOutState,I_RpcProxyCallbackInterface *ProxyCallbackInterface);
  RPCRTAPI RPC_STATUS RPC_ENTRY I_RpcReplyToClientWithStatus(void *ConnectionParameter,RPC_STATUS RpcStatus);
  RPCRTAPI void RPC_ENTRY I_RpcRecordCalloutFailure(RPC_STATUS RpcStatus,RDR_CALLOUT_STATE *CallOutState,unsigned short *DllName);

#ifdef __cplusplus
}
#endif
#endif
